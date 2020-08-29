{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}

module Frontend.Modal.Base where

import Control.Applicative (liftA2)
import Control.Lens (Rewrapped, Wrapped (Unwrapped, _Wrapped'), iso)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Primitive (PrimMonad (PrimState, primitive))
import Control.Monad.Reader (MonadReader)
import Control.Monad.Ref (MonadAtomicRef, MonadRef)
import Control.Monad.Trans (MonadTrans (lift))
import Data.Coerce (coerce)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Semigroup (First (..))
import Data.Text (Text)
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.EventM as EventM
import qualified GHCJS.DOM.GlobalEventHandlers as Events
import Language.Javascript.JSaddle (MonadJSM)
import Obelisk.Route.Frontend
import Reflex.Dom.Core
import Reflex.Host.Class (MonadReflexCreateTrigger)

import Frontend.Modal.Class (HasModal (ModalM, tellModal))

instance (Reflex t, Monad m) => HasModal t (ModalT t m) where
  type ModalM (ModalT t m) = ModalT t m
  tellModal = ModalT . tellEvent . fmap First

newtype ModalT t m a
  = ModalT { unModalT :: EventWriterT t (First (Event t () -> ModalT t m (Event t ()))) m a }
  deriving
    ( Functor, Applicative, Monad
    , MonadFix, MonadIO, MonadRef, MonadAtomicRef, MonadReader r
    , DomBuilder t, NotReady t, MonadHold t, MonadSample t
    , PerformEvent t, TriggerEvent t, PostBuild t, HasJS x
    , MonadReflexCreateTrigger t, MonadQuery t q
    )

instance PrimMonad m => PrimMonad (ModalT t m) where
  type PrimState (ModalT t m) = PrimState m
  primitive = lift . primitive

instance Wrapped (ModalT t m a) where
  type Unwrapped (ModalT t m a) = EventWriterT t (First (Event t () -> ModalT t m (Event t ()))) m a
  _Wrapped' = iso coerce coerce
instance ModalT t m a ~ x => Rewrapped (ModalT t m a) x

instance HasDocument m => HasDocument (ModalT t m)
instance HasJSContext m => HasJSContext (ModalT t m) where
  type JSContextPhantom (ModalT t m) = JSContextPhantom m
  askJSContext = ModalT askJSContext
#if !defined(ghcjs_HOST_OS)
instance MonadJSM m => MonadJSM (ModalT t m)
#endif

instance (Monad m, Routed t r m) => Routed t r (ModalT t m) where
  askRoute = lift askRoute

instance (Monad m, RouteToUrl r m) => RouteToUrl r (ModalT t m) where
  askRouteToUrl = lift askRouteToUrl

instance (Reflex t, Monad m, SetRoute t r m) => SetRoute t r (ModalT t m) where
  modifyRoute = lift . modifyRoute

instance EventWriter t w m => EventWriter t w (ModalT t m) where
  tellEvent = lift . tellEvent

instance Requester t m => Requester t (ModalT t m) where
  type Request (ModalT t m) = Request m
  type Response (ModalT t m) = Response m
  requesting = lift . requesting
  requesting_ = lift . requesting_

instance MonadTrans (ModalT t) where
  lift = ModalT . lift

instance (Adjustable t m, MonadHold t m, MonadFix m) => Adjustable t (ModalT t m) where
  runWithReplace a0 a' = ModalT $ runWithReplace (unModalT a0) (fmapCheap unModalT a')
  traverseDMapWithKeyWithAdjust f dm0 dm' = ModalT $ traverseDMapWithKeyWithAdjust (coerce f) dm0 dm'
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = ModalT $ traverseDMapWithKeyWithAdjustWithMove (coerce f) dm0 dm'
  traverseIntMapWithKeyWithAdjust f im0 im' = ModalT $ traverseIntMapWithKeyWithAdjust (coerce f) im0 im'

deriving instance DomRenderHook t m => DomRenderHook t (ModalT t m)

-- TODO: Fix prerender
instance (Prerender js t m, Monad m, Reflex t) => Prerender js t (ModalT t m) where
  type Client (ModalT t m) = ModalT t (Client m)
  prerender back front = do
    res :: Dynamic t (a, (Maybe (Event t (First (Event t () -> ModalT t (Client m) (Event t ())))))) <- lift $ prerender
      (fmap (\(a, _) -> (a, Nothing)) $ runEventWriterT $ unModalT back)
      (fmap (\(a, ev) -> (a, Just ev)) $ runEventWriterT $ unModalT front)
    -- ModalT $ prerender blank (dyn_ $ ffor res $ (mapM_ tellEvent) . snd)
    pure $ fst <$> res

-- | Like 'withModals' but with the full convenience of 'ModalT', allowing 'tellModal' to open a modal anywhere.
--
-- NB: This must wrap all other DOM building. This is because DOM for the modal
-- must occur *after* all other DOM in order for the modal to appear on top of it.
runModalT
  :: forall m js t a.
   ( MonadFix m
   , DomBuilder t m, MonadHold t m, PostBuild t m, Prerender js t m
   )
  => ModalT t m a -> m a
runModalT f = do
  let
    run f = runEventWriterT (unModalT f)
    openModal open = do
      (_, open') <- modalDom run (getFirst <$> open)
      widgetHold_ blank (ffor open' $ \v -> do
                           pb <- getPostBuild
                           openModal (v <$ pb))
  (a, open) <- run f
  openModal open
  pure a

-- | Set up DOM to support modals.
--
-- NB: This must wrap all other DOM building. This is because DOM for the modal
-- must occur *after* all other DOM in order for the modal to appear on top of it.
withModals
  :: forall m a b js t.
   ( MonadFix m
   , DomBuilder t m, MonadHold t m, PostBuild t m, Prerender js t m
   )
  => Event t (Event t () -> m (Event t a))
  -- ^ Event to trigger a modal to open.
  -- The event carries a function that takes close events and builds a modal window
  -- which returns a close event.
  -> m b -- ^ Page body
  -> m (b, Event t a) -- ^ Result of page body and an event firing whenever a modal closes
withModals open body = liftA2 (,) body (fst <$> (modalDom ((, never) <$>) open))

-- | Builds modal-related DOM. Avoid using this and use 'withModals' instead.
--
-- NB: This must run after all other DOM building. This is because DOM for the modal
-- must occur *after* all other DOM in order for the modal to appear on top of it.
modalDom
  :: forall a c m m2 js t. (DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m, Prerender js t m)
  => (forall b. m2 b -> m (b, Event t c))
  -> Event t (Event t () -> m2 (Event t a))
  -- ^ Event to trigger a modal to open.
  -- The event carries a function that takes close events and builds a modal window
  -- which returns a close event.
  -> m (Event t a, Event t c) -- ^ An event firing whenever the modal closes
modalDom renderF open = mdo
  escPressed :: Event t () <- fmap switchDyn $ prerender (pure never) $ do
    document <- DOM.currentDocumentUnchecked
    wrapDomEventMaybe document (`EventM.on` Events.keyDown) $ do
      key <- getKeyEvent
      pure $ if keyCodeLookup (fromIntegral key) == Escape then Just () else Nothing
  let modalAttr = ffor isVisible $ \b -> "style" =: "z-index: 500; position: fixed;" <> "class" =: ("modal " <> (if b then "is-active" else ""))
  (isVisible, close2, innerEv2) <- elDynAttr "div" modalAttr $ mdo
    isVisible <- holdDyn False $ leftmost [True <$ open, False <$ close]
    (backdropEl, _) <- elClass' "div" "modal-background" blank
    let close = switchDyn (fst <$> res)
        innerEv = switchDyn (snd <$> res)
    res <- elAttr "div" ("class" =: "modal-content" <> "style" =: "max-height: 100vh") $
      widgetHold (pure (never, never)) $ leftmost
        [ (\m2 -> renderF $ m2 $ leftmost [escPressed, domEvent Click backdropEl]) <$> open
        , pure (never, never) <$ close
        ]
    pure (isVisible, close, innerEv)
  pure (close2, innerEv2)
