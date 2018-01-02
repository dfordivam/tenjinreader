{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}

module FrontendCommon
  ( module FrontendCommon
  , module X
  )
  where

import Message as X
import Common as X
import Radicals as X

import Protolude as X hiding (link, (&), list, Alt, to)
import Control.Lens as X ((.~), (^.), (?~), (^?), (%~), _1, _2, _3, sets
  , _head, _Just, view, over, views, preview, (^..), to, mapped, forMOf_)
import Control.Monad.Fix as X

import Reflex.Dom as X
import Reflex.Dom.WebSocket.Monad as X
import Reflex.Dom.WebSocket.Message as X
import Reflex.Time as X (delay)
import Data.Time.Clock as X
import Data.Time.Calendar as X
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.These as X
import Data.Align as X

import Language.Javascript.JSaddle as X (call, eval)
import GHCJS.DOM.Types as X
       (liftJSM, askJSM, runJSM, JSM, MonadJSM)

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)

--
type AppMonadT t m = WithWebSocketT AppRequest t m
type AppMonad t m = (MonadWidget t m, MonadJSM m)

handleVisibility
  :: (DomBuilder t m, PostBuild t m, Eq a)
  => a -> Dynamic t a -> m v -> m v
handleVisibility v dv mv = elDynAttr "div" (f <$> dv) mv
  where
    f dv =
      if v == dv
        then Map.empty
        else ("style" =: "display: none;")

tshow :: (Show a) => a -> Text
tshow = (T.pack . show)

displayVocabT :: DomBuilder t m => Vocab -> m ()
displayVocabT (Vocab ks) = do
  let
    f (Kana k) = text k
    f (KanjiWithReading (Kanji k) r)
      = el "ruby" $ do
          text k
          el "rt" $ text r
  mapM_ f ks

btn cl t = do
  (e,_) <- elClass' "button" ("btn " <> cl) $ text t
  return $ domEvent Click e

-- Controls to add/edit related srs items
addEditSrsEntryWidget :: AppMonad t m
  => (Either KanjiId VocabId)
  -> Maybe Text -- Surface
  -> Maybe SrsEntryId
  -> AppMonadT t m ()
addEditSrsEntryWidget i t s = do
  let
    widget s = case s of
      (Just sId) -> do
        ev <- btn "btn-xs" "Edit SRS"
        openEditSrsItemWidget (sId <$ ev)
        return never

      (Nothing) -> do
        ev <- btn "btn-xs" "Add to SRS"
        resp <- getWebSocketResponse $ QuickAddSrsItem i t <$ ev
        showWSProcessing ev resp
        return resp
  rec
    sDyn <- holdDyn s resp
    resp <- switchPromptly never
      =<< (dyn $ widget <$> sDyn)

  return ()

openEditSrsItemWidget
  :: (AppMonad t m)
  => Event t (SrsEntryId)
  -> AppMonadT t m (Event t (SrsEntryId, SrsEntry))
openEditSrsItemWidget ev = do
  srsItEv <- getWebSocketResponse $ GetSrsItem <$> ev
  showWSProcessing ev srsItEv

  let
      modalWidget :: (AppMonad t m)
        => Maybe (SrsEntryId, SrsEntry)
        -> AppMonadT t m (Event t (SrsEntryId, SrsEntry))
      modalWidget (Just s) = do
        rec
          d <- widgetHold (editWidget s)
            ((return (never,never)) <$ switchPromptlyDyn (fst <$> d))
        return (switchPromptlyDyn $ snd <$> d)

      modalWidget Nothing = do
        return never

      editWidget :: AppMonad t m
        => (SrsEntryId, SrsEntry)
        -> AppMonadT t m (Event t (), Event t (SrsEntryId, SrsEntry))
      editWidget (sId, s) = do
        rec
          (sNew, saveEv, closeEv) <- editWidgetView s ev
          let sEv = tagDyn sNew saveEv
          ev <- getWebSocketResponse $ EditSrsItem sId <$> sEv
        return (closeEv, (,) sId <$> sEv)

  switchPromptlyDyn
    <$> widgetHold (return never) (modalWidget <$> srsItEv)

modalDiv m = do
  divClass "modal-backdrop fade in" $ return ()
  elAttr "div" attr $ divClass "modal-dialog"
    $ divClass "modal-content" m
  where attr = ("class" =: "modal")
          <> ("style" =: "display: block;")

editWidgetView
  :: MonadWidget t m
  => SrsEntry
  -> Event t ()
  -> m (Dynamic t SrsEntry
       , Event t (), Event t ())
editWidgetView s savedEv = modalDiv $ do
  closeEvTop <- divClass "modal-header" $ el "h3" $ do
    (e,_) <- elClass' "button" "close" $ text "Close"
    text $ "Edit " <> (s ^. field . to (NE.head))
    return (domEvent Click e)

  let bodyAttr = ("class" =: "modal-body")
          <> ("style" =: "height: 400px;\
              \overflow-y: auto")
      formAttr = ("class" =: "form-horizontal")
        <> ("onsubmit" =: "return false;")
      bodyForm m = elAttr "div" bodyAttr $
        elAttr "form" formAttr m

  ret <- bodyForm $ do

    f <- divClass "form-group" $ do
      elClass "label" "control-label col-sm-1" $
        text "Field:"
      editNonEmptyList (s ^. field) identity
        $ \t x -> do
             text t
             e <- x
             text ", "
             return e

    r <- divClass "form-group" $ do
      elClass "label" "control-label col-sm-2" $
        text "Reading:"
      editNonEmptyList (s ^. readings) Reading
        $ \t x -> do
             text $ unReading t
             e <- x
             text ", "
             return e

    -- m <- elAttr "div" (("class" =: "form-group")
    --       <> ("style" =: "height: 100px;\
    --           \overflow-y: scroll")) $ do
    m <- divClass "form-group" $ do
      elClass "label" "control-label col-sm-2" $
        text "Meanings:"
      editNonEmptyList (s ^. meaning) Meaning
        $ \t x -> do
             text $ unMeaning t
             e <- x
             text ", "
             return e

    rn <- divClass "form-group" $ do
      elClass "label" "control-label col-sm-2" $
        text "Reading Notes:"
      divClass "" $
        textArea $ def &
          textAreaConfig_initialValue .~
            (maybe "" unReadingNotes
             $ s ^. readingNotes)

    mn <- divClass "form-group" $ do
      elClass "label" "control-label col-sm-2" $
        text "Meaning Notes:"
      divClass "" $
        textArea $ def &
          textAreaConfig_initialValue .~
            (maybe "" unMeaningNotes
             $ s ^. meaningNotes)

    let
        g c v = gg c <$> value v
        gg c t
          | T.null t = Nothing
          | otherwise = Just $ c t

    return $ SrsEntry (_reviewState s)
              <$> r <*> m
              <*> (g ReadingNotes rn)
              <*> (g MeaningNotes mn)
              <*> f

  divClass "modal-footer" $ do
    let savedIcon = elClass "i" "" $ return ()
    saveEv <- btn "btn-primary" "Save"
    closeEv <- btn "btn-default" "Close"
    showWSProcessing saveEv savedEv
    widgetHold (return ()) (savedIcon <$ savedEv)
    return (ret, saveEv, leftmost[closeEv, closeEvTop])


editNonEmptyList :: (_)
  => NonEmpty v
  -> (Text -> v)
  -> (forall a . v -> m a -> m a)
  -> m (Dynamic t (NonEmpty v))
editNonEmptyList ne conT renderFun = do
  let
    rem = do
      -- (e,_) <- elClass' "span" "glyphicon glyphicon-remove" $ return ()
      (e,_) <- el' "a" $ text "(X)"
      return $ domEvent Click e

    initMap = Map.fromList $ zip [1..] (NE.toList ne)
    showItem k t = do
      ev <- renderFun t rem
      return (t, (k,Nothing) <$ ev)

  rec
    let
      remAddEv = Map.fromList . NE.toList <$> mergeList [addEv, remEv]
      addEv = attachDyn newKeyDyn (tagDyn (Just . conT <$> value ti) enterPress)
      remEv1 = switchPromptlyDyn $
        (leftmost . (fmap snd) . Map.elems) <$> d
      remEv = fmapMaybe g (attachDyn d remEv1)
      g (m,e) = if Map.size m > 1
        then Just e
        else Nothing
      newKeyDyn = ((+ 1) . fst . Map.findMax) <$> d

      enterPress = ffilter (==13) (ti ^. textInput_keypress) -- 13 -> Enter
      tiAttr = constDyn $ ("style" =: "width: 100%;")

    ti <- divClass "col-sm-2" $ textInput $ def
      & textInputConfig_attributes .~ tiAttr
      & textInputConfig_setValue .~ ("" <$ enterPress)
    d <- divClass "col-sm-6" $
      elClass "p" "form-control-static" $
        listHoldWithKey initMap remAddEv showItem

  return $ (NE.fromList . (fmap fst) . Map.elems) <$> d

showWSProcessing :: (_)
  => Event t a
  -> Event t b
  -> m ()
showWSProcessing evStart evFinish = do
  let
    showSpinner = divClass "spinner" $ do
      divClass "bounce1" $ return ()
      divClass "bounce2" $ return ()
      divClass "bounce3" $ return ()

  let waitForStart = do
        void $ widgetHold (showSpinner)
          (return () <$ evFinish)
  void $ widgetHold (return ())
    (waitForStart <$ evStart)
