{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Frontend.Modal.Class where

import Control.Monad.Reader (MonadReader (ask), ReaderT (..))
import Control.Monad.Trans (MonadTrans (lift))
import Reflex (Event, Reflex, EventWriterT, DynamicWriterT)
import Obelisk.Route.Frontend (RoutedT, askRoute, runRoutedT)

-- | The class of monads supporting a 'tellModal' operation which will open a modal
--   that stays on top of all other content.
class HasModal t m where
  -- | Opens a modal when the given event fires. The event carries a function which:
  --     * takes a "close" event triggered when the user signifies that they want to close the modal,
  --     * builds content in 'ModalM m',
  --     * returns a "close" event which will be used to actually close the modal.
  --
  --   For example, a modal may choose not to be closable by simply ignoring it's input and returning 'never'.
  tellModal :: Event t (Event t () -> m (Event t ())) -> m ()

instance (Monad m, Reflex t, HasModal t m) => HasModal t (ReaderT r m) where
  tellModal ev = do
    r <- ask
    lift $ tellModal $ (fmap . fmap) (`runReaderT` r) ev

instance (Monad m, Reflex t, HasModal t m) => HasModal t (RoutedT t r m) where
  tellModal ev = do
    r <- askRoute
    lift $ tellModal $ (fmap . fmap) (`runRoutedT` r) ev
