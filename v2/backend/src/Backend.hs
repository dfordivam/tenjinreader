{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Backend where

import Control.Monad.IO.Class
import Data.Text
import qualified Data.Text as T
import Data.Functor.Identity
import Data.Dependent.Sum (DSum (..))
import Data.Functor.Sum
import Servant
import Snap

import Common.Route
import Common.Api
import Obelisk.Backend
import Obelisk.Route

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      -- webSocketChatState <- newMVar WebSocketChat.newServerState
      serve $ \case
        BackendRoute_Api :=> Identity () -> do
          liftIO $ putStrLn "testing"
          (serveSnap readerAPI apiServer)
  , _backend_routeEncoder = backendRouteEncoder
  }

type ApiHandler = Snap

apiServer :: Server (ReaderAPI) '[] ApiHandler
apiServer = a1 :<|> a2
  where
    a1 :: Int -> ApiHandler Text
    a1 = (\bId -> return $ "Book " <> (T.pack $ show bId))
    a2 :: Int -> ApiHandler ()
    a2 = const $ return ()
