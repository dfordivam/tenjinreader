{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Handler.WebSocketHandler where

import Import
import Yesod.WebSockets

import Message
import Reflex.Dom.WebSocket.Message
import Reflex.Dom.WebSocket.Server hiding (Handler)
import qualified Reflex.Dom.WebSocket.Server (Handler)
import qualified Data.Map as Map
import qualified Data.Conduit.List

getWebSocketHandlerR :: Handler Html
getWebSocketHandlerR = do
    (_, user) <- requireAuthPair
    userSessionData <- newMVar (Map.empty)
    webSockets $ handleWebSocketConn user userSessionData
    redirect $ ("static/websocket/index.html" :: Text)

type WsHandlerM = ReaderT (MVar (Map Text Text)) (WebSocketsT Handler)

handleWebSocketConn user userSessionData =
  let runF bs = runReaderT (handleRequest wsHandler bs) (userSessionData)
  in sourceWS $$ ((Data.Conduit.List.mapM runF) =$= sinkWSBinary)

wsHandler :: HandlerWrapper WsHandlerM Message.AppRequest
wsHandler = HandlerWrapper $
  h getStoreData
  :<&> h getRetrieveData

  where
  h :: (WebSocketMessage Message.AppRequest a, Monad m)
    => (a -> m (ResponseT Message.AppRequest a))
    -> Reflex.Dom.WebSocket.Server.Handler m Message.AppRequest a
  h = makeHandler

getStoreData :: StoreData -> WsHandlerM StoreDataResp
getStoreData (StoreData k v) = do
  keyMapRef <- ask
  keyMap <- liftIO $ takeMVar keyMapRef
  let newMap = Map.insert k v keyMap
  liftIO $ putMVar keyMapRef newMap
  return $ StoreDataResp True

getRetrieveData :: RetrieveData -> WsHandlerM RetrieveDataResp
getRetrieveData (RetrieveData k) = do
  keyMapRef <- ask
  keyMap <- liftIO $ readMVar keyMapRef
  return $ RetrieveDataResp (Map.lookup k keyMap)
