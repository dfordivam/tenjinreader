{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Handler.WebSocketHandler
  (getWebSocketHandlerR)
  where

import Import
import Yesod.WebSockets
import Handler.WebSocketHandler.Utils

import Handler.WebSocketHandler.KanjiBrowser
import Handler.WebSocketHandler.SrsReview

import Message
import Reflex.Dom.WebSocket.Message
import Reflex.Dom.WebSocket.Server hiding (Handler)
import qualified Reflex.Dom.WebSocket.Server (Handler)
import qualified Data.Map as Map
import qualified Data.Conduit.List
import Database.Persist.Sql

getWebSocketHandlerR :: Handler Html
getWebSocketHandlerR = do
    -- uId <- requireAuthId
    let uId = toSqlKey 1 :: Key User
    webSockets $ handleWebSocketConn uId
    redirect $ ("static/websocket/index.html" :: Text)

handleWebSocketConn uId = do
  iref1  <- liftIO $ newIORef ([],0)
  iref2  <- liftIO $ newIORef ([],0)
  iref3  <- liftIO $ newIORef ([],0)
  let runF bs = do
        liftIO $ putStrLn $ decodeUtf8 bs
        lift $ runReaderT
          (handleRequest wsHandler bs) (userSessionData)
      userSessionData =
        WsHandlerEnv iref1 iref2 iref3 (fromSqlKey uId)

  sourceWS $$ ((Data.Conduit.List.mapM runF)
                  =$= sinkWSBinary)

wsHandler :: HandlerWrapper WsHandlerM Message.AppRequest
wsHandler = HandlerWrapper $

  h getKanjiFilterResult
  :<&> h getLoadMoreKanjiResults

  :<&> h getKanjiDetails
  :<&> h getLoadMoreKanjiVocab

  :<&> h getVocabSearch
  :<&> h getLoadMoreVocabSearchResult

  :<&> h getQuickAddSrsItem

  :<&> h getSrsStats

  :<&> h getGetNextReviewItem
  :<&> h getDoReview
  :<&> h getCheckAnswer

  :<&> h getBrowseSrsItems
  :<&> h getSrsItem
  :<&> h getEditSrsItem
  :<&> h getBulkEditSrsItems

  :<&> h getAddOrEditDocument
  :<&> h getListDocuments
  :<&> h getViewDocument
  :<&> h getDeleteDocument

  :<&> h getVocabDetails

  where
  h :: (WebSocketMessage Message.AppRequest a, Monad m)
    => (a -> m (ResponseT Message.AppRequest a))
    -> Reflex.Dom.WebSocket.Server.Handler m Message.AppRequest a
  h = makeHandler
