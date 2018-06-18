{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Handler.WebSocketHandler
  (getWebSocketHandlerR
  , getAppWebSocketHandlerR)
  where

import Import
import Yesod.WebSockets
import Handler.WebSocketHandler.Utils
import SrsDB

import Handler.WebSocketHandler.KanjiBrowser
import Handler.WebSocketHandler.SrsReview

import qualified Data.Map as Map

import qualified Data.Text as T

import Message
import Reflex.Dom.WebSocket.Message
import Reflex.Dom.WebSocket.Server hiding (Handler)
import qualified Reflex.Dom.WebSocket.Server (Handler)
import qualified Data.Conduit.List
import Database.Persist.Sql

getWebSocketHandlerR :: Handler Html
getWebSocketHandlerR = do
    uId <- requireAuthId
    -- let uId = toSqlKey 1 :: Key User
    webSockets $ handleWebSocketConn uId
    redirect $ ("static/app/complete/index.html" :: Text)

getAppWebSocketHandlerR :: Text -> Handler Value
getAppWebSocketHandlerR secret = do
    liftIO $ putStrLn secret
    muser <- runDB $ selectFirst [UserSecretKey ==. Just secret] []
    addHeader "Access-Control-Allow-Origin" "*"
    case muser of
      Just (Entity uId _) -> do
        webSockets $ handleWebSocketConn uId
        returnJson True
      Nothing -> do
        returnJson False

handleWebSocketConn :: UserId -> WebSocketsT Handler ()
handleWebSocketConn uId = do
  iref1  <- liftIO $ newIORef ([],0)
  iref2  <- liftIO $ newIORef ([],0)
  iref3  <- liftIO $ newIORef ([],0)

  dbLocksMVar <- lift $ asks appUserDbLocks
  db <- modifyMVar dbLocksMVar $ \m -> case Map.lookup uId m of
    Nothing -> do
      (db,hnds) <- liftIO $ openUserDB ("userData/" ++ (show $ fromSqlKey uId))
      return (Map.insert uId ((db,hnds), 1) m, db)
    (Just ((db,hnds),c)) -> do
      return (Map.insert uId ((db,hnds), c + 1) m, db)

  let runF bs = do
        lift $ runReaderT
          (handleRequest wsHandler showF bs) (userSessionData)
      userSessionData =
        WsHandlerEnv iref1 iref2 iref3 (fromSqlKey uId) db

      showF :: (Show a, Show b) => a -> b -> WsHandlerM ()
      showF a b = do
        $(logInfo) ("User " <> (T.pack $ show $ fromSqlKey uId)
                    <> ("\nRequest: " <> (T.pack $ take 60 $ show a)))
                    -- <> ("\nResponse: " <> (T.pack $ show b)))

  sourceWS $$ ((Data.Conduit.List.mapM runF)
                  =$= sinkWSBinary)

  modifyMVar_ dbLocksMVar $ \m -> case Map.lookup uId m of
    Nothing -> do
      $(logError) "DB Handle missing from Map"
      return m
    (Just ((db2,hnds),c)) -> case c of
      1 -> do
        liftIO $ closeUserDB hnds
        return (Map.delete uId m)
      c2 ->
        return (Map.insert uId ((db2,hnds), c2 - 1) m)


wsHandler :: HandlerWrapper WsHandlerM Message.AppRequest
wsHandler = HandlerWrapper $

  h getKanjiFilterResult
  :<&> h getLoadMoreKanjiResults

  :<&> h getKanjiDetails
  :<&> h getLoadMoreKanjiVocab

  :<&> h getVocabSearch
  :<&> h getLoadMoreVocabSearchResult

  :<&> h getQuickAddSrsItem
  :<&> h getQuickToggleWakaru

  :<&> h getSrsStats

  :<&> h getSyncReviewItems
  :<&> h getCheckAnswer

  :<&> h getBrowseSrsItems
  :<&> h getSrsItem
  :<&> h getEditSrsItem
  :<&> h getBulkEditSrsItems

  :<&> h getAddOrEditDocument
  :<&> h getListDocuments
  :<&> h getListBooks
  :<&> h getListArticles
  :<&> h getViewDocument
  :<&> h getViewRawDocument
  :<&> h getDeleteDocument

  :<&> h getQuickAnalyzeText

  :<&> h getReaderSettings
  :<&> h saveReaderSettings
  :<&> h saveReadingProgress

  :<&> h getVocabDetails
  :<&> h getVocabSentences
  :<&> h getRandomSentence
  :<&> h getLoadMoreSentences
  :<&> h getToggleSentenceFav

  :<&> h getImportSearchFields
  :<&> h getImportData

  where
  h :: (WebSocketMessage Message.AppRequest a, Monad m)
    => (a -> m (ResponseT Message.AppRequest a))
    -> Reflex.Dom.WebSocket.Server.Handler m Message.AppRequest a
  h = makeHandler
