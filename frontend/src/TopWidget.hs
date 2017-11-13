{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}

module TopWidget
  (topWidget)
  where

import Reflex.Dom

import Reflex.Dom.WebSocket.Monad
import Reflex.Dom.WebSocket.Message
import qualified Data.Text as T

import Message -- from package common

type AppMonadT t m = WithWebSocketT AppRequest t m
type AppMonad t m = (MonadWidget t m)


topWidget :: MonadWidget t m => m ()
topWidget = do
  let url = "ws://localhost:3000/websocket"
  withWSConnection
    url
    never -- close event
    True -- reconnect
    widget
  return ()

widget :: AppMonad t m => AppMonadT t m ()
widget = do
  el "div" $ do
    text "Send some data to server"
    tiKey <- textInput def
    tiVal <- textInput def
    sendEv <- button "Send"
    let reqEv = attachPromptlyDynWith StoreData
          (value tiKey) $ tagPromptlyDyn (value tiVal) sendEv
    respEv <- getWebSocketResponse reqEv
    widgetHold (text "Waiting for StoreData response")
      $ text <$> ((T.pack . show) <$> respEv)

  el "div" $ do
    text "Retrieve some data from server"
    tiKey <- textInput def
    sendEv <- button "Retrieve"
    let reqEv = RetrieveData <$> (tagPromptlyDyn (value tiKey) sendEv)
    respEv <- getWebSocketResponse reqEv
    widgetHold (text "Waiting for RetrieveData response")
      $ text <$> ((T.pack . show) <$> respEv)

  return ()
