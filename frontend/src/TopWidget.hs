{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RankNTypes #-}

module TopWidget
  (topWidget
  , readable_bootstrap_css
  , custom_css)
  where

import FrontendCommon
import SrsWidget
import KanjiBrowser
import TextReader
import ImportWidget

-- from package common
import Common
import Message

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Set as Set
import Control.Lens.Indexed
import Reflex.Dom.Location

import Data.FileEmbed
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.Text as T

import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.Element
import qualified Language.Javascript.JSaddle.Types as X

topWidget :: MonadWidget t m => m ()
topWidget = do
  urlPath <- getLocationPath
  urlHost <- getLocationHost
  proto <- getLocationProtocol
  let
    url = ws <> host <> path
    path = (fst $ T.breakOn "static" urlPath) <> "websocket"
    host = if T.isPrefixOf "localhost" urlHost
      then "localhost:3000"
      else urlHost


    ws = case proto of
      "http:" -> "ws://"
      _ -> "wss://"
  (_,ws) <- withWSConnection
    url
    never -- close event
    True -- reconnect
    widget
  let resp = traceEvent ("Response") (_webSocket_recv ws)
  d <- holdDyn "" resp
  dynText ((tshow . BS.length) <$> d)
  return ()

widget :: AppMonad t m => AppMonadT t m ()
widget = divClass "container" $ do
  -- navigation with visibility control
  tabDisplayUI wrapper "nav navbar-nav" "active" "" $
    Map.fromList
      [(0, ("SRS", srsWidget))
      ,(1, ("Kanji", kanjiBrowseWidget))
      , (2, ("Vocab", vocabSearchWidget))
      , (3, ("Reader", textReaderTop))
      , (4, ("Sentence", quickAnalyzeTop))
      , (5, ("Import", importWidgetTop))
      ]

wrapper m = elClass "nav" "navbar navbar-default" $
  divClass "container-fluid" $ do
    divClass "navbar-header" $
      elClass "a" "navbar-brand" $ text "てんじん Reader"
    a <- m
    elClass "ul" "nav navbar-nav navbar-right" $ do
      ev <- btn "btn-default" "Toggle"
      toggleTheme ev
    return a

readable_bootstrap_css = $(embedFile "src/readable_bootstrap.min.css")
custom_css = $(embedFile "src/custom.css")
slate_bootstrap_css = $(embedFile "src/slate_bootstrap.min.css")

toggleTheme :: AppMonad t m
  => Event t ()
  -> AppMonadT t m ()
toggleTheme ev = do
  rec
    d <- holdDyn False (not <$> (tag (current d) ev))

  let
    toggle b = X.liftJSM $ do
      let css = custom_css <> if b
            then slate_bootstrap_css
            else readable_bootstrap_css
      doc <- currentDocumentUnchecked
      headElement <- getHeadUnchecked doc
      setInnerHTML headElement $
        "<style>" <> T.unpack (decodeUtf8 css)
          <> "</style>" --TODO: Fix this

  void $ widgetHold (return ())
    (toggle <$> updated d)

