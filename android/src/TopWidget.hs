{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}

module TopWidget
  (topWidget
  , headWidget)
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

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.Text as T

import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.Element
import qualified Language.Javascript.JSaddle.Types as X

headWidget :: MonadWidget t m => m ()
headWidget = do
  elAttr "link"
    (("rel" =: "stylesheet")
      <> ("href" =: "https://bulma.io/css/bulma-docs.min.css?v=201806022344"))
    $ return ()
  elAttr "link"
    (("rel" =: "stylesheet")
      <> ("href" =: "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"))
    $ return ()

  elAttr "meta" (("name" =: "viewport")
    <> ("content" =: "width=device-width, initial-scale=1.0"))
    $ return ()

topWidget :: MonadWidget t m => m ()
topWidget = do
  -- urlPath <- getLocationPath
  -- urlHost <- getLocationHost
  -- proto <- getLocationProtocol
  -- let
  --   url = ws <> host <> path
  --   path = (fst $ T.breakOn "static" urlPath) <> "websocket"
  --   host = if T.isPrefixOf "localhost" urlHost
  --     then "localhost:3000"
  --     else urlHost


  --   ws = case proto of
  --     "http:" -> "ws://"
  --     _ -> "wss://"
  let url = "ws://192.168.0.31:3000/websocket"
  (_,wsConn) <- withWSConnection
    url
    never -- close event
    True -- reconnect
    widget

#if defined (DEBUG)
  let resp = traceEvent ("Response") (_webSocket_recv wsConn)
  d <- holdDyn "" resp
  dynText ((tshow . BS.length) <$> d)
#endif
  return ()

widget :: AppMonad t m => AppMonadT t m ()
widget = divClass "container" $ do
  -- navigation with visibility control
  tabDisplayUI wrapper "navbar-start" "navbar-item" "navbar-item" $
    Map.fromList [
#if !defined (ONLY_READER) && !defined (ONLY_SRS)
        (2, ("Sentence", sentenceWidget))
      , (3, ("Vocab", vocabSearchWidget))
      , (4, ("Kanji", kanjiBrowseWidget))
      , (5, ("Import", importWidgetTop))
      ,
#endif
#if !defined (ONLY_SRS)
        (0, ("Reader", textReaderTop))
#endif
#if !defined (ONLY_READER) && !defined (ONLY_SRS)
      ,
#endif
#if !defined (ONLY_READER)
        (1, ("SRS", srsWidget))
#endif
      ]

wrapper m = elClass "nav" "navbar" $ do
  clickEv <- divClass "navbar-brand" $ do
    elAttr "a" (("class" =: "navbar-item")) $ text "てんじん"
    (e,_) <- elAttr' "a" (("class" =: "navbar-burger")
                <> ("aria-label" =: "menu")
                <> ("role" =: "button")
                <> ("aria-expanded" =: "false")) $ do
      elAttr "span" (("aria-hidden" =: "true")) $ return ()
      elAttr "span" (("aria-hidden" =: "true")) $ return ()
      elAttr "span" (("aria-hidden" =: "true")) $ return ()
    return $ domEvent Click e

  openDyn <- toggle False clickEv
  let cl = ffor openDyn (\b -> "navbar-menu " <> if b
                          then "is-active" else "")
  elDynClass "div" cl $ do
    a <- m
    divClass "navbar-end" $ do
      divClass "navbar-item" $ do
        elAttr "a" (("class" =: "navbar-item")
          <> ("href" =: "https://tenjinreader.com/auth/logout"))
          $ text "Logout"
        btn "" "Theme"
          >>= toggleTheme
    return a

-- readable_bootstrap_css = $(embedFile "src/readable_bootstrap.min.css")
-- custom_css = $(embedFile "src/custom.css")
-- slate_bootstrap_css = $(embedFile "src/slate_bootstrap.min.css")

readable_bootstrap_css :: ByteString
readable_bootstrap_css = ""

custom_css :: ByteString
custom_css = ""

slate_bootstrap_css :: ByteString
slate_bootstrap_css = ""

toggleTheme :: AppMonad t m
  => Event t ()
  -> AppMonadT t m ()
toggleTheme ev = do
  rec
    d <- holdDyn False (not <$> (tag (current d) ev))

  let
    toggleW b = X.liftJSM $ do
      let
        css :: ByteString
        css = custom_css <> if b
            then slate_bootstrap_css
            else readable_bootstrap_css
      doc <- currentDocumentUnchecked
      headElement <- getHeadUnchecked doc
      setInnerHTML headElement $
        "<style>" <> T.unpack (decodeUtf8 css)
          <> "</style>" --TODO: Fix this

  void $ widgetHold (return ())
    (toggleW <$> updated d)

