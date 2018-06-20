{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
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
import LoginWidget

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

headWidget :: MonadWidget t m => Dynamic t Bool -> m ()
headWidget isDark = do
  let
    -- f True = "https://jenil.github.io/bulmaswatch/darkly/bulmaswatch.min.css"
    -- f _ = "https://jenil.github.io/bulmaswatch/flatly/bulmaswatch.min.css"
     -- "https://jenil.github.io/bulmaswatch/spacelab/bulmaswatch.min.css"
    f True = "/css/darkly.css"
    f _ = "/css/flatly.css"
    attrDyn = ffor isDark $ \b -> (("rel" =: "stylesheet")
      <> ("href" =: (f b)))
  elDynAttr "link" attrDyn
    $ return ()
  elAttr "link"
    (("rel" =: "stylesheet")
      -- <> ("href" =: "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"))
      <> ("href" =: "/css/font-awesome.min.css"))
    $ return ()

  elAttr "script"
    (("language" =: "javascript")
      <> ("src" =: "/js/wanakana.js"))
    $ return ()

  elAttr "meta" (("name" =: "viewport")
    <> ("content" =: "width=device-width, initial-scale=1.0"))
    $ return ()

topWidget :: MonadWidget t m => m (Dynamic t Bool)
topWidget = mdo
  dEv <- widgetHold loginWidget $ ffor (switch (current $ fst <$> dEv))
    (\case
        Nothing -> loginWidget
        (Just s) -> afterLoginWidget s)
  let e = switch (current $ snd <$> dEv)
  toggle False e

afterLoginWidget :: MonadWidget t m
  => Text
  -> m (Event t (Maybe Text), Event t ())
afterLoginWidget secret = do
  let url = "ws://192.168.2.60:3000/websocket/app/" <> secret
  ((ev,d),wsConn) <- withWSConnection
    url
    never -- close event
    True -- reconnect
    widget

  ev2 <- performEvent $ ffor ev $ \_ -> logout
    >> return (Nothing)
  return (ev2,d)

widget :: AppMonad t m
  => AppMonadT t m (Event t (), Event t ())
widget = divClass "" $
  -- navigation with visibility control
  tabDisplayUI wrapper "navbar-start" "navbar-item" "navbar-item" $
    Map.fromList [
        (0, ("Reader", textReaderTop))
      , (1, ("SRS", srsWidget))
      , (2, ("Sentence", sentenceWidget))
      , (3, ("Vocab", vocabSearchWidget))
      , (4, ("Kanji", kanjiBrowseWidget))
      ]

wrapper m = elClass "nav" "navbar" $ do
  clickEv <- divClass "navbar-brand" $ do
    elAttr "a" (("class" =: "navbar-item has-text-grey-lighter")) $ text "てんじん"
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
    b <- divClass "navbar-end" $ do
      divClass "navbar-item" $ do
        ev <- btn "" "Logout"
        ev2 <- btn "" "Theme"
        return (ev, ev2)
    return (a, b)
