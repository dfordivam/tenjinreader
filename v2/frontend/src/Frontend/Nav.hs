{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Frontend.Nav where

import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom.Core

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Fix
import Data.Bool
import Data.Dependent.Sum (DSum(..))
import Data.Functor.Identity
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable

import Common.Api
import Common.Route
import Common.Types
import Frontend.Common
import Obelisk.Generated.Static

nav
  :: forall t m js .( DomBuilder t m
     , Routed t (R FrontendRoute) m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , Prerender js t m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     )
  => Event t ()
  -> m (Dynamic t Bool, Dynamic t ReaderControls)
nav _ = do
  elClass "header" "" $ do
    topBar never

topBar
  :: forall t m js .( DomBuilder t m
     , Routed t (R FrontendRoute) m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , Prerender js t m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     )
  => Event t ()
  -> m (Dynamic t Bool, Dynamic t ReaderControls)
topBar inpEv = do
  let
    attr1 = ("aria-label" =: "main navigation") <>
            ("class" =: "navbar") <> ("role" =: "navigation")
  elAttr "nav" attr1 $ mdo
    showPanel <- divClass "navbar-brand" $ do
      let
        attr2 = ffor showPanel $ \s -> ("class" =: "navbar-item") <>
                (if s
                  then ("style" =: "width: 8em;")
                  else Map.empty)
        attr3 = ffor showPanel $ \s -> ("height" =: "30") <>
                ("src" =: "https://tenjinreader.com/static/logo.png") <>
                ("width" =: "60") <>
          (if not s
            then ("style" =: "display: none;")
            else Map.empty)
      elDynAttr "a" attr2 $ mdo
        (e,_) <- elDynAttr' "img" attr3 $ return ()
        setRoute ((FrontendRoute_Home :/ ()) <$ domEvent Click e)
        toggle True =<< burgerButton
    let
      attr8 = ("class" =: "navbar-menu") <>
              ("id" =: "navbarBasicExample")
    (rc, ie) <- elAttr "div" attr8 $ do
      divClass "navbar-start" $ return ()
      divClass "navbar-end" $ do
        rc <- readerControls
        divClass "navbar-item" $ do
          inputElement $ def
            & initialAttributes .~
            ("class" =: "input" <> "style" =: "width:30vw; max-width: 15em;")
        return (rc, ie)
    return (showPanel, rc)

burgerButton
  :: ( DomBuilder t m
     )
  => m (Event t ())
burgerButton = do
  let
    attr4 = ("aria-expanded" =: "false") <> ("aria-label" =: "menu") <>
            ("class" =: "navbar-burger burger") <>
            ("data-target" =: "navbarBasicExample") <> ("role" =: "button") <>
            ("style" =: "display: block;")
  (e, _) <- elAttr' "a" attr4 $ do
    let attr5 = ("aria-hidden" =: "true")
    replicateM_ 3 $ elAttr "span" attr5 $ return ()
  return $ (() <$ domEvent Click e)

sidePanel
  :: forall t m .( DomBuilder t m
     , Routed t (R FrontendRoute) m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     )
  => Dynamic t Bool
  -> m ()
sidePanel visDyn = do
  let
    panelItems =
      [ ("Reader", "fa-book", FrontendRoute_Reader :/ ())
      , ("SRS", "fa-question-circle", FrontendRoute_SRS :/ ())
      , ("Sentence", "fa-align-justify", FrontendRoute_Analyze :/ ())
      ]
  elClass "nav" "panel" $ do
    for panelItems $ \(t, i, l) -> do
      (e, _) <- elClass' "a" "panel-block" $ do
        elClass "span" "panel-icon" $ icon i
        text t
      setRoute (l <$ domEvent Click e)
  return ()

readerControls
  :: forall t m js .( DomBuilder t m
     , Routed t (R FrontendRoute) m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , Prerender js t m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     )
  => m (Dynamic t ReaderControls)
readerControls = do
  let
    initRc = ReaderControls 120 120 True 15 20 2
    sizeOptions :: Dynamic t (Map Int Text)
    sizeOptions = constDyn $ Map.fromList $
      map (\v -> (v, T.pack $ show v))
      [120, 125 .. 250]
    gapOptions :: Dynamic t (Map Int Text)
    gapOptions = constDyn $ Map.fromList $
      map (\v -> (v, T.pack $ show v))
      [120, 125 .. 250]
    directionOptions :: Dynamic t (Map Text Text)
    directionOptions = constDyn $ Map.fromList $
      -- [("V", "V"), ("H", "H")]
      [("V", "V")]
    charCountOptions :: Dynamic t (Map Int Text)
    charCountOptions = constDyn $ Map.fromList $
      map (\v -> (v, T.pack $ show v))
      [15..30]
    lineCountOptions :: Dynamic t (Map Int Text)
    lineCountOptions = constDyn $ Map.fromList $
      map (\v -> (v, T.pack $ show v))
      [25..50]
    rowsOptions :: Dynamic t (Map Int Text)
    rowsOptions = constDyn $ Map.fromList $
      map (\v -> (v, T.pack $ show v))
      [2..5]
  -- Hoverable
    allControls :: (DomBuilder t m) => Event t ReaderControls -> (forall a . m a -> m a) -> (forall b. m b -> m b) -> m (Event t ReaderControls)
    allControls irc wrap nest = wrap $ do
      s <- nest $
        divClass "select" $ dropdown 120 sizeOptions $ def
          & dropdownConfig_setValue .~ (fmap _readerControls_fontSize irc)
      g <- nest $
        divClass "select" $ dropdown 120 gapOptions $ def
          & dropdownConfig_setValue .~ (fmap _readerControls_lineGap irc)
      c <- nest $
        divClass "select" $ dropdown 15 charCountOptions $ def
          & dropdownConfig_setValue .~ (fmap _readerControls_charPerLine irc)
      l <- nest $
        divClass "select" $ dropdown 30 lineCountOptions $ def
          & dropdownConfig_setValue .~ (fmap _readerControls_lineCount irc)
      d <- nest $
        divClass "select" $ dropdown "V" directionOptions $ def
          & dropdownConfig_setValue .~ (fmap ((bool "H" "V") . _readerControls_isVertical) irc)
      r <- nest $
        divClass "select" $ dropdown 2 rowsOptions $ def
          & dropdownConfig_setValue .~ (fmap _readerControls_rowCount irc)
      nest $
        elClass "label" "checkbox" $ do
          inputElement $ def
            & initialAttributes .~ "type" =: "checkbox"
            -- & inputElementConfig_setChecked .~ setChecked
          text "Highlight"
      let
        rc = ReaderControls
          <$> (value s)
          <*> (value g)
          <*> ((==) "V" <$> (value d))
          <*> (value l)
          <*> (value c)
          <*> (value r)
      updated <$> holdUniqDyn rc

  rec
    rcd1 <- prerender (pure never) $ debounce 0.1 rc2
    let rcd = switch $ current rcd1
    rc1 <- divClass "navbar-item is-hidden-mobile" $ allControls rcd (divClass "navbar-item") id
    rc2 <- divClass "navbar-item has-dropdown is-hoverable is-hidden-tablet" $ do
      elClass "a" "navbar-link" $ do
        text ""
      divClass "navbar-dropdown is-right" $ do
        allControls rc1 id (divClass "navbar-item")
  holdDyn initRc (leftmost [rc1, rc2])
