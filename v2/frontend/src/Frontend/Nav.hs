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

import qualified Data.Text as T
import Data.Dependent.Sum (DSum(..))
import Control.Monad.Fix
import Data.Functor.Identity
import qualified Data.Map as Map
import Control.Monad
import Data.Traversable

import Common.Api
import Common.Route
import Frontend.Common
import Obelisk.Generated.Static

nav
  :: ( DomBuilder t m
     , Routed t (R FrontendRoute) m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     , Prerender js m
     )
  => Event t ()
  -> m (Dynamic t Bool)
nav _ = do
  elClass "header" "" $ do
    topBar never

topBar
  :: ( DomBuilder t m
     , Routed t (R FrontendRoute) m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     , Prerender js m
     )
  => Event t ()
  -> m (Dynamic t Bool)
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
    ie <- elAttr "div" attr8 $ do
      divClass "navbar-start" $ return ()
      divClass "navbar-end" $ do
        divClass "navbar-item" $ do
          let
          inputElement $ def
            & initialAttributes .~ "class" =: "input"
    dynText $ value ie
    return showPanel

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
