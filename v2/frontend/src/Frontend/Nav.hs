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

import Common.Api
import Common.Route
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
                  then ("style" =: "width: 14em;")
                  else Map.empty)
        attr3 = ffor showPanel $ \s -> ("height" =: "30") <>
                ("src" =: "https://tenjinreader.com/static/logo.png") <>
                ("width" =: "60") <>
          (if not s
            then ("style" =: "display: none;")
            else Map.empty)
      elDynAttr "a" attr2 $ mdo
        elDynAttr "img" attr3 $ return ()
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
  elClass "nav" "panel" $ do
    elClass "a" "panel-block is-active" $ do
      elClass "span" "panel-icon" $ do
        let
          attr4 = ("aria-hidden" =: "true") <> ("class" =: "fas fa-book")
        elAttr "i" attr4 $ return ()
        return ()
      text "bulma"
      return ()
    elClass "a" "panel-block" $ do
      elClass "span" "panel-icon" $ do
        let
          attr5 = ("aria-hidden" =: "true") <> ("class" =: "fas fa-book")
        elAttr "i" attr5 $ return ()
        return ()
      text "marksheet"
      return ()
    elClass "a" "panel-block" $ do
      elClass "span" "panel-icon" $ do
        let
          attr6 = ("aria-hidden" =: "true") <> ("class" =: "fas fa-book")
        elAttr "i" attr6 $ return ()
        return ()
      text "minireset.css"
      return ()
    elClass "a" "panel-block" $ do
      elClass "span" "panel-icon" $ do
        let
          attr7 = ("aria-hidden" =: "true") <> ("class" =: "fas fa-book")
        elAttr "i" attr7 $ return ()
        return ()
      text "jgthms.github.io"
      return ()
    elClass "a" "panel-block" $ do
      elClass "span" "panel-icon" $ do
        let
          attr8 = ("aria-hidden" =: "true") <>
                  ("class" =: "fas fa-code-branch")
        elAttr "i" attr8 $ return ()
        return ()
      text "daniellowtw/infboard"
      return ()
    elClass "a" "panel-block" $ do
      elClass "span" "panel-icon" $ do
        let
          attr9 = ("aria-hidden" =: "true") <>
                  ("class" =: "fas fa-code-branch")
        elAttr "i" attr9 $ return ()
        return ()
      text "mojs"
      return ()
    elClass "label" "panel-block" $ do
      let
        cbConf10 = def
                   & (checkboxConfig_attributes .~ (constDyn (attr11)))
        attr11 = Map.empty
      cb10 <- checkbox False cbConf10
      text "remember me"
      return ()
    divClass "panel-block" $ do
      let
        attr12 = ("class" =: "button is-link is-outlined is-fullwidth")
      (btnEl13,_) <- elAttr' "button" attr12 $ do
        text "reset all filters"
        return ()
      {-
        let
          btnEv13 = domEvent Click btnEl13
      -}
      return ()
    return ()
