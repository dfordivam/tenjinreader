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

import Common.Api
import Common.Route
import Obelisk.Generated.Static

nav
  :: forall t m .( DomBuilder t m
     , Routed t (R FrontendRoute) m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     )
  => Event t ()
  -> m ()
nav _ = do
  elClass "header" "" $ do
    topBar never

topBar
  :: forall t m .( DomBuilder t m
     , Routed t (R FrontendRoute) m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     )
  => Event t ()
  -> m ()
topBar _ = do
  let
    attr1 = ("aria-label" =: "main navigation") <>
            ("class" =: "navbar") <> ("role" =: "navigation")
  elAttr "nav" attr1 $ do
    divClass "navbar-brand" $ do
      let
        attr2 = ("class" =: "navbar-item") <>
                ("href" =: "https://bulma.io")
      elAttr "a" attr2 $ do
        let
          attr3 = ("height" =: "28") <>
                  ("src" =: "https://bulma.io/images/bulma-logo.png") <>
                  ("width" =: "112")
        elAttr "img" attr3 $ return ()
        return ()
      let
        attr4 = ("aria-expanded" =: "false") <> ("aria-label" =: "menu") <>
                ("class" =: "navbar-burger burger") <>
                ("data-target" =: "navbarBasicExample") <> ("role" =: "button")
      elAttr "a" attr4 $ do
        let
          attr5 = ("aria-hidden" =: "true")
        elAttr "span" attr5 $ return ()
        let
          attr6 = ("aria-hidden" =: "true")
        elAttr "span" attr6 $ return ()
        let
          attr7 = ("aria-hidden" =: "true")
        elAttr "span" attr7 $ return ()
        return ()
      return ()
    let
      attr8 = ("class" =: "navbar-menu") <>
              ("id" =: "navbarBasicExample")
    elAttr "div" attr8 $ do
      divClass "navbar-start" $ do
        elClass "a" "navbar-item" $ do
          text "Home"
          return ()
        elClass "a" "navbar-item" $ do
          text "Documentation"
          return ()
        divClass "navbar-item has-dropdown is-hoverable" $ do
          elClass "a" "navbar-link" $ do
            text "More"
            return ()
          divClass "navbar-dropdown" $ do
            elClass "a" "navbar-item" $ do
              text "About"
              return ()
            elClass "a" "navbar-item" $ do
              text "Jobs"
              return ()
            elClass "a" "navbar-item" $ do
              text "Contact"
              return ()
            elClass "hr" "navbar-divider" $ return ()
            elClass "a" "navbar-item" $ do
              text "Report an issue"
              return ()
            return ()
          return ()
        return ()
      divClass "navbar-end" $ do
        divClass "navbar-item" $ do
          divClass "buttons" $ do
            elClass "a" "button is-primary" $ do
              el "strong" $ do
                text "Sign up"
                return ()
              return ()
            elClass "a" "button is-light" $ do
              text "Log in"
              return ()
            return ()
          return ()
        return ()
      return ()
    return ()

sidePanel
  :: forall t m .( DomBuilder t m
     , Routed t (R FrontendRoute) m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     )
  => Event t ()
  -> m ()
sidePanel _ = do
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
