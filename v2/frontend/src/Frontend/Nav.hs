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
  elClass "nav" "panel" $ do
    elClass "p" "panel-heading" $ do
      text "repositories"
      return ()
    divClass "panel-block" $ do
      elClass "p" "control has-icons-left" $ do
        let
          ieConf :: (Reflex t) => InputElementConfig EventResult t (DomBuilderSpace m)
          ieConf = def
                    & (inputElementConfig_initialValue .~ "search")
                    -- & (inputElementConfig_elementConfig .~ elConf)
          -- elConf = def
            -- & elementConfig_initialAttributes .~ attr2
          attr2 = ("class" =: "input is-small")
        ti1 <- inputElement ieConf
        elClass "span" "icon is-small is-left" $ do
          let
            attr3 = ("aria-hidden" =: "true") <> ("class" =: "fas fa-search")
          elAttr "i" attr3 $ return ()
    elClass "p" "panel-tabs" $ do
      elClass "a" "is-active" $ do
        text "all"
        return ()
      el "a" $ do
        text "public"
        return ()
      el "a" $ do
        text "private"
        return ()
      el "a" $ do
        text "sources"
        return ()
      el "a" $ do
        text "forks"
        return ()
      return ()
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
