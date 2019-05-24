{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Frontend.Home where

import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom.Core

import qualified Data.Text as T
import Data.Dependent.Sum (DSum(..))
import Control.Monad.Fix
import Data.Functor.Identity

import Common.Api
import Common.Route
import Frontend.Common
import Obelisk.Generated.Static

home
  :: ( DomBuilder t m
     , Routed t (R FrontendRoute) m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     )
  => m ()
home = do
  divClass "tile is-ancestor" $ do
    divClass "tile is-vertical" $ do
      divClass "tile" $ do
        divClass "tile is-parent is-vertical" $ do
          elClass "article" "tile is-child" $ randomSentenceWidget
      divClass "tile is-parent" $ do
        elClass "article" "tile is-child notification is-danger is-8" $ do
          elClass "p" "title" $ do
            text "Books list"
          elClass "p" "subtitle" $ do
            text "Book 1"
          divClass "content" $ return ()
        elClass "article" "tile is-child notification is-success" $ do
          divClass "content" $ do
            elClass "p" "title" $ do
              text "Words list"
            elClass "p" "subtitle" $ do
              text "long list"
            divClass "content" $ return ()
  e <- button "click"
  count e >>= display

randomSentenceWidget
  :: ( DomBuilder t m
     , Routed t (R FrontendRoute) m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     )
  => m ()
randomSentenceWidget = divClass "box" $ do
  let
    s = "ここからそのホテルまでどのくらいの距離ですか。"
    e = "How far is it from here to the hotel?"
  elClass "p" "is-size-2-desktop is-size-3-touch" $ do
    text s
  elClass "nav" "level" $ do
    divClass "level-left" $ do
      elClass "p" "is-size-4-desktop is-size-5-touch" $ text e
    divClass "level-right" $ do
      btnIcon "" "fa-plus"
      btnIcon "" "fa-check-double"
      btnIcon "" "fa-align-justify"
  blank
