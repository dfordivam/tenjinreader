{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Frontend.SRS where

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

srs
  :: ( DomBuilder t m
     , Routed t (R FrontendRoute) m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     )
  => m ()
srs = do
  reviewWidget

reviewWidget
  :: ( DomBuilder t m
     , Routed t (R FrontendRoute) m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     )
  => m ()
reviewWidget = divClass "container" $ do
  divClass "container" $ do
    divClass "columns is-centered" $ divClass "column" $
      divClass "is-size-1-desktop is-size-2-touch has-text-centered" $
        text "検討"
    divClass "columns is-centered" $ divClass "column message is-success is-half-tablet" $ do
      divClass "message-body is-size-3-desktop is-size-4-touch has-text-centered-tablet" $
        text "けんとう"
  divClass "container" $ do
    divClass "columns is-centered" $ divClass "column is-half-tablet" $ do
      inputElement $ def
        & initialAttributes .~
        ("class" =: "input" <> "style" =: "text-align: center")
  divClass "container" $ do
    divClass "columns is-centered" $ divClass "column has-text-centered" $ do
      btnA "" "Edit" $ Just "Edit the SRS entry"
      btnA "" "Bury" $ Just "Remove the item from today's review, to do tomorrow"
      btnA "" "Suspend" $ Just "Remove the item from SRS reviews (can be changed from SRS browse menu)"
    divClass "columns is-centered" $ divClass "column is-half-tablet" $ divClass "level is-mobile" $ do
      divClass "level-item" $ btnIcon "is-large is-success" "fa-check-square" $ Just "Mark as correct"
      divClass "level-item" $ btnIcon "is-large is-primary" "fa-align-justify" $ Just "Show example sentences"
      divClass "level-item" $ btnIcon "is-large is-danger" "fa-times-circle" $ Just "Show answer"
  return ()

