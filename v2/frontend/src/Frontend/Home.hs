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
          elClass "article" "tile is-child notification is-primary" $ do
            elClass "p" "title" $ do
              text "Vertical..."
            elClass "p" "subtitle" $ do
              text "Top tile"
          elClass "article" "tile is-child notification is-warning" $ do
            elClass "p" "title" $ do
              text "...tiles"
            elClass "p" "subtitle" $ do
              text "Bottom tile"
      divClass "tile is-parent" $ do
        elClass "article" "tile is-child notification is-danger" $ do
          elClass "p" "title" $ do
            text "Wide tile"
          elClass "p" "subtitle" $ do
            text "Aligned with the right tile"
          divClass "content" $ return ()
        elClass "article" "tile is-child notification is-success" $ do
          divClass "content" $ do
            elClass "p" "title" $ do
              text "Tall tile"
            elClass "p" "subtitle" $ do
              text "With even more content"
            divClass "content" $ return ()
  e <- button "click"
  count e >>= display
  routeLink (FrontendRoute_Reader :/ ()) $ text "reader"
