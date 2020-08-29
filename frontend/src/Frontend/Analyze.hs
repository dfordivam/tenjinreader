{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Frontend.Analyze where

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

import Frontend.Common
import Frontend.Modal.Class

analyze
  :: AppWidget js t m
  => m ()
analyze = do
  text "ANALYZE"
  e <- button "click"
  count e >>= display
  divClass "" $ do
    el "h2" $ text "Test modals"
    openModal <- btn "" "Open modal" Nothing
    tellModal $ ffor openModal $ \_ -> \closeEv -> do
      modalTest
      pure closeEv

modalTest :: (_) => m ()
modalTest = do
  divClass "modal-card box" $ text "inside modal 1"
  openModal2 <- btn "" "Open another Modal" Nothing
  tellModal $ ffor openModal2 $ \_ -> \closeEv2 -> do
    divClass "modal-card box" $ text "inside modal 2"
    pure closeEv2
