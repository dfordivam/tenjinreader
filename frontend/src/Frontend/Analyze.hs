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
import Common.Types
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
    let lvl = 1
    tellModal $ ffor openModal $ \_ -> \closeEv -> do
      modalTest lvl
      pure closeEv

modalTest :: (_) => Int -> m ()
modalTest lvl = do
  divClass "modal-card box" $ text $ "inside modal 1 " <> (tshow lvl)
  openModal2 <- btn "" "Open another Modal" Nothing
  tellModal $ ffor openModal2 $ \_ -> \closeEv2 -> do
    modalTest2 (lvl + 1)
    pure closeEv2

modalTest2 :: (_) => Int -> m ()
modalTest2 lvl = do
  divClass "modal-card box" $ text $ "inside modal 2 " <> (tshow lvl)
  openModal2 <- btn "" "Open another Modal" Nothing
  tellModal $ ffor openModal2 $ \_ -> \closeEv2 -> do
    modalTest (lvl + 1)
    pure closeEv2
