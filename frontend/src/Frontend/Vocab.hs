{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Frontend.Vocab where

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

vocabSearchInput
  :: ( DomBuilder t m
     , Routed t (R FrontendRoute) m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     )
  => m ()
vocabSearchInput = mdo
  let cDyn = ffor isActive $ \a -> "dropdown" <>
               if a then " is-active" else ""
  isActive <- elDynClass "div" cDyn $ do
    ie <- divClass "dropdown-trigger" $ do
      inputElement $ def
        & initialAttributes .~
        ("class" =: "input" <> "style" =: "width:30vw; max-width: 15em;")
    let
      attr4 = ("class" =: "dropdown-menu") <> ("id" =: "dropdown-menu")
              <> ("role" =: "menu")
    elAttr "div" attr4 $ do
      divClass "dropdown-content" $ do
        let
          attr5 = ("class" =: "dropdown-item") <> ("href" =: "#")
        elAttr "a" attr5 $ do
          text "Dropdown item"
          return ()
        elClass "a" "dropdown-item" $ do
          text "Other dropdown item"
          return ()
        let
          attr6 = ("class" =: "dropdown-item is-active") <> ("href" =: "#")
        elAttr "a" attr6 $ do
          text "Active dropdown item"
          return ()
        let
          attr7 = ("class" =: "dropdown-item") <> ("href" =: "#")
        elAttr "a" attr7 $ do
          text "Other dropdown item"
          return ()
        elClass "hr" "dropdown-divider" $ return ()
        let
          attr8 = ("class" =: "dropdown-item") <> ("href" =: "#")
        elAttr "a" attr8 $ do
          text "With a divider"

    return $ (not . T.null) <$> (value ie)
  return ()