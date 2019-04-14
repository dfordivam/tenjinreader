{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Frontend where

import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom.Core

import qualified Data.Text as T
import Data.Dependent.Sum (DSum(..))
import Control.Monad.Fix
import Data.Functor.Identity
import Data.Bool

import Common.Api
import Common.Route
import Obelisk.Generated.Static

import Frontend.Home
import Frontend.Nav
import Frontend.Head
import Frontend.Reader
import Frontend.SRS
import Frontend.Analyze
-- import Frontend.

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = headEl
  , _frontend_body = do
      rec navDyn <- nav click
          click <- divClass "columns" $ do
            elDynClass "div" ((<>) "column is-narrow " <$> (ffor navDyn $ bool "is-hidden" "")) $ sidePanel navDyn
            mainContainer sections
      return ()
  }

mainContainer :: DomBuilder t m => m () -> m (Event t ())
mainContainer w = domEvent Click . fst <$> elClass' "main" "column container section" w

sections
  :: ( DomBuilder t m
     , Routed t (R FrontendRoute) m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     )
  => m ()
sections = do
  askRoute >>= \r -> do
    let
      sec' = ffor r $ \r' -> case r' of
        FrontendRoute_Home :/ () -> Section_Home
        FrontendRoute_Reader :/ () -> Section_Reader
        FrontendRoute_SRS :/ () -> Section_SRS
        FrontendRoute_Analyze :/ () -> Section_Analyze
    sequence_ $ ffor sectionsList $ \(s', wm) -> do
      let vis = ffor sec' $ ("style" =:) . \s -> if s == s'
            then ""
            else "display: none;"
      elDynAttr "section" vis wm

sectionsList
  :: ( DomBuilder t m
     , Routed t (R FrontendRoute) m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     )
  => [(Section, m ())]
sectionsList =
  [ (Section_Home, home)
  , (Section_Reader, reader)
  , (Section_SRS, srs)
  , (Section_Analyze, analyze)
  ]

data Section
  = Section_Home
  | Section_Reader
  | Section_SRS
  | Section_Analyze
  deriving (Eq, Show)
