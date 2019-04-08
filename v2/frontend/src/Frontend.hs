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
-- import Frontend.
-- import Frontend.
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
mainContainer w = domEvent Click . fst <$> elClass' "main" "column container" w

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
  askRoute >>= \r ->
    sequence_ $ ffor sectionsList $ \(wr, wm) -> do
      let vis = ffor r $ ("style" =:) . \r' -> if r' == wr
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
  => [(DSum FrontendRoute Identity, m ())]
sectionsList =
  [ (FrontendRoute_Home :/ (), home)
  , (FrontendRoute_Reader :/ (), reader)
  ]
