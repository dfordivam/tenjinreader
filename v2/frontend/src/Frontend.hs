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
import Control.Monad.Reader (MonadReader, runReaderT, asks)
import Data.Functor.Identity
import Data.Bool

import Common.Api
import Common.Route
import Common.Types
import Obelisk.Generated.Static

import Frontend.Analyze
import Frontend.Common
import Frontend.Head
import Frontend.Home
import Frontend.Nav
import Frontend.Reader
import Frontend.SRS
-- import Frontend.

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = headEl
  , _frontend_body = mdo
      themeDyn <- holdDyn Theme_White themeEv
      let appData = AppData themeDyn

      themeEv <- (flip runReaderT) appData $ mdo
        (navDyn, rc) <- nav click
        (themeEv, click) <- divClassT "hero is-fullheight" $ divClass "columns" $ do
          themeEv <- elDynClass "div" ((<>) "column is-narrow " <$> (ffor navDyn $ bool "is-hidden" "")) $
            sidePanel navDyn
          mainContainer (sections rc)
          pure (themeEv, click)
        return themeEv
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
     , MonadReader (AppData t) m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     )
  => Event t NavControls
  -> m ()
sections rc = do
  askRoute >>= \r -> do
    let
      sec' = ffor r $ \r' -> case r' of
        FrontendRoute_Home :/ () -> Section_Home
        FrontendRoute_Reader :/ () -> Section_Reader
        FrontendRoute_SRS :/ () -> Section_SRS
        FrontendRoute_Analyze :/ () -> Section_Analyze
    sequence_ $ ffor (sectionsList rc) $ \(s', wm) -> do
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
     , MonadReader (AppData t) m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     )
  => Event t NavControls
  -> [(Section, m ())]
sectionsList rc =
  [ (Section_Home, home)
  , (Section_Reader, reader rc)
  , (Section_SRS, srs)
  , (Section_Analyze, analyze)
  ]
