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

import Common.Api
import Common.Route
import Obelisk.Generated.Static

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = headTag
  , _frontend_body = do
      let nav _ = text "nav"
      rec el "header" $ nav click
          click <- mainContainer sections
      return ()
  }

mainContainer :: DomBuilder t m => m () -> m (Event t ())
mainContainer w = domEvent Click . fst <$> el' "main" w

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

home
  :: ( DomBuilder t m
     , MonadHold t m
     , MonadFix m
     , PostBuild t m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     )
  => m ()
home = do
  e <- button "click"
  count e >>= display
  routeLink (FrontendRoute_Reader :/ ()) $ text "reader"

reader
  :: ( DomBuilder t m
     , MonadHold t m
     , MonadFix m
     , PostBuild t m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     )
  => m ()
reader = do
  e <- button "click 2"
  count e >>= display
  routeLink (FrontendRoute_Home :/ ()) $ text "home"

headTag :: DomBuilder t m => m ()
headTag = do
  traverse (\s -> elAttr "link" ("rel" =: "stylesheet" <> "href" =: s) blank)
    [ static @"css/tenjin-reader.css"
    ]
  elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1.0, maximum-scale=1.0") blank
  elAttr "meta" ("charset" =: "utf-8") blank
