{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import qualified Data.Text as T
import Obelisk.Frontend
import Obelisk.Route
import Reflex.Dom.Core

import Common.Api
import Common.Route
import Obelisk.Generated.Static

import Control.Monad.Fix

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = headTag
  , _frontend_body = do
      text "Welcome to Obelisk!"
      el "p" $ text $ T.pack commonStuff
      elAttr "img" ("src" =: static @"images/obelisk.jpg") blank
      landingPage
  }

landingPage
  :: ( DomBuilder t m
     , MonadHold t m
     , MonadFix m
     , PostBuild t m
     )
  => m ()
landingPage = do
  e <- button "click"
  count e >>= display

headTag :: DomBuilder t m => m ()
headTag = do
  traverse (\s -> elAttr "link" ("rel" =: "stylesheet" <> "href" =: s) blank)
    [ static @"css/tenjin-reader.css"
    ]
  elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1.0, maximum-scale=1.0") blank
  elAttr "meta" ("charset" =: "utf-8") blank
