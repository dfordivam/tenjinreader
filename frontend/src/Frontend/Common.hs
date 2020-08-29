{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Frontend.Common where

import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom.Core

import Control.Monad.Fix
import Control.Monad.Reader (MonadReader, asks)
import Data.Dependent.Sum (DSum(..))
import Data.Functor.Identity
import Data.Text (Text)
import qualified Data.Text as T

import Common.Api
import Common.Route
import Common.Types
import Obelisk.Generated.Static

import Frontend.Modal.Class

type AppWidget js t m
  = ( DomBuilder t m
    , MonadHold t m
    , MonadFix m
    , PostBuild t m
    , PerformEvent t m
    , TriggerEvent t m
    , MonadReader (AppData t) m
    , Prerender js t m
    , HasModal t m
    , Routed t (R (FrontendRoute)) m
    , RouteToUrl (R (FrontendRoute)) m
    , SetRoute t (R (FrontendRoute)) m
    )

data Section
  = Section_Home
  | Section_Reader
  | Section_SRS
  | Section_Analyze
  deriving (Eq, Show)

data Theme
  = Theme_White
  | Theme_Light
  | Theme_Dark
  deriving (Eq, Show)

data NavControls
  = NavControls_None
  | NavControls_ReaderControls ReaderControls
  deriving (Eq, Show)

data AppData t = AppData
  { _appData_theme :: Dynamic t Theme
  }

icon :: DomBuilder t m => Text -> m ()
icon i = do
  let
    attr4 = ("aria-hidden" =: "true") <> ("class" =: ("fas " <> i))
  elAttr "i" attr4 $ return ()

btnEl :: DomBuilder t m => Text -> Text -> Text -> Maybe Text -> m (Event t ())
btnEl e' c t mt = do
  let attr = ("class" =: ("button " <> c <> (maybe "" (const " tooltip") mt)))
        <> (maybe mempty (\t -> "data-tooltip" =: t) mt)
  (e, _) <- elAttr' e' attr $ text t
  return $ domEvent Click e

btn, btnA :: DomBuilder t m => Text -> Text -> Maybe Text -> m (Event t ())
btn = btnEl "button"
btnA = btnEl "a"

btnIcon :: DomBuilder t m => Text -> Text -> Maybe Text ->m (Event t ())
btnIcon c i mt = do
  let attr = ("class" =: ("button " <> c <> (maybe "" (const " tooltip") mt)))
        <> (maybe mempty (\t -> "data-tooltip" =: t) mt)
  (e, _) <- elAttr' "button" attr $
    elClass "span" "icon" $ elClass "i" ("fas " <> i) blank
  return $ domEvent Click e

divClassT :: (DomBuilder t m, PostBuild t m, MonadReader (AppData t) m) => Text -> m a -> m a
divClassT = elClassT "div"

elClassT :: (DomBuilder t m, PostBuild t m, MonadReader (AppData t) m) => Text -> Text -> m a -> m a
elClassT e c m = snd <$> elClassT' e c m

elClassT' :: (DomBuilder t m, PostBuild t m, MonadReader (AppData t) m) => Text -> Text -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
elClassT' e c m = do
  theme <- asks _appData_theme
  let cDyn = fmap ((<>) c) $ ffor theme $ \case
        Theme_White -> ""
        Theme_Light -> " has-background-grey-lighter"
        Theme_Dark -> " has-background-grey-dark has-text-white-bis"
  elDynClass' e cDyn m
