{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Frontend.Nav where

import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom.Core

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Fix
import Control.Monad.Reader (MonadReader, asks)
import Data.Bool
import Data.Dependent.Sum (DSum(..))
import Data.Functor.Identity
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable

import Common.Api
import Common.Route
import Common.Types
import Frontend.Common
import Frontend.Vocab
import Obelisk.Generated.Static

nav
  :: forall t m js .( DomBuilder t m
     , Routed t (R FrontendRoute) m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , Prerender js t m
     , MonadReader (AppData t) m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     )
  => Event t ()
  -> m (Dynamic t Bool, Event t NavControls)
nav _ = do
  elClass "header" "" $ do
    topBar never

topBar
  :: forall t m js .( DomBuilder t m
     , Routed t (R FrontendRoute) m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , Prerender js t m
     , MonadReader (AppData t) m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     )
  => Event t ()
  -> m (Dynamic t Bool, Event t NavControls)
topBar inpEv = do
  theme <- asks _appData_theme
  let
    attr1 = fmap ((<>) (("aria-label" =: "main navigation")
      <> ("role" =: "navigation"))) $ ffor theme $ \t -> "class" =: ("navbar" <> case t of
        Theme_White -> " has-background-white-bis"
        Theme_Light -> " has-background-grey-light"
        Theme_Dark -> " has-background-grey-darker")
  elDynAttr "nav" attr1 $ mdo
    showPanel <- divClass "navbar-brand" $ do
      let
        style s = "style" =: ("width: 16em;" <> s)
        attr2 = ffor2 theme showPanel $ \t s ->
          ("class" =: ((if t == Theme_Dark then "has-text-white-ter " else "") <> "navbar-item has-text-centered")) <>
                (if s
                  then style ""
                  else Map.empty)
        attr3 = ffor showPanel $ \s -> ("class" =: "is-size-3") <>
          -- ("height" =: "30") <>
                -- ("src" =: "https://tenjinreader.com/static/logo.png") <>
          style (if not s then "display: none;" else "")
      elDynAttr "a" attr2 $ mdo
        -- (e,_) <- elDynAttr' "img" attr3 $ return ()
        (e,_) <- elDynAttr' "span" attr3 $ text "TR"
        setRoute ((FrontendRoute_Home :/ ()) <$ domEvent Click e)
        toggle True =<< btnIcon "has-background-grey-lighter" "fa-bars"
    let
      attr8 = ("class" =: "navbar-menu") <>
              ("id" =: "navbarBasicExample")
    (nc, ie) <- elAttr "div" attr8 $ do
      divClass "navbar-start" $ return ()
      divClass "navbar-end" $ do
        nc <- navbarContents
        divClass "navbar-item" $ do
          vocabSearchInput
        return (nc, ie)
    return (showPanel, nc)

sidePanel
  :: forall t m .( DomBuilder t m
     , Routed t (R FrontendRoute) m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , MonadReader (AppData t) m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     )
  => Dynamic t Bool
  -> m (Event t Theme)
sidePanel visDyn = do
  let
    pReview = constDyn "23"
    rReview = constDyn "43"
    panelItems =
      [ ("Reader", "fa-book", FrontendRoute_Reader :/ (), Nothing)
      , ("Review: Production", "fa-question-circle", FrontendRoute_SRS :/ (), Just pReview)
      , ("Review: Recognition", "fa-question-circle", FrontendRoute_SRS :/ (), Just rReview)
      , ("Sentence", "fa-language", FrontendRoute_Analyze :/ (), Nothing)
      , ("Browse SRS", "fa-list-alt", FrontendRoute_SRS :/ (), Nothing)
      ]
  elClass "nav" "panel" $ do
    for panelItems $ \(t, i, l, mtg) -> do
      (e, _) <- elClassT' "a" "panel-block" $ do
        elClassT "span" "panel-icon" $ icon i
        elClass "span" "" $ text t
        forM_ mtg $ \tg ->
          elAttr "span" (("class" =: "tag") <> ("style" =: "margin-left:1em")) $ dynText tg
      setRoute (l <$ domEvent Click e)
    elClass "a" "panel-block" $ do
      e1 <- btnIcon "is-outlined is-black has-background-white" ""
      e2 <- btnIcon "is-outlined is-black has-background-grey-lighter" ""
      e3 <- btnIcon "is-outlined is-white has-background-grey-dark" ""
      pure $ leftmost
        [ Theme_White <$ e1
        , Theme_Light <$ e2
        , Theme_Dark <$ e3
        ]

navbarContents
  :: forall t m js .( DomBuilder t m
     , Routed t (R FrontendRoute) m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , Prerender js t m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     )
  => m (Event t NavControls)
navbarContents = askRoute >>= \r1 -> do
  v <- dyn $ ffor r1 $ \case
    FrontendRoute_Home :/ () -> pure never
    FrontendRoute_Reader :/ () -> readerControls
    FrontendRoute_SRS :/ () -> reviewStats
    FrontendRoute_Analyze :/ () -> pure never
  switchHold never v

readerControls
  :: forall t m js .( DomBuilder t m
     , Routed t (R FrontendRoute) m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , Prerender js t m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     )
  => m (Event t NavControls)
readerControls = do
  let
    initRc = ReaderControls 120 120 True 15 20 2
    sizeOptions :: Dynamic t (Map Int Text)
    sizeOptions = constDyn $ Map.fromList $
      map (\v -> (v, T.pack $ show v))
      [120, 125 .. 250]
    gapOptions :: Dynamic t (Map Int Text)
    gapOptions = constDyn $ Map.fromList $
      map (\v -> (v, T.pack $ show v))
      [120, 125 .. 250]
    directionOptions :: Dynamic t (Map Text Text)
    directionOptions = constDyn $ Map.fromList $
      -- [("V", "V"), ("H", "H")]
      [("V", "V")]
    charCountOptions :: Dynamic t (Map Int Text)
    charCountOptions = constDyn $ Map.fromList $
      map (\v -> (v, T.pack $ show v))
      [15..30]
    lineCountOptions :: Dynamic t (Map Int Text)
    lineCountOptions = constDyn $ Map.fromList $
      map (\v -> (v, T.pack $ show v))
      [25..50]
    rowsOptions :: Dynamic t (Map Int Text)
    rowsOptions = constDyn $ Map.fromList $
      map (\v -> (v, T.pack $ show v))
      [2..5]
  -- Hoverable
    allControls :: (DomBuilder t m) => Event t ReaderControls -> (forall a . m a -> m a) -> (forall b. m b -> m b) -> m (Event t ReaderControls)
    allControls irc wrap nest = wrap $ do
      s <- nest $
        divClass "select" $ dropdown 120 sizeOptions $ def
          & dropdownConfig_setValue .~ (fmap _readerControls_fontSize irc)
      g <- nest $
        divClass "select" $ dropdown 120 gapOptions $ def
          & dropdownConfig_setValue .~ (fmap _readerControls_lineGap irc)
      c <- nest $
        divClass "select" $ dropdown 15 charCountOptions $ def
          & dropdownConfig_setValue .~ (fmap _readerControls_charPerLine irc)
      l <- nest $
        divClass "select" $ dropdown 30 lineCountOptions $ def
          & dropdownConfig_setValue .~ (fmap _readerControls_lineCount irc)
      d <- nest $
        divClass "select" $ dropdown "V" directionOptions $ def
          & dropdownConfig_setValue .~ (fmap ((bool "H" "V") . _readerControls_isVertical) irc)
      r <- nest $
        divClass "select" $ dropdown 2 rowsOptions $ def
          & dropdownConfig_setValue .~ (fmap _readerControls_rowCount irc)
      -- nest $
      --   elClass "label" "checkbox" $ do
      --     inputElement $ def
      --       & initialAttributes .~ "type" =: "checkbox"
      --       -- & inputElementConfig_setChecked .~ setChecked
      --     text "Highlight"
      let
        rc = ReaderControls
          <$> (value s)
          <*> (value g)
          <*> ((==) "V" <$> (value d))
          <*> (value l)
          <*> (value c)
          <*> (value r)
      updated <$> holdUniqDyn rc

  rec
    rcd1 <- prerender (pure never) $ debounce 0.1 rc2
    let rcd = switch $ current rcd1
    rc1 <- divClass "navbar-item is-hidden-mobile" $ allControls rcd (divClass "navbar-item") id
    rc2 <- divClass "navbar-item has-dropdown is-hoverable is-hidden-tablet" $ do
      elClass "a" "navbar-link" $ do
        text ""
      divClass "navbar-dropdown is-right" $ do
        allControls rc1 id (divClass "navbar-item")
  pure $ NavControls_ReaderControls <$> (leftmost [rc1, rc2])

reviewStats
  :: forall t m js .( DomBuilder t m
     , Routed t (R FrontendRoute) m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , Prerender js t m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     )
  => m (Event t NavControls)
reviewStats = do
  divClass "navbar-item" $
    divClass "tags has-addons" $ do
      divClass "tag" $ text "23"
      divClass "tag is-success" $ text "42"
      divClass "tag is-warning" $ text "3"

  let
  pure never
