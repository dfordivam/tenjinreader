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
module Frontend.Modals where

import Obelisk.Frontend
import Obelisk.Generated.Static
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom.Core

import qualified GHCJS.DOM.DOMRectReadOnly as DOM
import qualified GHCJS.DOM.Element as DOM
import qualified GHCJS.DOM.Types as DOM

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Reader (MonadReader, asks)
import Data.Dependent.Sum (DSum(..))
import Data.Functor.Identity
import Data.Text (Text)
import qualified Data.Text as T

import Common.Api
import Common.Route
import Common.Types
import Frontend.Common

wordMeanings
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
  => m ()
wordMeanings = void $ prerender (return ()) $ divClass "" $ do
  let

    attrBack = ("class" =: "modal")
          <> ("style" =: "display: block;\
              \opacity: 0%; z-index: 1050;")
    attrFront y h
      | h > 300 && y < 300 = f "is-fixed-top" -- Should be middle?
      | y > 300 = f "is-fixed-top"
      | otherwise = f "is-fixed-bottom"
      where f p = ("class" =: ("navbar " <> p))
              <> ("style" =: "z-index: 1060; padding: 2vw;")

    wrapper :: (Monad m1, DOM.MonadJSM m1, DomBuilder t m1, DOM.IsElement e)
      => Maybe e
      -> m1 a
      -> m1 (Event t ())
    wrapper e m = do
      (y,h) <- case e of
        Nothing -> return (0,0)
        (Just e') -> DOM.liftJSM $ do
          rect <- DOM.getBoundingClientRect (e')
          y <- DOM.getY rect
          h <- DOM.getHeight rect
          return (y,h)
      (e1,_) <- elAttr' "div" attrBack $ return ()
      elAttr "div" (attrFront y h) $
        elAttr "div" (("class" =: "notification")
          <> ("style" =: "max-height: 30vh; overflow-y: auto; overflow-x: hidden; min-width: 96vw;")) $ do
          (e2,_) <- elClass' "button" "delete" $ return ()
          _ <- m
          return $ leftmost
            [domEvent Click e2
            , domEvent Click e1]

    wd :: (Monad m1, DOM.MonadJSM m1, DomBuilder t m1, DOM.IsElement e)
      => Maybe (Maybe e, [VocabDetails])
      -> m1 (Event t ())
    wd (Just (e, es)) = (wrapper e)
      (mapM_ showVocabDetails es)
    wd Nothing = return never

  openEv <- getPostBuild
  (e, _) <- el' "div" blank
  let details = (Just $ _element_raw e, vocabs)
      detailsEv = details <$ openEv
  rec
    let ev = leftmost [Just <$> detailsEv
             , Nothing <$ ((switch . current) closeEv)]
    closeEv <- widgetHold (return never)
      (wd <$> ev)

  return ()

type VocabDetails = (Text, Text, Text)

vocabs :: [VocabDetails]
vocabs =
  [ ("長長", "Long, Drawn-Out, Very long", "Adv")
  , ("ながなが", "Long, Drawn-Out, Very long", "Adv")
  ]

showVocabDetails
  :: forall t m .( DomBuilder t m
     )
  => VocabDetails
  -> m ()
showVocabDetails (v, m, p) = divClass "message" $ do
  void $ divClass "message-body" $ do
    divClass "level is-mobile" $ do
      divClass "level-left" $ divClass "level-item is-size-3" $ text v
      divClass "level-right" $ divClass "level-item" $ do
        btnIcon "" "fa-plus" (Just "Add to SRS")
        btnIcon "" "fa-check" (Just "Hide furigana / Mark known")
        btnIcon "" "fa-align-justify" (Just "Show example sentences")
    divClass "" $ do
      elClass "span" "" $ text $ "(" <> p <> ") "
      elClass "span" "is-size-5" $ text m
