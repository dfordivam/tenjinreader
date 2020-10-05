{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Frontend.Home where

import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom.Core

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Reader (MonadReader, asks)
import Data.Functor.Identity
import Data.Traversable
import qualified Data.Text as T
import Data.Dependent.Sum (DSum(..))

import Common.Api
import Common.Route
import Frontend.Common
import Frontend.Modals
import Obelisk.Generated.Static

home
  :: ( DomBuilder t m
     , Routed t (R FrontendRoute) m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , MonadReader (AppData t) m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     )
  => m ()
home = do
  divClass "tile is-ancestor" $ do
    divClass "tile is-vertical" $ do
      divClass "tile is-parent" $ do
        elClass "article" "tile is-child is-11" $ randomSentenceWidget
        divClass "tile is-child" $ do
          divClass "tile is-parent is-vertical" $ do
            divClass "tile is-child" $ btnIcon "is-fullwidth is-size-5-tablet" "fa-random" (Just "Random sentence")
            divClass "tile is-child" $ btnIcon "is-fullwidth is-size-5-tablet" "fa-random" (Just "Random sentence")
      divClass "tile is-parent" $ do
        elClass "article" "tile is-child is-8" $ recentBookList
        elClass "article" "tile is-child" $ divClassT "box" $ recentWordList

randomSentenceWidget
  :: ( DomBuilder t m
     , Routed t (R FrontendRoute) m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , MonadReader (AppData t) m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     )
  => m ()
randomSentenceWidget = do
  let
    s = "ここからそのホテルまでどのくらいの距離ですか。"
    e = "How far is it from here to the hotel?"
  renderOneSentence ([s], [e])

recentBookList
  :: ( DomBuilder t m
     , Routed t (R FrontendRoute) m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , MonadReader (AppData t) m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     )
  => m ()
recentBookList = do
  let
    books =
      [ ("竹取物語", "竹取物語和田萬吉...")
      , ("坊っちゃん", "このことがあつてからも")
      , ("こころ", "ところで、竹たけの中から出た子は、育そだて方かたがよかつ")
      , ("悪魔の弟子", "で人にふさはしい少女りや髮飾かみかざをさせましたが、衣")
      ]
  forM_ books $ \(k, r) -> divClassT "card" $ do
    (e,_) <- elClass' "div" "card-header" $ do
      elClass "p" "card-header-title" $ elClassT "span" "is-size-4-desktop is-size-5-touch" $ text k
      elClass' "a" "card-header-icon" $ elClass "span" "icon" $
        elClass "i" "fas fa-angle-down" blank
           
    let openEv = domEvent Click e
    openDyn <- toggle False openEv 
    dyn $ ffor openDyn $ \b -> when b $ do
      divClass "card-content" $ do
        divClass "content" $ text "彼《かれ》は現代生活《げんだいせいかつ》の複雑性《ふくざつせい》について長々《ながなが》と話《はな》した。"
      divClass "card-footer" $ do
        divClass "card-footer-item" $ text "27%"
        elClass "a" "card-footer-item" $ text "Open"
        elClass "a" "card-footer-item" $ text "Edit"
        elClass "a" "card-footer-item" $ text "Delete"

recentWordList
  :: ( DomBuilder t m
     , Routed t (R FrontendRoute) m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , MonadReader (AppData t) m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     )
  => m ()
recentWordList = do
  let
    words =
      [ ("距離", "きょり")
      , ("途中", "とちゅう")
      , ("恐ろしい", "おそろしい")
      , ("足元", "あしもと")
      , ("見事", "みごと")
      , ("複雑性", "ふくざつせい")
      ]
  forM_ words $ \(k, r) -> divClassT "card" $ do
    (e,_) <- elClass' "div" "card-header" $ do
      elClass "p" "card-header-title" $ elClassT "span" "is-size-4-desktop is-size-5-touch" $ text k
      elClass' "a" "card-header-icon" $ elClass "span" "icon" $
        elClass "i" "fas fa-angle-down" blank
   
    let openEv = domEvent Click e
    openDyn <- toggle False openEv 
    dyn $ ffor openDyn $ \b -> when b $ do
      divClass "card-content" $ do
        divClass "content" $ do
          divClass "" $ text "距離, きょり"
          divClass "" $ text "(Noun) Distance, Range"
      divClass "card-footer" $ do
            elClass "a" "card-footer-item" $ text "Edit SRS"
            elClass "a" "card-footer-item" $ text "Sentences"
            return()
