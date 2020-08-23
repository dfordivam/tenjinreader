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
wordMeanings = divClass "" $ do
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
    wd (Just (e, es)) = wrapper e $
      (mapM_ showVocabDetails es)
    wd Nothing = return never

  void $ prerender (return ()) $ do
    openEv <- getPostBuild
    (e, _) <- el' "div" blank
    let details = (Just $ _element_raw e, vocabs)
        detailsEv = details <$ openEv
    rec
      let ev = leftmost [Just <$> detailsEv
               , Nothing <$ ((switch . current) closeEv)]
      closeEv <- widgetHold (return never)
        (wd <$> ev)
    pure ()

  sentencesModal "surface" sentences
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
  -> m (Event t ())
showVocabDetails (v, m, p) = divClass "message" $ do
  divClass "message-body" $ do
    e <- divClass "level is-mobile" $ do
      divClass "level-left" $ divClass "level-item is-size-3" $ text v
      divClass "level-right" $ divClass "level-item" $ do
        btnIcon "" "fa-plus" (Just "Add to SRS")
        btnIcon "" "fa-check" (Just "Hide furigana / Mark known")
        btnIcon "" "fa-align-justify" (Just "Show example sentences")
    divClass "" $ do
      elClass "span" "" $ text $ "(" <> p <> ") "
      elClass "span" "is-size-5" $ text m
    pure e

type Sentence = ([Text], [Text])

sentences :: [Sentence]
sentences =
  [ (["あなたから連絡がない限り、五時に会う予定でいます。"], ["Unless I hear from you, I'll plan to meet you at five."])
  , (["彼女は今ディナーを食べているところです。", "彼女はディナーを食べているところです。"], ["She is having dinner now."])
  ]
sentencesModal
  :: ( DomBuilder t m
     , Routed t (R FrontendRoute) m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , MonadReader (AppData t) m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     )
  => Text
  -> [Sentence]
  -> m (Event t ())
sentencesModal surface ss = modalDiv $ do
  (headElm, closeEvTop) <- elClass' "header" "modal-card-head" $ do
    (e,_) <- elClass' "button" "delete" $ return ()
    divClass "column is-1" $ return ()
    elClass "p" "modal-card-title" $ do
      text surface
    return (domEvent Click e)

  let bodyAttr = ("class" =: "modal-card-body")
          <> ("style" =: "")

  cl2 <- elAttr "div" bodyAttr $ do
    rec
      mapM renderOneSentence ss

      closeBot <- divClass "level" $ do
        rec
          loadMoreEv <- divClass "level-item" $ do
            btn "" "Load More" Nothing
        divClass "level-item" $ do
          btn "btn-primary" "Top" Nothing
          -- performEvent_ $ topEv $> DOM.scrollIntoView (_element_raw headElm) True
        closeBot1 <- divClass "level-item" $ do
          btn "btn-primary" "Close" Nothing
        return closeBot1

    pure closeBot

  return (leftmost [closeEvTop, cl2])

modalDiv :: (DomBuilder t m, PostBuild t m, MonadReader (AppData t) m) => m b -> m b
modalDiv m = do
  elAttr "div" attr $ do
    divClass "modal-background" $ return ()
    divClass "modal-card" m
  where attr = ("class" =: "modal")
          <> ("style" =: "display: block;")

renderOneSentence
  :: ( DomBuilder t m
     , Routed t (R FrontendRoute) m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     , MonadReader (AppData t) m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     )
  => Sentence
  -> m ()
renderOneSentence (js, es) = divClassT "box" $ do
  let
  divClassT "is-size-3-desktop is-size-4-touch" $ do
    forM js $ \s -> el "p" $ text s
  showEng <- elClass "nav" "level" $ do
    divClass "level-left" $ blank
    divClass "level-right" $ do
      btnIcon "" "fa-bookmark" (Just "Bookmark sentence")
      btnIcon "" "fa-expand" (Just "Show sentence with inlined furigana")
      btnIcon "" "fa-language" (Just "Show english")
  widgetHold blank $ ffor showEng $ \_ ->
    divClassT "is-size-4-desktop is-size-5-touch" $
      forM_ es $ \s -> el "p" $ text s
  blank
