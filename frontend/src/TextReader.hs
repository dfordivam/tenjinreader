{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
module TextReader where

import FrontendCommon

import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List.NonEmpty (NonEmpty)

textReaderWidget
  :: AppMonad t m
  => AppMonadT t m ()
textReaderWidget = divClass "" $ do
  ta <- textArea def

  send <- button "send"

  annTextEv <- getWebSocketResponse
    (GetAnnotatedText <$> (tagDyn (value ta) send))

  widgetHold (return ())
    (readingPane (constDyn True) <$> annTextEv)
  return ()

vocabRuby :: (_) => Dynamic t Bool -> Vocab -> m (_)
vocabRuby visDyn (Vocab ks) = do
  let
    g r True = r
    g _ _ = ""
    f (Kana k) = text k
    f (KanjiWithReading (Kanji k) r)
      = el "ruby" $ do
          text k
          el "rt" $ dynText (g r <$> visDyn)
  (e,_) <- el' "span" $ mapM f ks
  return $ (domEvent Click e, domEvent Mouseenter e, domEvent Mouseleave e)

readingPane :: AppMonad t m
  => Dynamic t Bool
  -> AnnotatedText -- [(Either Text (Vocab, VocabId, Bool))]
  -> AppMonadT t m ()
readingPane showAllFurigana annText = do
  vIdEv <- el "div" $ el "p" $ do
    let f (Left t) = never <$ text t
        f (Right (v, vId, vis)) = do
          rec
            let evVis = leftmost [True <$ eme, tagDyn showAllFurigana eml]
            visDyn <- holdDyn vis evVis
            (ek, eme, eml) <- vocabRuby visDyn v
          return $ vId <$ ek
    leftmost <$> mapM f annText

  el "div" $ do
    detailsEv <- getWebSocketResponse $ GetVocabDetails <$> vIdEv
    showVocabDetailsWidget detailsEv

showVocabDetailsWidget detailsEv = do
  text "vocab detials widget"
