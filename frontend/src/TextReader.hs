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
  ta <- textArea

  send <- button "send"

  annTextEv <- getWebSocketResponse
    (GetAnnotatedText <$> (tagDyn (value ta) send))

  widgetHold (return ())
    (readingPane <$> annTextEv)

vocabRuby :: Dynamic t Bool -> Vocab -> m (_)
vocabRuby visDyn (Vocab ks) =
  let
    g r True = r
    g _ _ = ""
    f (Kana k) = text k
    f (KanjiWithReading (Kanji k) r)
      = el "ruby" $ do
          text k
          el "rt" $ dyntext (g r <$> visDyn)
  (e,_) <- el' "span" $ mapM f ks
  return $ (domEvent Click e, domEvent Mouseenter e, domEvent Mouseleave e)

readingPane :: Dynamic t Bool -> [(Either Text (Vocab, VocabId))]
readingPane showAllFurigana annText = do
  vIdEv <- el "div" $ el "p" $ do
    let f (Left t) = text t
        f (Right (v, vId)) = do
          rec
            let evVis = leftmost [True <$ eme, tagDyn showAllFurigana eml]
            visDyn <- holdDyn True evVis
            (ek, eme, eml) <- vocabRuby visDyn v
          return $ vId <$ ek
    mapM f annText

  el "div" $ do
    detailsEv <- getWebSocketResponse $ GetVocabDetails <$> vIdEv
    showVocabDetailsWidget detailsEv
