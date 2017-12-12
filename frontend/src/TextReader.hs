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
import qualified Data.List.NonEmpty as NE

textReaderWidget
  :: AppMonad t m
  => AppMonadT t m ()
textReaderWidget = divClass "" $ do
  ta <- textArea def

  send <- button "send"

  annTextEv <- getWebSocketResponse
    (GetAnnotatedText <$> (tagDyn (value ta) send))

  widgetHold (return ())
    (readingPane (constDyn False) <$> annTextEv)
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
  return ()

showVocabDetailsWidget detailsEv = do
  let
    showEntry (e, sId) = divClass "" $ do
      divClass "" $ do
        elClass "span" "" $ do
          entryKanjiAndReading e
        elClass "span" "" $ do
          text $ "Entry other properties"
        addEditSrsEntryWidget (Right $ e ^. entryUniqueId) sId

      divClass "" $ do
        let
          showGloss m = divClass "" $ text $ "-> " <> m
        mapM showGloss $ take 5 $
          e ^.. entrySenses . traverse . senseGlosses . traverse . glossDefinition
  widgetHold (return ())
    ((mapM_ showEntry) <$> (orderEntries <$> detailsEv))

entryKanjiAndReading :: (_) => Entry -> m ()
entryKanjiAndReading e = do
  sequenceA_ (intersperse sep els)
  where
  els = map (renderElement (restrictedKanjiPhrases e)
    (e ^. entryReadingElements . to (NE.head) . readingPhrase))
    (orderElements e)
  sep = text ", "

restrictedKanjiPhrases :: Entry
  -> Map KanjiPhrase ReadingElement
restrictedKanjiPhrases e = Map.fromList $ concat $
  e ^.. entryReadingElements . traverse
    . to (\re -> re ^.. readingRestrictKanji . traverse
           . to (\kp -> (kp, re)))

-- Priority of entries
-- Entry with priority elements
-- Entry normal
-- Entry with Info elements
orderEntries :: [(Entry,a)] -> [(Entry,a)]
orderEntries es = sortBy (comparing f) es
  where
    f (e,_)
      | any (not . null) $
        (ke ^.. traverse . kanjiPriority) ++
        (re ^.. traverse . readingPriority)
        = 1
      | any (not . null)
        (ke ^.. traverse . kanjiInfo) ||
        any (not . null)
        (re ^.. traverse . readingInfo)
        = 3
      | otherwise = 2
      where
        ke = e ^. entryKanjiElements
        re = e ^. entryReadingElements

-- Priority of elements
-- Kanji with priority
-- Reading with priority
-- Kanji with reading
-- Kanji With restricted reading
-- Reading
-- Kanji with Info
-- Reading with Info
orderElements
  :: Entry
  -> [(Either KanjiElement ReadingElement)]
orderElements e = sortBy (comparing f)
  (e ^.. entryKanjiElements . traverse . to (Left)) ++
  readingWithoutRes

  where
    f (Left ke)
      | (ke ^. kanjiPriority . to (not . null)) = 1
      | (ke ^. kanjiInfo . to (not . null)) = 6
      | Map.member (ke ^. kanjiPhrase)
        (restrictedKanjiPhrases e) = 4
      | otherwise = 3

    f (Right re)
      | (re ^. readingPriority . to (not . null)) = 2
      | (re ^. readingInfo . to (not . null)) = 7
      | otherwise = 5

    readingWithoutRes = map Right $
      filter (view $ readingRestrictKanji . to (null)) $
      (e ^.. entryReadingElements . traverse)

renderElement :: (_)
  => Map KanjiPhrase ReadingElement
  -> ReadingPhrase
  -> (Either KanjiElement ReadingElement)
  -> m ()
renderElement restMap defR (Left ke) = case v of
  (Right v) -> displayVocabT v
  (Left _) -> text $ unKanjiPhrase $ ke ^. kanjiPhrase
  where
    kp = (ke ^. kanjiPhrase)
    v = case Map.lookup kp restMap of
          (Just r) -> makeFurigana kp (r ^. readingPhrase)
          Nothing -> makeFurigana kp defR

renderElement _ _ (Right re) =
  text $ unReadingPhrase $ re ^. readingPhrase
