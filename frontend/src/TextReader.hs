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

vocabRuby :: (_) => Dynamic t Int -> Dynamic t Bool -> Vocab -> m (_)
vocabRuby fontSizePctDyn visDyn (Vocab ks) = do
  let
    rubyAttr = (\s -> "style" =: ("font-size: " <> tshow s <>"%;")) <$> fontSizePctDyn
    g r True = r
    g _ _ = ""
    f (Kana k) = text k
    f (KanjiWithReading (Kanji k) r)
      = elDynAttr "ruby" rubyAttr $ do
          text k
          el "rt" $ dynText (g r <$> visDyn)
  (e,_) <- el' "span" $ mapM f ks
  return $ (domEvent Click e, domEvent Mouseenter e, domEvent Mouseleave e)

lineHeightOptions = Map.fromList $ (\x -> (x, (tshow x) <> "%"))
  <$> ([100,150..400]  :: [Int])

fontSizeOptions = Map.fromList $ (\x -> (x, (tshow x) <> "%"))
  <$> ([80,85..200]  :: [Int])

readingPane :: AppMonad t m
  => Dynamic t Bool
  -> AnnotatedText -- [[(Either Text (Vocab, VocabId, Bool))]]
  -> AppMonadT t m ()
readingPane showAllFurigana annText = do
  fontSizeDD <- dropdown 100 (constDyn fontSizeOptions) def
  rubySizeDD <- dropdown 120 (constDyn fontSizeOptions) def
  lineHeightDD <- dropdown 150 (constDyn lineHeightOptions) def
  let divAttr = (\s l -> "style" =: ("font-size: " <> tshow s <>"%;"
                                    <> "line-height: " <> tshow l <> "%;"))
        <$> (value fontSizeDD) <*> (value lineHeightDD)
  vIdEv <- elDynAttr "div" divAttr $ forM annText $ \annTextPara ->
    el "p" $ do
      let f (Left t) = never <$ text t
          f (Right (v, vId, vis)) = do
            rec
              let evVis = leftmost [True <$ eme, tagDyn showAllFurigana eml]
              visDyn <- holdDyn vis evVis
              (ek, eme, eml) <- vocabRuby (value rubySizeDD) visDyn v
            return $ (vId, vocabToText v) <$ ek
          onlyKana (Vocab ks) = (flip all) ks $ \case
            (Kana _) -> True
            _ -> False
          addSpace [] = []
          addSpace (l@(Left _):r@(Right _):rs) =
            l : (Left "　") : (addSpace (r:rs))
          addSpace (r1@(Right (v1,_,_)):r2@(Right _):rs)
            | onlyKana v1 = r1 : (Left "　") : (addSpace (r2:rs))
            | otherwise = r1:(addSpace (r2:rs))
          addSpace (r:rs) = r : (addSpace rs)

      leftmost <$> mapM f (annTextPara)

  divClass "" $ do
    let ev = leftmost vIdEv
    detailsEv <- getWebSocketResponse $ GetVocabDetails
      <$> fmap fst ev
    surfDyn <- holdDyn "" (fmap snd ev)
    showVocabDetailsWidget (attachDyn surfDyn detailsEv)
  return ()

showVocabDetailsWidget :: (AppMonad t m)
  => Event t (Text, [(Entry, Maybe SrsEntryId)])
  -> AppMonadT t m ()
showVocabDetailsWidget detailsEv = do
  let
    showEntry surface (e, sId) = do
      divClass "" $ do
        elClass "span" "" $ do
          entryKanjiAndReading e
        elClass "span" "" $ do
          text $ "Entry other properties"
        addEditSrsEntryWidget (Right $ e ^. entryUniqueId) (Just surface) sId

      divClass "" $ do
        let
          showGloss m = divClass "" $ text $ "-> " <> m
        mapM showGloss $ take 5 $
          e ^.. entrySenses . traverse . senseGlosses . traverse . glossDefinition


    wrapper :: (_) => m a -> m (Event t ())
    wrapper m = divClass "nav navbar-fixed-bottom" $
      divClass "container-fluid" $
        elAttr "div" (("class" =: "panel panel-default")
          <> ("style" =: "max-height: 200px;\
                         \overflow-y: auto;")) $ do
          (e,_) <- elClass' "button" "close" $ text "Close"
          m
          return $ domEvent Click e

    wd :: AppMonad t m
      => Maybe _
      -> AppMonadT t m (Event t ())
    wd (Just (s,es)) = wrapper
      (mapM_ (showEntry s) (orderEntries (fst) es))
    wd Nothing = return never

  rec
    let ev = leftmost [Just <$> detailsEv
             , Nothing <$ (switchPromptlyDyn closeEv)]
    closeEv <- widgetHold (return never)
      (wd <$> ev)

  return ()

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
orderEntries :: (a -> Entry) -> [a] -> [a]
orderEntries g es = sortBy (comparing (f . g)) es
  where
    f e
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
  ((e ^.. entryKanjiElements . traverse . to (Left)) ++
  readingWithoutRes)

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
