{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Handler.WebSocketHandler.KanjiBrowser where

import Import
import Protolude (foldl)
import Control.Lens
import Handler.WebSocketHandler.Utils
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.Pretty.Simple
import Data.SearchEngine

mostUsedKanjis = undefined

-- Pagination,
getKanjiFilterResult :: KanjiFilter -> WsHandlerM KanjiFilterResult
getKanjiFilterResult (KanjiFilter inpTxt (AdditionalFilter filtTxt filtType _) rads) = do
  kanjiDb <- lift $ asks appKanjiDb
  vocabDb <- lift $ asks appVocabDb
  radicalDb <- lift $ asks appRadicalDb
  kanjiSearchEng <- lift $ asks appKanjiSearchEng
  let uniqKanji = ordNub $ getKanjis inpTxt

  -- inpTxt in empty
  -- but filt and rads are non empty

  -- Filt, rads, inpText empty, then return most used kanji list
  -- Preferable sequence of search is
  -- 1. Input text - This will limit the number of kanji to the length of text
  -- 2. Reading filter
  -- 3. Radicals
  let
    fun :: [KanjiId]
    fun
      | (T.null inpTxt) && (T.null filtTxt)
        && (null rads) =
        mostUsedKanjis

      | (T.null inpTxt) && (T.null filtTxt) =
        Set.toList
          $ foldl Set.intersection Set.empty
          $ catMaybes $ map (flip Map.lookup radicalDb) rads

      | (T.null inpTxt) && (null rads) =
        query kanjiSearchEng (T.words filtTxt)

      | (T.null filtTxt) && (null rads) =
        query kanjiSearchEng uniqKanji

      | (null rads) =
        Set.toList $ Set.intersection
          (Set.fromList $ query kanjiSearchEng uniqKanji)
          (Set.fromList $ query kanjiSearchEng (T.words filtTxt))

      | (T.null filtTxt) = do
        Set.toList $ Set.intersection
          (Set.fromList $ query kanjiSearchEng uniqKanji)
          (foldl Set.intersection Set.empty
            $ catMaybes $ map (flip Map.lookup radicalDb) rads)

      | otherwise = do
        Set.toList $ Set.intersection
          (Set.fromList $ query kanjiSearchEng uniqKanji)
          (foldl Set.intersection
            (Set.fromList $ query kanjiSearchEng (T.words filtTxt))
            $ catMaybes $ map (flip Map.lookup radicalDb) rads)


  let kanjisFiltered = catMaybes $
        (flip Map.lookup) kanjiDb <$> kanjisFilteredIds
      kanjisFilteredIds = fun

  let kanjiList = map convertKanji $ map _kanjiDetails kanjisFiltered
      convertKanji
        :: KanjiDetails
        -> (KanjiId, Kanji, Maybe Rank, [Meaning])
      convertKanji k =
        (k ^. kanjiId, k ^. kanjiCharacter
        , k ^. kanjiMostUsedRank, k ^. kanjiMeanings)
      validRadicals = Set.toList $ foldl Set.intersection
        (Map.keysSet radicalDb) (map _kanjiRadicalSet kanjisFiltered)

  return $ KanjiFilterResult kanjiList validRadicals

getLoadMoreKanjiResults :: LoadMoreKanjiResults -> WsHandlerM KanjiFilterResult
getLoadMoreKanjiResults = undefined

getKanjiDetails :: GetKanjiDetails -> WsHandlerM (Maybe KanjiSelectionDetails)
getKanjiDetails (GetKanjiDetails kId _) = do
  kanjiDb <- lift $ asks appKanjiDb
  vocabDb <- lift $ asks appVocabDb
  let kd = Map.lookup kId kanjiDb

      vocabs = map _vocabDetails $
        catMaybes ((flip Map.lookup) vocabDb <$> keys)
      keys = maybe [] (Set.toList . _kanjiVocabSet) kd

  return $ KanjiSelectionDetails <$> kd ^? _Just . kanjiDetails <*> pure vocabs

getVocabSearch :: VocabSearch -> WsHandlerM [VocabDetails]
getVocabSearch (VocabSearch (AdditionalFilter r _ m)) = do
  vocabDb <- lift $ asks appVocabDb
  vocabSearchEng <- lift $ asks appVocabSearchEng
  let vocabs = map _vocabDetails $
        catMaybes ((flip Map.lookup) vocabDb <$> keys)
      keys = query vocabSearchEng terms
      terms = (T.words m)
  return $ vocabs
