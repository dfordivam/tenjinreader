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

searchResultCount = 20

mostUsedKanjis kanjiDb = take 20 $ map _kanjiId $ sortWith _kanjiMostUsedRank
  $ map _kanjiDetails $ Map.elems kanjiDb

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
        mostUsedKanjis kanjiDb

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


  let
      kanjisFilteredIds = fun

  asks kanjiSearchResult >>= \ref ->
    liftIO $ writeIORef ref (kanjisFilteredIds, searchResultCount)

  let
    kanjiList = take searchResultCount $ getKanjiList kanjiDb kanjisFilteredIds

    kanjisFiltered = catMaybes $
        (flip Map.lookup) kanjiDb <$> kanjisFilteredIds
    validRadicals = Set.toList $ foldl Set.intersection
      (Map.keysSet radicalDb) (map _kanjiRadicalSet kanjisFiltered)

  return $ KanjiFilterResult kanjiList validRadicals

getKanjiList :: KanjiDb -> [KanjiId] -> KanjiList
getKanjiList kanjiDb ks = map convertKanji $ map _kanjiDetails kanjisFiltered
  where
    kanjisFiltered = catMaybes $
        (flip Map.lookup) kanjiDb <$> ks
    convertKanji
      :: KanjiDetails
      -> (KanjiId, Kanji, Maybe Rank, [Meaning])
    convertKanji k =
      (k ^. kanjiId, k ^. kanjiCharacter
      , k ^. kanjiMostUsedRank, k ^. kanjiMeanings)

getLoadMoreKanjiResults :: LoadMoreKanjiResults -> WsHandlerM KanjiList
getLoadMoreKanjiResults _ = do
  kanjiDb <- lift $ asks appKanjiDb
  (ks, count) <- asks kanjiSearchResult >>= \ref ->
    liftIO $ readIORef ref
  let
    kanjiList = take searchResultCount $ drop count $ getKanjiList kanjiDb ks

  asks kanjiSearchResult >>= \ref ->
    liftIO $ writeIORef ref (ks, count + (length kanjiList))

  return kanjiList

getKanjiDetails :: GetKanjiDetails -> WsHandlerM (Maybe KanjiSelectionDetails)
getKanjiDetails (GetKanjiDetails kId _) = do
  kanjiDb <- lift $ asks appKanjiDb
  vocabDb <- lift $ asks appVocabDb
  let kd = Map.lookup kId kanjiDb

      vocabs = map _vocabDetails $
        catMaybes ((flip Map.lookup) vocabDb <$> keys)
      keys = maybe [] (Set.toList . _kanjiVocabSet) kd

      vs = take searchResultCount vocabs

  asks kanjiVocabResult >>= \ref ->
    liftIO $ writeIORef ref (keys, searchResultCount)
  return $ KanjiSelectionDetails <$> kd ^? _Just . kanjiDetails <*> pure vs

getLoadMoreKanjiVocab :: LoadMoreKanjiVocab -> WsHandlerM [VocabDetails]
getLoadMoreKanjiVocab _ = do
  vocabDb <- lift $ asks appVocabDb

  (keys, count) <- asks kanjiVocabResult >>= \ref ->
    liftIO $ readIORef ref
  let
      vocabs = map _vocabDetails $
        catMaybes ((flip Map.lookup) vocabDb <$> keys)
      vs = take searchResultCount $ drop count vocabs

  asks kanjiVocabResult >>= \ref ->
    liftIO $ writeIORef ref (keys, count + (length vs))
  return vs

getVocabSearch :: VocabSearch -> WsHandlerM [VocabDetails]
getVocabSearch (VocabSearch (AdditionalFilter r _ m)) = do
  vocabDb <- lift $ asks appVocabDb
  vocabSearchEng <- lift $ asks appVocabSearchEng
  let vocabs = map _vocabDetails $
        catMaybes ((flip Map.lookup) vocabDb <$> keys)
      keys = query vocabSearchEng terms
      terms = (T.words m)
  asks vocabSearchResult >>= \ref ->
    liftIO $ writeIORef ref (keys, searchResultCount)
  return $ take searchResultCount vocabs

getLoadMoreVocabSearchResult :: LoadMoreVocabSearchResult -> WsHandlerM [VocabDetails]
getLoadMoreVocabSearchResult _ = do
  vocabDb <- lift $ asks appVocabDb

  (keys, count) <- asks vocabSearchResult >>= \ref ->
    liftIO $ readIORef ref
  let
      vocabs = map _vocabDetails $
        catMaybes ((flip Map.lookup) vocabDb <$> keys)
      vs = take searchResultCount $ drop count vocabs

  asks vocabSearchResult >>= \ref ->
    liftIO $ writeIORef ref (keys, count + (length vs))
  return vs
