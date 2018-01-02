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
import SrsDB
import Control.Lens
import Handler.WebSocketHandler.Utils
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as V
import Text.Pretty.Simple
import Data.SearchEngine
import qualified Data.BTree.Impure as Tree
import NLP.Japanese.Utils
import Control.Monad.State hiding (foldM)
import Data.BTree.Alloc (AllocM, AllocReaderM)
import Mecab

searchResultCount = 20

mostUsedKanjis kanjiDb = take 1000 $ map _kanjiId
  $ sortBy (comparing f) $ map _kanjiDetails
  $ Map.elems kanjiDb
  where f k = case (_kanjiMostUsedRank k) of
          (Just r) -> r
          Nothing -> Rank $ 1000000

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
    kanjiFullSet = Map.keysSet kanjiDb
    fun :: [KanjiId]
    fun
      | (T.null inpTxt) && (T.null filtTxt)
        && (null rads) =
        mostUsedKanjis kanjiDb

      | (T.null inpTxt) && (T.null filtTxt) =
        Set.toList
          $ foldl Set.intersection kanjiFullSet
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
          (foldl Set.intersection kanjiFullSet
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
    validRadicals = Set.toList $ Set.unions
      (map _kanjiRadicalSet kanjisFiltered)

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
  let kd = Map.lookup kId kanjiDb

      keys = maybe [] (Set.toList . _kanjiVocabSet) kd

  uId <- asks currentUserId
  rs <- lift $ transactReadOnlySrsDB $ \db ->
    Tree.lookupTree uId (db ^. userData) >>= mapM (\rd ->
      Tree.lookupTree kId (rd ^. kanjiSrsMap))

  vs <- loadVocabList (take searchResultCount keys)

  asks kanjiVocabResult >>= \ref ->
    liftIO $ writeIORef ref (keys, searchResultCount)
  return $ KanjiSelectionDetails <$> kd ^? _Just . kanjiDetails
    <*> rs <*> vs

loadVocabList keys = do
  vocabDb <- lift $ asks appVocabDb
  let
      vocabs = map _vocabDetails $
        catMaybes ((flip Map.lookup) vocabDb <$> keys)

  uId <- asks currentUserId
  s <- lift $ transactReadOnlySrsDB $ \db ->
    Tree.lookupTree uId (db ^. userData) >>= mapM (\rd -> do
      mapM ((flip Tree.lookupTree) (rd ^. vocabSrsMap)) keys)
  return $ zip vocabs <$> s

getLoadMoreKanjiVocab :: LoadMoreKanjiVocab -> WsHandlerM VocabList
getLoadMoreKanjiVocab _ = do
  vocabDb <- lift $ asks appVocabDb

  (keys, count) <- asks kanjiVocabResult >>= \ref ->
    liftIO $ readIORef ref

  vs <- loadVocabList (take searchResultCount
                       $ drop count keys)

  asks kanjiVocabResult >>= \ref ->
    liftIO $ writeIORef ref (keys, count + (length vs))
  return $ maybe [] id vs

getVocabSearch :: VocabSearch -> WsHandlerM VocabList
getVocabSearch (VocabSearch m filt) = do
  vocabDb <- lift $ asks appVocabDb
  vocabSearchEng <- lift $ asks appVocabSearchEng
  let
      -- keys1 = map snd expl
      -- expl = queryExplain vocabSearchEng terms
      keys1 = query vocabSearchEng terms
      terms = (T.words m)

  let
      es = map _vocabEntry $
        catMaybes ((flip Map.lookup) vocabDb <$> keys1)
      allVs = filter (filterPOS filt) es
      keys = map _entryUniqueId allVs
      vs = (take searchResultCount allVs)

  vs <- loadVocabList (take searchResultCount keys)
  asks vocabSearchResult >>= \ref ->
    liftIO $ writeIORef ref (keys, searchResultCount)
  return $ maybe [] id vs

filterPOS :: Maybe PartOfSpeech -> Entry -> Bool
filterPOS Nothing _ = True
filterPOS (Just pf) e = any f ePs
  where
    ePs = e ^.. entrySenses . traverse . sensePartOfSpeech . traverse
    f p = case pf of
      (PosNoun) -> case p of
        (PosNoun) -> True
        (PosNounType _) -> True
        (PosPronoun) -> True
        _ -> False

      (PosVerb _ _) -> case p of
        (PosVerb _ _) -> True
        _ -> False

      (PosAdjective _) -> case p of
        (PosAdverb _) -> True
        (PosAdjective _) -> True
        (PosAdverb _) -> True
        _ -> False

      _ -> True

getLoadMoreVocabSearchResult :: LoadMoreVocabSearchResult -> WsHandlerM VocabList
getLoadMoreVocabSearchResult _ = do
  vocabDb <- lift $ asks appVocabDb

  (keys, count) <- asks vocabSearchResult >>= \ref ->
    liftIO $ readIORef ref

  vs <- loadVocabList (take searchResultCount
                       $ drop count keys)
  asks vocabSearchResult >>= \ref ->
    liftIO $ writeIORef ref (keys, count + (length vs))
  return $ maybe [] id vs

makeReaderDocumentContent t = do
  vocabDb <- asks appVocabDb
  mec <- asks appMecabPtr
  se <- asks appVocabSearchEngNoGloss
  liftIO $ parseAndSearch vocabDb se mec t

getAddOrEditDocument :: AddOrEditDocument
  -> WsHandlerM (Maybe (ReaderDocumentData))
getAddOrEditDocument (AddOrEditDocument dId title t) = do
  uId <- asks currentUserId
  c <- lift $ makeReaderDocumentContent t

  let
    upFun :: (AllocM m)
      => AppUserDataTree CurrentDb
      -> StateT (Maybe ReaderDocumentData) m
           (AppUserDataTree CurrentDb)
    upFun rd = do
      mx <- lift $
        Tree.lookupMaxTree (rd ^. readerDocuments)
      let newk = maybe zerokey nextKey mx
          zerokey = ReaderDocumentId 0
          nextKey (ReaderDocumentId k, _)
            = ReaderDocumentId (k + 1)

          docId = maybe newk id dId
          nd = ReaderDocument docId title c (0,Nothing)

      put $ Just (getReaderDocumentData nd Nothing)
      lift $ rd &
        readerDocuments %%~ Tree.insertTree docId nd

  lift $ transactSrsDB $ runStateWithNothing $
    userData %%~ updateTreeLiftedM uId upFun

getQuickAnalyzeText :: QuickAnalyzeText
  -> WsHandlerM [(Int, AnnotatedPara)]
getQuickAnalyzeText (QuickAnalyzeText t) = do
  v <- lift $ makeReaderDocumentContent t
  return $ zip [1..] $ V.toList v

getListDocuments :: ListDocuments
  -> WsHandlerM [(ReaderDocumentId, Text, Text)]
getListDocuments _ = do
  uId <- asks currentUserId

  ds <- lift $ transactReadOnlySrsDB $ \db -> do
    rd <- Tree.lookupTree uId $ db ^. userData
    mapM Tree.toList (rd ^? _Just . readerDocuments)

  let f (ReaderDocument i t c _) = (i,t,p c)
      p c = T.take 50 (foldl' fol "" c)
      fol t ap = t <> (mconcat $ map getT ap)
      getT (Left t) = t
      getT (Right (v,_,_)) = vocabToText v

  return $ maybe [] (map (f . snd)) ds

getViewDocument :: ViewDocument
  -> WsHandlerM (Maybe (ReaderDocumentData))
getViewDocument (ViewDocument i paraNum) = do
  uId <- asks currentUserId
  s <- lift $ transactReadOnlySrsDB $ \db ->
    Tree.lookupTree uId (db ^. userData)
      >>= mapM (\rd ->
        Tree.lookupTree i (rd ^. readerDocuments))
  return $ flip getReaderDocumentData paraNum <$> join s

getReaderDocumentData r paraNum = (r ^. readerDocId, r ^. readerDocTitle
                                  , p, slice)
  where
    slice = zip [startI..] $ V.toList $ V.slice startI len $ r ^. readerDocContent
    len = min (totalLen - startI) 30
    totalLen = (V.length $ r ^. readerDocContent)
    startI = maybe (max 0 (fst p - 10)) (\p -> min p (totalLen - 1)) paraNum
    p = r ^. readerDocProgress

getViewRawDocument :: ViewRawDocument
  -> WsHandlerM (Maybe (ReaderDocumentId, Text, Text))
getViewRawDocument (ViewRawDocument i) = do
  uId <- asks currentUserId
  s <- lift $ transactReadOnlySrsDB $ \db ->
    Tree.lookupTree uId (db ^. userData)
      >>= mapM (\rd ->
        Tree.lookupTree i (rd ^. readerDocuments))
  let combineToText d = T.unlines $ map getText $ V.toList d
      getText ap = mconcat $ map getT ap
      getT (Left t) = t
      getT (Right (v,_,_)) = vocabToText v
  return $ (\r -> (i, r ^. readerDocTitle
                  , r ^. readerDocContent . to (combineToText)))
                  <$> (join s)

getDeleteDocument :: DeleteDocument
  -> WsHandlerM [(ReaderDocumentId, Text, Text)]
getDeleteDocument (DeleteDocument dId) = do
  uId <- asks currentUserId
  lift $ transactSrsDB_ $
    userData %%~ updateTreeM uId
      (readerDocuments %%~ Tree.deleteTree dId)
  getListDocuments ListDocuments

getReaderSettings :: GetReaderSettings
  -> WsHandlerM (ReaderSettings CurrentDb)
getReaderSettings _ = do
  uId <- asks currentUserId
  s <- lift $ transactReadOnlySrsDB $ \db ->
    Tree.lookupTree uId (db ^. userData)
      >>= (\x -> return (x ^? _Just . readerSettings))
  return $ maybe def id s

saveReaderSettings :: SaveReaderSettings
  -> WsHandlerM ()
saveReaderSettings (SaveReaderSettings rs) = do
  uId <- asks currentUserId
  lift $ transactSrsDB_ $
    userData %%~ updateTreeM uId
      (readerSettings %%~ (const (return rs)))

saveReadingProgress :: SaveReadingProgress
  -> WsHandlerM ()
saveReadingProgress (SaveReadingProgress docId p) = do
  uId <- asks currentUserId
  lift $ transactSrsDB_ $
    userData %%~ updateTreeM uId
      (readerDocuments %%~ updateTreeM docId
        (readerDocProgress %%~ (const (return p))))

getVocabDetails :: GetVocabDetails
  -> WsHandlerM [(Entry, Maybe SrsEntryId)]
getVocabDetails (GetVocabDetails eIds) = do
  uId <- asks currentUserId
  vocabDb <- lift $ asks appVocabDb
  es <- lift $ transactReadOnlySrsDB $ \db -> do
    rd <- Tree.lookupTree uId $ db ^. userData
    let findAll vmap = do
          mapM (findOne vmap) eIds
        findOne vmap eId = do
          srsId <- Tree.lookupTree eId vmap
          let e = _vocabEntry <$> Map.lookup eId vocabDb
          return $ (,) <$> e <*> (pure srsId)
    mapM findAll (_vocabSrsMap <$> rd)
  return $ maybe [] catMaybes es
