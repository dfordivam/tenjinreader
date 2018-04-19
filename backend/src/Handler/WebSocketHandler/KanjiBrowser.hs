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
import qualified Data.List as List

import Data.SearchEngine
import qualified Data.BTree.Impure as Tree
import NLP.Japanese.Utils
import Control.Monad.State hiding (foldM, ap, forM_)
import Data.BTree.Alloc (AllocM, AllocReaderM)
import qualified Data.Array as A
import Data.Array (Array)
import Mecab
import Radicals
import System.Random

searchResultCount = 20

mostUsedKanjis :: Array i KanjiData -> [KanjiId]
mostUsedKanjis kanjiDb = take 1000 $ map _kanjiId
  $ sortBy (comparing f) $ map _kanjiDetails
  $ A.elems kanjiDb
  where f k = case (_kanjiMostUsedRank k) of
          (Just r) -> r
          Nothing -> Rank $ 1000000

-- Pagination,
getKanjiFilterResult :: KanjiFilter -> WsHandlerM KanjiFilterResult
getKanjiFilterResult (KanjiFilter inpTxt (AdditionalFilter filtTxt _ _) rads) = do
  kanjiDb <- lift $ asks appKanjiDb
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
    kanjiFullSet = Set.fromList $ A.indices kanjiDb
    fun :: [KanjiId]
    fun
      | (T.null inpTxt) && (T.null filtTxt)
        && (null rads) =
        mostUsedKanjis kanjiDb

      | (T.null inpTxt) && (T.null filtTxt) =
        Set.toList
          $ foldl Set.intersection kanjiFullSet
          $ arrayLookup radicalDb rads

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
            $ arrayLookup radicalDb rads)

      | otherwise = do
        Set.toList $ Set.intersection
          (Set.fromList $ query kanjiSearchEng uniqKanji)
          (foldl Set.intersection
            (Set.fromList $ query kanjiSearchEng (T.words filtTxt))
            $ arrayLookup radicalDb rads)


  let
      kanjisFilteredIds = fun

  asks kanjiSearchResult >>= \ref ->
    liftIO $ writeIORef ref (kanjisFilteredIds, searchResultCount)

  let
    kanjiList = take searchResultCount $ getKanjiList kanjiDb kanjisFilteredIds

    kanjisFiltered = arrayLookup kanjiDb kanjisFilteredIds
    validRadicals = Set.toList $ Set.unions
      (map _kanjiRadicalSet kanjisFiltered)

  return $ KanjiFilterResult kanjiList validRadicals

getKanjiList :: KanjiDb -> [KanjiId] -> KanjiList
getKanjiList kanjiDb ks = map convertKanji $ map _kanjiDetails kanjisFiltered
  where
    kanjisFiltered = arrayLookup kanjiDb ks
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
  let kd = arrayLookupMaybe kanjiDb kId
      rads = maybe [] ((map (\(RadicalDetails t _) -> t)) . catMaybes
                        . (map (\i -> Map.lookup i radicalTable))
          . Set.toList . _kanjiRadicalSet) kd

      keys = maybe [] (Set.toList . _kanjiVocabSet) kd

  rs <- transactReadOnlySrsDB $ \rd ->
    getVocabSrsState <$> Tree.lookupTree kId (rd ^. kanjiSrsMap)

  vs <- loadVocabList (take searchResultCount keys)

  asks kanjiVocabResult >>= \ref ->
    liftIO $ writeIORef ref (keys, searchResultCount)
  return $ KanjiSelectionDetails <$> ((,,) <$> kd ^? _Just . kanjiDetails
    <*> pure rs <*> pure rads) <*> pure vs

loadVocabList
  :: [VocabId]
  -> ReaderT WsHandlerEnv Handler [(Entry, VocabSrsState)]
loadVocabList keys = do
  vocabDb <- lift $ asks appVocabDb
  let
      vocabs = map _vocabEntry $ arrayLookup vocabDb keys

  s <- transactReadOnlySrsDB $ \rd -> do
      (map getVocabSrsState)
        <$> mapM ((flip Tree.lookupTree) (rd ^. vocabSrsMap)) keys
  return $ zip vocabs s

getLoadMoreKanjiVocab :: LoadMoreKanjiVocab -> WsHandlerM VocabList
getLoadMoreKanjiVocab _ =
  getLoadMoreVocabSearchResult LoadMoreVocabSearchResult

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
      es = map _vocabEntry $ arrayLookup vocabDb keys1
      allVs = filter (filterPOS filt) es
      keys = map _entryUniqueId allVs

  vs <- loadVocabList (take searchResultCount keys)
  asks vocabSearchResult >>= \ref ->
    liftIO $ writeIORef ref (keys, searchResultCount)
  return $ vs

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
        _ -> False

      _ -> True

getLoadMoreVocabSearchResult :: LoadMoreVocabSearchResult -> WsHandlerM VocabList
getLoadMoreVocabSearchResult _ = do
  (vIds, len) <- asks vocabSearchResult >>= \ref ->
    liftIO $ readIORef ref

  vs <- loadVocabList (take searchResultCount
                       $ drop len vIds)
  asks vocabSearchResult >>= \ref ->
    liftIO $ writeIORef ref (vIds, len + (length vs))
  return $ vs

makeReaderDocumentContent
  :: (MonadIO m, MonadReader App m)
  => Text -> m AnnotatedDocument
makeReaderDocumentContent t = do
  vocabDb <- asks appVocabDb
  mec <- asks appMecabPtr
  se <- asks appVocabSearchEngNoGloss
  liftIO $ parseAndSearch vocabDb se mec t

getAddOrEditDocument :: AddOrEditDocument
  -> WsHandlerM (Maybe (ReaderDocumentData))
getAddOrEditDocument (AddOrEditDocument dId title t) = do
  limitReached <- transactReadOnlySrsDB $ \rd -> do
    ds <- Tree.toList (rd ^. readerDocuments)
    let f (MyDocument _ c) = sum $ map length c
        f _ = 0
        al = traceShowId ((length t) + (sum $ map (f . _readerDoc . snd) ds))
    return $ (length ds > 50)
      || (al > 500000)

  if limitReached
    then return Nothing
    else do
      c <- lift $ makeReaderDocumentContent t
      addEditNewDocumentCommon dId (MyDocument title c)

addEditNewDocumentCommon
  :: Maybe ReaderDocumentId
  -> ReaderDocumentTypeCurrent
  -> ReaderT WsHandlerEnv Handler (Maybe ReaderDocumentData)
addEditNewDocumentCommon dId docContent = do
  booksDb <- lift $ asks appBooksDb
  articlesDb <- lift $ asks appArticlesDb
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
          nd = ReaderDocument docId docContent (0,Nothing)

      put $ Just (getReaderDocumentData booksDb articlesDb nd Nothing)
      lift $ rd &
        readerDocuments %%~ Tree.insertTree docId nd
        >>= documentAccessOrder %%~ (\ol -> return $ newk : (List.delete newk ol))

  ret <- transactSrsDB $ runStateWithNothing $ upFun

  transactReadOnlySrsDB $ \rd -> do
    let
      f :: (AllocReaderM m)
        => (Vocab, [VocabId], Bool)
        -> m (Vocab, [VocabId], Bool)
      f v@(_,vIds,_) = do
        sIds <- mapM (\vi -> Tree.lookupTree vi (rd ^. vocabSrsMap)) vIds
        return $ (v & _3 .~ (null $ catMaybes sIds)) -- Show if not in srs map
    ret & _Just . _5 . each . _2 . each . _Right %%~ f

getQuickAnalyzeText :: QuickAnalyzeText
  -> WsHandlerM [(Int, AnnotatedPara)]
getQuickAnalyzeText (QuickAnalyzeText t) = do
  v <- lift $ makeReaderDocumentContent t
  return $ zip [1..] $ V.toList v

getListDocuments :: ListDocuments
  -> WsHandlerM [(ReaderDocumentId, Text, Text)]
getListDocuments _ = do
  booksDb <- lift $ asks appBooksDb
  articlesDb <- lift $ asks appArticlesDb
  ds <- transactReadOnlySrsDB $ \rd -> do
    let dl = take 20 $ rd ^. documentAccessOrder
    catMaybes <$> mapM (\i -> Tree.lookupTree i (rd ^. readerDocuments)) dl

  let
    f (ReaderDocument i (MyDocument t c) _) = Just (i,t, getAnnDocContent c)
    f (ReaderDocument i (Book bId) _) = (\(t,c) -> (i,t, getAnnDocContent c)) <$>
      (arrayLookupMaybe booksDb bId)
    f (ReaderDocument i (Article aId) _) = (\(t,c) -> (i,t, getAnnDocContent c)) <$>
      (arrayLookupMaybe articlesDb aId)
  return $ catMaybes $ map f ds

getAnnDocContent :: AnnotatedDocument -> Text
getAnnDocContent c = T.take 50 (foldl' fol "" c)
  where
      fol t ap = t <> (mconcat $ map getT ap)
      getT (Left t) = t
      getT (Right (v,_,_)) = vocabToText v

getListBooks :: ListBooks
  -> WsHandlerM [(BookId, Text, Text)]
getListBooks _ = do
  booksDb <- lift $ asks appBooksDb
  let
    xs = A.assocs booksDb
  return (map (\(i,(t,c)) -> (i,t, getAnnDocContent c)) xs)


getListArticles :: ListArticles
  -> WsHandlerM [(ArticleId, Text, Text)]
getListArticles _ = do
  db <- lift $ asks appArticlesDb
  let
    xs = take 20 $ A.assocs db
  return (map (\(i,(t,c)) -> (i,t, getAnnDocContent c)) xs)

getViewDocument :: ViewDocument
  -> WsHandlerM (Maybe (ReaderDocumentData))
getViewDocument (ViewDocument i paraNum) = do
  booksDb <- lift $ asks appBooksDb
  articlesDb <- lift $ asks appArticlesDb

  s <- transactReadOnlySrsDB $ \rd ->
    Tree.lookupTree i (rd ^. readerDocuments)

  forM_ s $ \_ -> transactSrsDB_ $
    documentAccessOrder %%~ (\ol -> return $ i : (List.delete i ol))

  let
    ret :: Maybe ReaderDocumentData
    ret = flip (getReaderDocumentData booksDb articlesDb) paraNum <$> s

  transactReadOnlySrsDB $ \rd -> do
    let
      f :: (AllocReaderM m)
        => (Vocab, [VocabId], Bool)
        -> m (Vocab, [VocabId], Bool)
      f v@(_,vIds,_) = do
        sIds <- mapM (\vi -> Tree.lookupTree vi (rd ^. vocabSrsMap)) vIds
        return $ (v & _3 .~ (null $ catMaybes sIds)) -- Show if not in srs map
    ret & _Just . _5 . each . _2 . each . _Right %%~ f

getViewDocument req = do
  rs <- transactReadOnlySrsDB $ \rd ->
    Tree.toList (rd ^. readerDocuments)
  let
    c = case req of
      (ViewBook i) -> Book i
      (ViewArticle i) -> Article i
      _ -> error "Cannot happen"
    f (_,(ReaderDocument _ d _)) = d == c

  case (headMay $ filter f rs) of
    (Just r) ->
      getViewDocument (ViewDocument (fst r) Nothing)
    Nothing ->
      addEditNewDocumentCommon Nothing c


getReaderDocumentData
  :: Array BookId (Text, AnnotatedDocument)
  -> Array ArticleId (Text, AnnotatedDocument)
  -> ReaderDocumentTree t0
  -> Maybe Int
  -> (ReaderDocumentId, Text, (Int, Maybe Int), Int,
       [(Int, AnnotatedPara)])
getReaderDocumentData booksDb articlesDb r paraNum =
  (r ^. readerDocId, title, pg, endParaNum, slice)
  where
    (title, c) = case r ^. readerDoc of
      (MyDocument t c') -> (t,c')
      (Book i) -> maybe empVec id $ arrayLookupMaybe booksDb i
      (Article i) -> maybe empVec id $ arrayLookupMaybe articlesDb i
    empVec :: (Text, AnnotatedDocument)
    empVec = ("", V.singleton [Left "error"])
    slice = zip [startI..] $ V.toList $ V.slice startI len $ c
    len = min (totalLen - startI) 60
    totalLen = (V.length $ c)
    startI = maybe (max 0 (fst pg - 20)) (\p -> min p (totalLen - 1)) paraNum
    pg = r ^. readerDocProgress
    endParaNum = totalLen - 1

getViewRawDocument :: ViewRawDocument
  -> WsHandlerM (Maybe (ReaderDocumentId, Text, Text))
getViewRawDocument (ViewRawDocument i) = do
  s <- transactReadOnlySrsDB $ \rd ->
    Tree.lookupTree i (rd ^. readerDocuments)
  let combineToText d = T.unlines $ map getText $ V.toList d
      getText ap = mconcat $ map getT ap
      getT (Left t) = t
      getT (Right (v,_,_)) = vocabToText v
      f (MyDocument t c) = Just (i, t, combineToText c)
      f _ = Nothing
  return $ f . _readerDoc =<< s

getDeleteDocument :: DeleteDocument
  -> WsHandlerM [(ReaderDocumentId, Text, Text)]
getDeleteDocument (DeleteDocument dId) = do
  transactSrsDB_ $ (readerDocuments %%~ Tree.deleteTree dId)
  getListDocuments (ListDocuments Nothing)

getReaderSettings :: GetReaderSettings
  -> WsHandlerM (ReaderSettings CurrentDb)
getReaderSettings _ = do
  transactReadOnlySrsDB $ (\rd -> return $ rd ^. readerSettings)

saveReaderSettings :: SaveReaderSettings
  -> WsHandlerM ()
saveReaderSettings (SaveReaderSettings rs) = do
  transactSrsDB_ $ (readerSettings %%~ (const (return rs)))

saveReadingProgress :: SaveReadingProgress
  -> WsHandlerM ()
saveReadingProgress (SaveReadingProgress docId p) = do
  transactSrsDB_ $ (readerDocuments %%~ updateTreeM docId
                    (readerDocProgress %%~ (const (return p))))

getVocabDetails :: GetVocabDetails
  -> WsHandlerM [(Entry, VocabSrsState)]
getVocabDetails (GetVocabDetails eIds) = do
  vocabDb <- lift $ asks appVocabDb
  es <- transactReadOnlySrsDB $ \rd -> do
    let findAll vmap = do
          mapM (findOne vmap) eIds
        findOne vmap eId = do
          srsId <- Tree.lookupTree eId vmap
          let e = _vocabEntry <$> arrayLookupMaybe vocabDb eId
          return $ (,) <$> e <*> (pure $ getVocabSrsState srsId)
    findAll (_vocabSrsMap rd)
  return $ catMaybes es

getRandomSentence :: GetRandomSentence
  -> WsHandlerM ((Bool, SentenceId), SentenceData)
getRandomSentence req = do
  sentenceDb <- lift $ asks appSentenceDb
  favSet <- transactReadOnlySrsDB (pure . view favouriteSentences)

  sId <- case (Set.null favSet, req) of
    (False, GetRandomFavSentence) -> do
      i <- liftIO $ randomRIO (0, Set.size favSet - 1)
      return $ Set.elemAt i favSet
    _ -> SentenceId <$> (liftIO $ randomRIO $ (A.bounds sentenceDb) & both %~ unSentenceId)

  let
    isFav = Set.member sId favSet
    g (i,b) = (,) <$> pure (not b, i) <*> arrayLookupMaybe sentenceDb i
    (Just val) = g (sId, isFav)
  return val

getVocabSentences :: GetVocabSentences
  -> WsHandlerM ([VocabId], [((Bool, SentenceId), SentenceData)])
getVocabSentences (GetVocabSentences svId) = do
  eId <- case svId of
    (Left eId) -> return $ Just eId
    (Right sId) -> transactReadOnlySrsDB $ \rd ->
      Tree.lookupTree sId (rd ^. srsKanjiVocabMap)
        >>= mapM (\e -> return $ either (const Nothing) Just e)
        >>= (\x -> return $ join x)

  ss <- getLoadMoreSentences (LoadMoreSentences (maybeToList eId) [])
  return (maybeToList eId, ss)

getLoadMoreSentences :: LoadMoreSentences
  -> WsHandlerM [((Bool, SentenceId), SentenceData)]
getLoadMoreSentences (LoadMoreSentences [] _) = return []
getLoadMoreSentences (LoadMoreSentences (eId:_) alreadySent) = do
  vocabSentenceDb <- lift $ asks appVocabSentenceDb
  sentenceDb <- lift $ asks appSentenceDb
  favSet <- transactReadOnlySrsDB (pure . view favouriteSentences)
  let
    ss = maybe [] (catMaybes . map g) (f <$> sSetMB)

    f allSet = take 10 $ (favs ++ others)
      where
        sSet = Set.difference allSet (Set.fromList alreadySent)
        favs = (\i -> (i,True)) <$> (Set.toList $ Set.intersection favSet sSet)
        others = (\i -> (i,False)) <$> (Set.toList $ Set.difference sSet favSet)

    g (sId,b) = (,) <$> pure (not b, sId) <*> arrayLookupMaybe sentenceDb sId
    sSetMB = Map.lookup eId vocabSentenceDb
  return (ss)

getToggleSentenceFav :: ToggleSentenceFav
  -> WsHandlerM ()
getToggleSentenceFav (ToggleSentenceFav sId) =
  transactSrsDB_ $
    favouriteSentences %%~
      (\s -> return $
             if Set.member sId s
               then Set.delete sId s
               else Set.insert sId s)
