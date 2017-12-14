{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Handler.WebSocketHandler.SrsReview where

import Import
import Control.Lens hiding (reviews)
import SrsDB
import Handler.WebSocketHandler.Utils
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Time.Clock
import Data.Time.Calendar
import Text.Pretty.Simple
import System.Random
import Data.Aeson

import qualified Data.BTree.Impure as Tree
import Control.Monad.Haskey
import Data.BTree.Alloc (AllocM, AllocReaderM)
import Database.Persist.Sql
import Data.These
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)

getSrsStats :: GetSrsStats -> WsHandlerM SrsStats
getSrsStats _ = do
  uId <- asks currentUserId
  -- TODO Fix this
  rs <- getAllPendingReviews ReviewTypeRecogReview
  allRs <- lift $ transactReadOnlySrsDB $ \db -> do
    rd <- Tree.lookupTree uId $ db ^. userReviews
    mapM Tree.toList (_reviews <$> rd)
  -- { reviewsToday :: Int
  -- , totalItems :: Int
  -- , totalReviews :: Int
  -- , averageSuccess :: Int
  let total = maybe 0 length allRs
      succ = sumOf (folded . _2 . reviewState . (to mergeSucF)) <$> allRs
      fail = sumOf (folded . _2 . reviewState . (to mergeFailF)) <$> allRs
      mergeSucF = mergeTheseWith getSucF getSucF (+)
      getSucF = (view (_2 . successCount))
      mergeFailF = mergeTheseWith getFailF getFailF (+)
      getFailF = (view (_2 . failureCount))
      totalR = maybe 0 id $ (+) <$> succ <*> fail
  return $ SrsStats (length rs) total totalR 0

-- ( _2 %%~ (here (\x -> print x))) (3, This 4)
-- view (_2 . (to (here pure ))) (3, These 4 7)

reviewStateL :: (Profunctor p, Contravariant f)
  => ReviewType
  -> Optic' p f SrsEntry (Maybe (SrsEntryState, SrsEntryStats))
reviewStateL ReviewTypeRecogReview
  = to (\r -> (r ^? reviewState . _This)
    <|> (r ^? reviewState . _These . _1))

reviewStateL ReviewTypeProdReview
  = to (\r -> (r ^? reviewState . _That)
    <|> (r ^? reviewState . _These . _2))

getAllPendingReviews
  :: ReviewType
  -> WsHandlerM [(SrsEntryId, SrsEntry)]
getAllPendingReviews rt = do
  uId <- asks currentUserId
  today <- liftIO $ utctDay <$> getCurrentTime

  lift $ transactReadOnlySrsDB $ \db -> do
    rd <- Tree.lookupTree uId $ db ^. userReviews
    rs <- mapM Tree.toList (_reviews <$> rd)
    let
      f (k,r) = (k,r) <$
        (r ^? (reviewStateL rt) . _Just . _1 . _NewReview)
        <|> (g =<< r ^? (reviewStateL rt) . _Just .
            _1 . _NextReviewDate)
        where g (d,_) = if d <= today
                then Just (k,r)
                else Nothing

    return $ maybe [] (mapMaybe f) rs

getBrowseSrsItems :: BrowseSrsItems -> WsHandlerM [SrsItem]
getBrowseSrsItems (BrowseSrsItems rt brws) = do
  uId <- asks currentUserId
  today <- liftIO $ utctDay <$> getCurrentTime

  let
    filF (k,r) = (k,r) <$ case brws of
      (BrowseDueItems l) -> g =<< r ^? (reviewStateL rt)
          . _Just . _1 . _NextReviewDate
        where g (d, interval) = if d <= today
                then checkInterval l interval
                else Nothing

      (BrowseNewItems) ->
        (r ^? (reviewStateL rt) . _Just . _1 . _NewReview)

      (BrowseSuspItems l) -> checkInterval l =<<
        (r ^? (reviewStateL rt) . _Just . _1 . _Suspended)

      (BrowseOtherItems l) -> g =<< r ^? (reviewStateL rt)
          . _Just . _1 . _NextReviewDate
        where g (d, interval) = if d > today
                then checkInterval l interval
                else Nothing

    checkInterval LearningLvl (SrsInterval l)=
      if l <= 4 then Just () else Nothing

    checkInterval IntermediateLvl (SrsInterval l)=
      if l > 4 && l <= 60 then Just () else Nothing

    checkInterval MatureLvl (SrsInterval l)=
      if l > 60 then Just () else Nothing

  rs <- lift $ transactReadOnlySrsDB $ \db -> do
    rd <- Tree.lookupTree uId $ db ^. userReviews
    rs <- mapM Tree.toList (_reviews <$> rd)
    return $ maybe [] (mapMaybe filF) rs
  return $ map (\(i,r) -> SrsItem i (r ^. field)) rs

getBulkEditSrsItems :: BulkEditSrsItems
  -> WsHandlerM (Maybe ())
getBulkEditSrsItems
  (BulkEditSrsItems _ ss DeleteSrsItems) = do
  return Nothing

getBulkEditSrsItems (BulkEditSrsItems rt ss op) = do
  today <- liftIO $ utctDay <$> getCurrentTime
  uId <- asks currentUserId

  let
    setL = case rt of
      ReviewTypeRecogReview -> here
      ReviewTypeProdReview -> there
    upF =
      case op of
        SuspendSrsItems ->
          reviewState . setL . _1 %~ \case
            NextReviewDate _ i -> Suspended i
            a -> a
        MarkDueSrsItems ->
          reviewState . setL . _1 %~ \case
            NextReviewDate _ i -> NextReviewDate today i
            Suspended i -> NextReviewDate today i
            a -> a
        ChangeSrsReviewData d ->
          reviewState . setL . _1 %~ \case
            NextReviewDate _ i -> NextReviewDate d i
            Suspended i -> NextReviewDate d i
            a -> a

    doUp :: (AllocM m) => Tree.Tree SrsEntryId SrsEntry
      -> SrsEntryId
      -> m (Tree.Tree SrsEntryId SrsEntry)
    doUp t rId = t & updateTreeM rId
      (\r -> return $ upF r)

  lift $ transactSrsDB_ $
    userReviews %%~ updateTreeM uId
      (reviews %%~ (\rt -> foldlM doUp rt ss))

  return Nothing

getSrsItem :: GetSrsItem
  -> WsHandlerM (Maybe SrsItemFull)
getSrsItem (GetSrsItem i) = do
  return Nothing

getEditSrsItem :: EditSrsItem
  -> WsHandlerM ()
getEditSrsItem (EditSrsItem sItm)= return ()

getGetNextReviewItem :: GetNextReviewItems
  -> WsHandlerM [ReviewItem]
getGetNextReviewItem (GetNextReviewItems rt alreadyPresent) = do
  rs <- getAllPendingReviews rt
  return $ getReviewItem <$> (take 20 rs)

getDoReview :: DoReview
  -> WsHandlerM Bool
getDoReview (DoReview rt results) = do
  today <- liftIO $ utctDay <$> getCurrentTime
  uId <- asks currentUserId

  let
    doUp :: (AllocM m) => Tree.Tree SrsEntryId SrsEntry
      -> (SrsEntryId, Bool)
      -> m (Tree.Tree SrsEntryId SrsEntry)
    doUp t (rId,b) = updateTreeM rId
        (\r -> return $ updateSrsEntry b today rt r) t

  lift $ transactSrsDB_ $
    userReviews %%~ updateTreeM uId
      (reviews %%~ (\rt -> foldlM doUp rt results))
  return True

updateSrsEntry :: Bool -> Day -> ReviewType -> SrsEntry -> SrsEntry
updateSrsEntry b today rt r = r
  & reviewState . setL . _1 %~ modifyState
  & reviewState . setL . _2 %~ modifyStats

  where
    setL = case rt of
      ReviewTypeRecogReview -> here
      ReviewTypeProdReview -> there
    modifyState (NextReviewDate d i) =
      getNextReviewDate today d i b
    modifyState (NewReview) =
      getNextReviewDate today today (SrsInterval 1) b
    modifyState s = s -- error

    modifyStats s = if b
      then s & successCount +~ 1
      else s & failureCount +~ 1

updateTreeM :: _
  => k -> (v -> m v) -> Tree.Tree k v -> m (Tree.Tree k v)
updateTreeM k fun tree = do
  Tree.lookupTree k tree
  >>= mapM fun
  >>= mapM (\v -> Tree.insertTree k v tree)
  >>= (\t -> return $ maybe tree id t)

getReviewItem
  :: (SrsEntryId, SrsEntry)
  -> ReviewItem
getReviewItem (i,s) =
  ReviewItem i (s ^. field) (m,mn) (r,rn)
  where
    m = (s ^. meaning)
    mn = (s ^. meaningNotes)
    r = (s ^. readings)
    rn = (s ^. readingNotes)


getNextReviewDate
  :: Day
  -> Day
  -> SrsInterval
  -> Bool
  -> SrsEntryState
getNextReviewDate
  today dueDate (SrsInterval lastInterval) success =
  NextReviewDate (addDays nextInterval today) (SrsInterval fullInterval)
  where extraDays = diffDays today dueDate
        fullInterval = lastInterval + extraDays
        fi = fromIntegral fullInterval
        nextInterval = ceiling $ if success
          then fi * 2.5
          else fi * 0.25

-- getNextReviewDate
--   :: Bool
--   -> UTCTime
--   -> Maybe UTCTime
--   -> Int
--   -> UTCTime
-- getNextReviewDate
--   success curTime revDate oldGrade =
--   let
--     addHour h = addUTCTime (h*60*60) curTime
--     addDay d = addUTCTime (d*24*60*60) curTime
--   in case (oldGrade, success) of
--     (0,_) -> addHour 4
--     (1,False) -> addHour 4

--     (2,False) -> addHour 8
--     (1,True) -> addHour 8

--     (2,True) -> addDay 1
--     (3,False) -> addDay 1

--     (3,True) -> addDay 3
--     (4,False) -> addDay 3

--     (4,True) -> addDay 7
--     (5,False) -> addDay 7

--     (5,True) -> addDay 14
--     (6,False) -> addDay 14

--     (6,True) -> addDay 30
--     (7,False) -> addDay 30

--     (7,True) -> addDay 120
--     _ -> curTime -- error

getCheckAnswer :: CheckAnswer -> WsHandlerM CheckAnswerResult
getCheckAnswer (CheckAnswer readings alt) = do
  liftIO $ pPrint alt
  let  f (c,t) = (,) <$> pure c <*> getKana t
  kanaAlts <- lift $ mapM (mapM f) alt
  liftIO $ pPrint kanaAlts
  return $ checkAnswerInt readings kanaAlts

checkAnswerInt :: [Reading] -> [[(Double, Text)]] -> CheckAnswerResult
checkAnswerInt readings kanaAlts =
  case any (\r -> elem r kanas) (unReading <$> readings) of
    True -> AnswerCorrect
    False -> AnswerIncorrect "T"
  where
    kanas = mconcat $ map (map snd) kanaAlts

getQuickAddSrsItem :: QuickAddSrsItem -> WsHandlerM (Maybe SrsEntryId)
getQuickAddSrsItem (QuickAddSrsItem v) = do
  uId <- asks currentUserId

  srsItm <- lift $ makeSrsEntry v
  let
    upFun :: _ => SrsEntry
      -> SrsReviewData -> m SrsReviewData
    upFun itm rd = do
      mx <- Tree.lookupMaxTree (rd ^. reviews)
      let newk = maybe zerokey nextKey mx
          zerokey = SrsEntryId 0
          nextKey (SrsEntryId k, _) = SrsEntryId (k + 1)

      rd & reviews %%~ Tree.insertTree newk itm
         >>= srsKanjiVocabMap %%~ Tree.insertTree newk v
         >>= case v of
         (Left k) ->
           kanjiSrsMap %%~ Tree.insertTree k newk
         (Right v) ->
           vocabSrsMap %%~ Tree.insertTree v newk

  forM srsItm $ \itm -> do
    lift $ transactSrsDB_ $
      userReviews %%~ updateTreeM uId (upFun itm)

  s <- lift $ transactReadOnlySrsDB $ \db ->
    Tree.lookupTree uId (db ^. userReviews) >>= mapM (\rd ->
      case v of
        (Left k) -> Tree.lookupTree k (rd ^. kanjiSrsMap)
        (Right v) -> Tree.lookupTree v (rd ^. vocabSrsMap))
  return $ join s


data TRM = TRM SrsEntryField (NonEmpty Reading) (NonEmpty Meaning)

makeSrsEntry
  :: (Either KanjiId VocabId)
  -> Handler (Maybe SrsEntry)
makeSrsEntry v = do
  today <- liftIO $ utctDay <$> getCurrentTime
  let
    vocabToText (Vocab ks) = mconcat $ map f ks
      where f (KanjiWithReading (Kanji k) _) = k
            f (Kana k) = k

  tmp <- case v of
    (Left kId) -> do
      kanjiDb <- asks appKanjiDb
      let k = Map.lookup kId kanjiDb
          r = nonEmpty =<< (++)
            <$> k ^? _Just . kanjiDetails . kanjiOnyomi
            <*> k ^? _Just . kanjiDetails . kanjiKunyomi
          m = nonEmpty =<< k ^? _Just . kanjiDetails . kanjiMeanings
          f = k ^? _Just . kanjiDetails . kanjiCharacter . to unKanji
            . to (:|[])
      return $ TRM <$> f <*> r <*> m

    (Right vId) -> do
      vocabDb <- asks appVocabDb
      let v = Map.lookup vId vocabDb
          r = nonEmpty $ v ^.. _Just . vocabEntry . entryReadingElements
            . traverse . readingPhrase . to (Reading . unReadingPhrase)
          m = nonEmpty $ v ^.. _Just . vocabEntry . entrySenses
            . traverse . senseGlosses . traverse . glossDefinition . to (Meaning)
          f = v ^? _Just . vocabDetails . vocab
            . to vocabToText . to (:|[])
      return $ TRM <$> f <*> r <*> m

  let get (TRM f r m) = SrsEntry
        { _reviewState = These state state
          , _readings = r
          , _meaning  = m
          , _readingNotes = Nothing
          , _meaningNotes = Nothing
          , _field = f
        }
      state = (NewReview, SrsEntryStats 0 0)
  return (get <$> tmp)

initSrsDb :: Handler ()
initSrsDb = do
  users <- runDB $ selectKeysList ([] :: [Filter User]) []
  transactSrsDB_ $ \db -> do
    let f db u = do
          let uId = fromSqlKey u
              d = SrsReviewData  Tree.empty Tree.empty Tree.empty Tree.empty
          Tree.lookupTree uId (db ^. userReviews) >>= \case
            (Just _) -> return db
            Nothing -> db & userReviews %%~ (Tree.insertTree uId d)
    foldM f db users

  return ()
