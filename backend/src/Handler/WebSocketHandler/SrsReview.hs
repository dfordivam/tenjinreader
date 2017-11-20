{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Handler.WebSocketHandler.SrsReview where

import Import
import Control.Lens
import Model
import Message
import Common
import Handler.WebSocketHandler.Utils
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Time.Clock
import Data.Time.Calendar
import Text.Pretty.Simple
import System.Random
import Data.Aeson

getSrsStats :: GetSrsStats -> WsHandlerM SrsStats
getSrsStats _ = do
  srsEsMap <- gets (_srsEntries)
  srsEs <- if (null srsEsMap)
    then do
      ss <- runSrsDB $ getSrsEntries
      modify (set srsEntries (Map.fromList $ map (\s-> (primaryKey s, s)) ss))
      return ss
    else return $ map snd $ Map.toList srsEsMap

  curTime <- liftIO getCurrentTime

  let
    today = utctDay curTime
    tomorrow = addDays 1 today
    countGrade g = length $ filter (\s -> s ^. srsEntryCurrentGrade == g) srsEs
    pendingReviewCount = length $ filter  (\s -> isJust $ (<) curTime <$> (s ^. srsEntryNextAnswerDate))
      srsEs
    reviewsToday = length $ filter (\s -> isJust $ (<) tomorrow <$> (utctDay <$> (s ^. srsEntryNextAnswerDate)))
      srsEs
    totalItems = length srsEs
    totalReviews = sum $ map (\s -> s ^. srsEntrySuccessCount + s ^. srsEntryFailureCount) srsEs
    averageSuccess = floor $ ((fromIntegral ((sum $ map _srsEntrySuccessCount srsEs)*100)) / (fromIntegral totalReviews))
    discoveringCount = (countGrade 0, countGrade 1)
    committingCount = (countGrade 2, countGrade 3)
    bolsteringCount = (countGrade 4, countGrade 5)
    assimilatingCount = (countGrade 6, countGrade 7)
    setInStone = countGrade 8
  return SrsStats {..}

getBrowseSrsItems      :: BrowseSrsItems
  -> WsHandlerM [SrsItem]
getBrowseSrsItems (BrowseSrsItems lvls) = do
  srsEsMap <- gets (_srsEntries)
  srsEs <- if (null srsEsMap)
    then do
      ss <- runSrsDB $ getSrsEntries
      modify (set srsEntries (Map.fromList $ map (\s-> (primaryKey s, s)) ss))
      return ss
    else return $ map snd $ Map.toList srsEsMap
  srsEsMap <- gets (_srsEntries)
  curTime <- liftIO getCurrentTime

  let res = Map.elems $
        Map.filter
          (\s -> not (s ^. srsEntryIsDeleted)) $
        Map.filter
          (\s -> elem (s ^. srsEntryCurrentGrade) lvls)
        srsEsMap
      f s = SrsItem <$> (getKey $ primaryKey s) <*> v
        <*> pure (isJust $ s ^. srsEntrySuspensionDate)
        <*> p (s ^. srsEntryNextAnswerDate)
        where
          v = getVocabOrKanji s
          p Nothing = Just False
          p (Just d) = Just (d < curTime)

  liftIO $ pPrint res
  let r = catMaybes $ map f res
  liftIO $ pPrint r

  return $ r

getVocabOrKanji :: SrsEntry -> Maybe (Either Vocab Kanji)
getVocabOrKanji s =
  case (s ^. srsEntryAssociatedKanji,
        s ^. srsEntryAssociatedVocab) of
    ((Just k), _) -> Just $ Right $ Kanji k
    (_, (Just v)) -> Just $ Left $ Vocab $ [Kana v]
    _ -> Nothing

getBulkEditSrsItems :: BulkEditSrsItems
  -> WsHandlerM [SrsItem]
getBulkEditSrsItems (BulkEditSrsItems ss op filt) = do
  srsEsMap <- gets (_srsEntries)
  curTime <- liftIO getCurrentTime

  case op of
    SuspendSrsItems -> do
      let
        f :: Map SrsEntryId SrsEntry -> SrsItemId -> WsHandlerM (Map SrsEntryId SrsEntry)
        f sMap sId = do
          let
            sIdK = (makeKey $ Just sId)
            s = Map.lookup sIdK sMap
            sNew = s & _Just . srsEntrySuspensionDate ?~ curTime
            sMap' = Map.update (const sNew) sIdK sMap
          mapM runSrsDB (updateSrsEntry <$> sNew)
          return sMap'

      nMap <- foldlM f srsEsMap ss
      modify (set srsEntries nMap)

    ResumeSrsItems -> do
      let
        f :: Map SrsEntryId SrsEntry -> SrsItemId -> WsHandlerM (Map SrsEntryId SrsEntry)
        f sMap sId = do
          let
            sIdK = (makeKey $ Just sId)
            s = Map.lookup sIdK sMap
            -- s must be already suspended and has a valid review date
            -- Add check for grade <8 ??
            reviewDate :: Maybe UTCTime
            reviewDate = g <$> (s ^? _Just . srsEntrySuspensionDate . _Just)
              <*> (s ^? _Just . srsEntryNextAnswerDate . _Just)
            g susDate prevReviewDate = if susDate > prevReviewDate
              then curTime
              else addUTCTime (diffUTCTime prevReviewDate susDate) curTime
            sNew :: Maybe SrsEntry
            sNew = join $ (\r -> s & _Just . srsEntrySuspensionDate .~ Nothing
                     & _Just . srsEntryNextAnswerDate ?~ r) <$> reviewDate
            sMap' = maybe sMap identity ((\n -> Map.update (const (Just n)) sIdK sMap) <$> sNew)
          mapM runSrsDB (updateSrsEntry <$> sNew)
          return sMap'

      nMap <- foldlM f srsEsMap ss
      modify (set srsEntries nMap)

    ChangeSrsLevel l -> do
      let
        f :: Map SrsEntryId SrsEntry -> SrsItemId -> WsHandlerM (Map SrsEntryId SrsEntry)
        f sMap sId = do
          let
            sIdK = (makeKey $ Just sId)
            s = Map.lookup sIdK sMap
            sNew = s & _Just . srsEntryCurrentGrade .~ l
            sMap' = Map.update (const sNew) sIdK sMap
          mapM runSrsDB (updateSrsEntry <$> sNew)
          return sMap'

      nMap <- foldlM f srsEsMap ss
      modify (set srsEntries nMap)

    ChangeSrsReviewData d -> do
      let
        f :: Map SrsEntryId SrsEntry -> SrsItemId -> WsHandlerM (Map SrsEntryId SrsEntry)
        f sMap sId = do
          let
            sIdK = (makeKey $ Just sId)
            s = Map.lookup sIdK sMap
            sNew = s & _Just . srsEntryNextAnswerDate ?~ d
            sMap' = Map.update (const sNew) sIdK sMap
          mapM runSrsDB (updateSrsEntry <$> sNew)
          return sMap'

      nMap <- foldlM f srsEsMap ss
      modify (set srsEntries nMap)

    DeleteSrsItems -> do
      let
        f :: Map SrsEntryId SrsEntry -> SrsItemId -> WsHandlerM (Map SrsEntryId SrsEntry)
        f sMap sId = do
          let
            sIdK = (makeKey $ Just sId)
            s = Map.lookup sIdK sMap
            sNew = s & _Just . srsEntryIsDeleted .~ True
            sMap' = Map.update (const sNew) sIdK sMap
          mapM runSrsDB (updateSrsEntry <$> sNew)
          return sMap'

      nMap <- foldlM f srsEsMap ss
      modify (set srsEntries nMap)

  getBrowseSrsItems filt

getSrsItem :: GetSrsItem
  -> WsHandlerM (Maybe SrsItemFull)
getSrsItem (GetSrsItem i) = do
  srsEsMap <- gets (_srsEntries)
  let
      res :: SrsEntry -> Maybe SrsItemFull
      res s = g s <$> (getKey $ primaryKey s) <*> v
        where
          v = getVocabOrKanji s
      g :: SrsEntry -> Int -> Either Common.Vocab Common.Kanji -> SrsItemFull
      g s k v = SrsItemFull k v
        (s ^. srsEntryNextAnswerDate)
        (s ^. srsEntryMeanings)
        (s ^. srsEntryReadings)
        (s ^. srsEntryCurrentGrade)
        (s ^. srsEntryMeaningNote)
        (s ^. srsEntryReadingNote)
        (s ^. srsEntryTags)

  return $ join $ res <$> Map.lookup (makeKey $ Just i) srsEsMap

getEditSrsItem :: EditSrsItem
  -> WsHandlerM ()
getEditSrsItem (EditSrsItem sItm)= do
  srsEsMap <- gets (_srsEntries)
  curTime <- liftIO getCurrentTime

  let
    sIdK = (makeKey $ Just $ srsItemFullId sItm)
    s = Map.lookup sIdK srsEsMap
    sNew = s
      & _Just . srsEntryNextAnswerDate .~ (srsReviewDate sItm)
      & _Just . srsEntryMeanings .~ (srsMeanings sItm)
      & _Just . srsEntryReadings .~ (srsReadings sItm)
      & _Just . srsEntryCurrentGrade .~ (srsCurrentGrade sItm)
      & _Just . srsEntryMeaningNote .~ (srsMeaningNote sItm)
      & _Just . srsEntryReadingNote .~ (srsReadingNote sItm)
      & _Just . srsEntryTags .~ (srsTags sItm)

    sMap' = Map.update (const sNew) sIdK srsEsMap
  mapM runSrsDB (updateSrsEntry <$> sNew)
  void $ modify (set srsEntries sMap')

-- When a new review session is started (via "start review" button)
-- A snapshot of pending reviews is fetched and appended to the existing
-- appSrsReviewData ie it wont overwrite the value of ReviewState for
-- items previously in the queue
getGetNextReviewItem   :: GetNextReviewItem
  -> WsHandlerM (Maybe ReviewItem)
getGetNextReviewItem _ = do
  fetchNewReviewItem

-- Assumption - A review item which fails while reviewing
-- will be present in the reviewQueue
-- Therefore its DB entry is not modified till it is
-- succesfully reviewed

-- This will not update the reviewQueue from the DB
-- ie a review session will not see more reviews added to it
-- the user need to close the review and start again
getDoReview
  :: DoReview
  -> WsHandlerM (Maybe ReviewItem)

getDoReview = \case
  (DoReview k r b) -> do
    let i = getSrsEntryId k
    mapM (doReview r b) i
  (AddAnswer k r b) -> return Nothing
  (Undo) -> undoLastReview

-- Correct Answer
doReview r True i = do
  reviewDataRef <- lift $ getReviewDataTVar
  curTime <- liftIO getCurrentTime

  let

    getNewReviewData :: SrsReviewData -> ReviewState
      -> (SrsReviewData, ReviewState)
    getNewReviewData oldReviewData old
      = (SrsReviewData newReviewQ newUndoQ newStats, new)
      where
        new :: ReviewState
        new = Map.adjust f r old

        f :: ReviewStatus -> ReviewStatus
        f NotAnswered = AnsweredWithoutMistake
        f AnsweredWrong = AnsweredWithMistake
        f _ = AnsweredWithMistake -- error?

        oldStats = _reviewStats oldReviewData
        oldReviewQ = _reviewQueue oldReviewData

        reviewDone = isJust (isComplete new)
        newReviewQ = if reviewDone
          then Map.delete i oldReviewQ
          else Map.insert i new oldReviewQ

        newStats = case (isComplete new) of
          (Just True) -> oldStats
            & srsReviewStats_correctCount +~ 1
            & srsReviewStats_pendingReviewItems -~ 1
          (Just False) -> oldStats
            & srsReviewStats_pendingReviewItems -~ 1
          _ -> oldStats

        newUndoQ = take undoQueueLength $
                  (sOld <$ (isComplete new), i, old, r)
                  : (_undoQueue oldReviewData)

    getNewSrsEntry :: Bool -> SrsEntry
    getNewSrsEntry = \case
      True -> sOld
        & srsEntrySuccessCount +~ 1
        & srsEntryCurrentGrade +~ 1
        & srsEntryNextAnswerDate ?~ g True
      False -> sOld
        & srsEntryFailureCount +~ 1
        & srsEntryCurrentGrade -~ 1
        & srsEntryNextAnswerDate ?~ g False
      where
        g b = getNextReviewDate b curTime
            (sOld ^. srsEntryNextAnswerDate)
            (sOld ^. srsEntryCurrentGrade)


    updateReviewData oldReviewData =
      (getNewReviewData oldReviewData) <$> oldReviewState
      where
        -- Getting Nothing here is an error and should be logged
        oldReviewState :: Maybe ReviewState
        oldReviewState = Map.lookup i (_reviewQueue oldReviewData)

  newReviewState <- liftIO $ atomically $ do
    o <- readTVar reviewDataRef
    let n = updateReviewData o
    mapM (writeTVar reviewDataRef) (fst <$> n)
    return (snd <$> n)

  let
    newSrsEntry :: Maybe SrsEntry
    newSrsEntry = getNewSrsEntry oldSrsEntry curTime
      <$> (join $ isComplete <$> newReviewState)

  -- If the answer is complete then update db
  mapM writeSrsEntry newSrsEntry

  fetchNewReviewItem

  -- Wrong Answer
doReview r False i = do
  reviewDataRef <- lift $ getReviewDataTVar
  -- ReviewState mark AnsweredWrong
  -- Undo Q add
  -- Stats modify if NotAnswered -> AnsweredWrong
  let
    getNewReviewData :: SrsReviewData -> ReviewState
      -> (SrsReviewData, ReviewState)
    getNewReviewData oldReviewData old
      = (SrsReviewData newReviewQ newUndoQ newStats, new)
      where
        new :: ReviewState
        new = Map.adjust f r old

        f :: ReviewStatus -> ReviewStatus
        f _ = AnsweredWrong

        oldStats = _reviewStats oldReviewData
        oldReviewQ = _reviewQueue oldReviewData

        newReviewQ = Map.insert i new oldReviewQ

        newStats = if (all (== NotAnswered) $ Map.elems old)
          then oldStats
            & srsReviewStats_incorrectCount +~ 1
          else oldStats

        newUndoQ = take undoQueueLength $
                  (Nothing, i, old, r)
                  : (_undoQueue oldReviewData)

    updateReviewData oldReviewData =
      (getNewReviewData oldReviewData) <$> oldReviewState
      where
        -- Getting Nothing here is an error and should be logged
        oldReviewState :: Maybe ReviewState
        oldReviewState = Map.lookup i (_reviewQueue oldReviewData)

  newReviewState <- liftIO $ atomically $ do
    o <- readTVar reviewDataRef
    let n = updateReviewData o
    mapM (writeTVar reviewDataRef) (fst <$> n)
    return (snd <$> n)

  fetchNewReviewItem

-- Notes on undoQueue working
--
undoLastReview = do
  return Nothing

getReviewDataTVar :: Handler (TVar SrsReviewData)
getReviewDataTVar = do
  uId <- requireAuthId
  appRDRef <- asks appSrsReviewData
  appRD <- liftIO $ readTVarIO appRDRef
  case Map.lookup uId appRD of
    Just r -> return r
    Nothing -> do -- Should not happen ideally
      liftIO $ atomically $ do
        r <- newTVar def
        modifyTVar (Map.insert uId r) appRDRef
        return r

-- Just True -> No Mistake
-- Just False -> Did Mistake
-- Nothing -> Not Complete
isComplete :: ReviewState -> Maybe Bool
isComplete rSt =
  if all done reviews
    then Just not (any wrong reviews)
    else Nothing
  where done AnsweredWithMistake = True
        done AnsweredWithoutMistake = True
        done _ = False
        wrong AnsweredWithMistake = True
        wrong _ = False
        reviews = map snd $ Map.toList rSt

-- The reviewQueue contains the active reviews
-- and it is randomly chosen from pending reviews (from DB)
-- It provides a window of reviews which have to be completed
-- When the length of this drops below minReviewQLength, then
-- more pending reviews from the DB are fetched and added to it
minReviewQLength = 7
maxReviewQLength = 15

fetchNewReviewItem :: WsHandlerM (Maybe ReviewItem)
fetchNewReviewItem = do
  reviewDataRef <- lift $ getReviewDataTVar
  reviewData <- liftIO $ readTVarIO reviewDataRef
  let reviewQ = _reviewQueue reviewData
  if Map.null reviewQ
    then return Nothing
    else fetchNewReviewItemInt reviewQ

fetchNewReviewItemInt reviewQ = do
  srsEsMap <- gets (_srsEntries)
  reviewStats <- gets (_reviewStats)
  (rId:_) <- liftIO $ getRandomItems (Map.keys reviewQ) 1
  rtToss <- liftIO $ randomIO
  let
    rtDone :: Maybe ReviewType
    rtDone = (Map.lookup rId reviewQ) ^? _Just . _1 . _Just

    rt :: ReviewType
    rt = case rtDone of
      Nothing -> if rtToss
        then ReadingReview
        else MeaningReview
      (Just ReadingReview) -> MeaningReview
      (Just MeaningReview) -> ReadingReview

  return $ getReviewItem srsEsMap reviewStats rId rt

getReviewItem
  :: Map SrsEntryId SrsEntry
  -> SrsReviewStats
  -> SrsEntryId
  -> ReviewType
  -> Maybe ReviewItem
getReviewItem srsEsMap reviewStats rId rt =
  let
    s :: Maybe SrsEntry
    s = Map.lookup rId srsEsMap

    i :: Maybe SrsItemId
    i = join $ (getKey <$> (primaryKey <$> s))

    k :: Maybe (Either Vocab Kanji)
    k = join $ getVocabOrKanji <$> s

    m :: Maybe (Either (Meaning, MeaningNotes)
                (Reading, ReadingNotes))
    m = getM <$> s
    getM s = case rt of
      MeaningReview -> Left
        (Meaning $ s ^. srsEntryMeanings
        , maybe "" identity $ s ^.srsEntryMeaningNote)

      ReadingReview -> Right
        (s ^. srsEntryReadings
        , maybe "" identity $ s ^. srsEntryReadingNote)

    ret = ReviewItem <$> i <*> k <*> m <*> pure reviewStats
  in ret


getRandomItems :: [a] -> Int -> IO [a]
getRandomItems inp s = do
  let l = length inp
      idMap = Map.fromList $ zip [1..l] inp

      loop set = do
        r <- randomRIO (1,l)
        let setN = Set.insert r set
        if Set.size setN >= s
          then return setN
          else loop setN

  set <- loop Set.empty
  return $ catMaybes $
    fmap (\k -> Map.lookup k idMap) $ Set.toList set

getNextReviewDate
  :: Bool
  -> UTCTime
  -> Maybe UTCTime
  -> Int
  -> UTCTime
getNextReviewDate
  success curTime revDate oldGrade =
  let
    addHour h = addUTCTime (h*60*60) curTime
    addDay d = addUTCTime (d*24*60*60) curTime
  in case (oldGrade, success) of
    (0,_) -> addHour 4
    (1,False) -> addHour 4

    (2,False) -> addHour 8
    (1,True) -> addHour 8

    (2,True) -> addDay 1
    (3,False) -> addDay 1

    (3,True) -> addDay 3
    (4,False) -> addDay 3

    (4,True) -> addDay 7
    (5,False) -> addDay 7

    (5,True) -> addDay 14
    (6,False) -> addDay 14

    (6,True) -> addDay 30
    (7,False) -> addDay 30

    (7,True) -> addDay 120
    _ -> curTime -- error

getCheckAnswer :: CheckAnswer -> WsHandlerM CheckAnswerResult
getCheckAnswer (CheckAnswer reading alt) = do
  liftIO $ pPrint alt
  let  f (c,t) = (,) <$> pure c <*> getKana t
  kanaAlts <- lift $ mapM (mapM f) alt
  liftIO $ pPrint kanaAlts
  return $ checkAnswerInt reading kanaAlts

checkAnswerInt :: Reading -> [[(Double, Text)]] -> CheckAnswerResult
checkAnswerInt (Reading reading) kanaAlts =
  case elem reading kanas of
    True -> AnswerCorrect
    False -> AnswerIncorrect "T"
  where
    kanas = mconcat $ map (map snd) kanaAlts

getSrsItemId :: SrsEntryId -> SrsItemId
getSrsItemId = SrsItemId . fromSqlKey

getSrsEntryId :: SrsItemId -> SrsEntryId
getSrsEntryId (SrsItemId v) = toSqlKey v
