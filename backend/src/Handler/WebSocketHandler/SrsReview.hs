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
import Model
import SrsDB
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

import qualified Data.BTree.Impure as Tree
import Control.Monad.Haskey
import Data.BTree.Alloc (AllocM, AllocReaderM)

getSrsStats :: GetSrsStats -> WsHandlerM SrsStats
getSrsStats _ = do
  return $ SrsStats 0 0 0 0

getBrowseSrsItems      :: BrowseSrsItems
  -> WsHandlerM [SrsItem]
getBrowseSrsItems (BrowseSrsItems lvls) = do

  return $ []

getBulkEditSrsItems :: BulkEditSrsItems
  -> WsHandlerM [SrsItem]
getBulkEditSrsItems (BulkEditSrsItems ss op filt) = do
  curTime <- liftIO getCurrentTime

  -- case op of
  --   SuspendSrsItems -> do
  --     let
  --       f :: Map SrsEntryId SrsEntry -> SrsEntryId -> WsHandlerM (Map SrsEntryId SrsEntry)
  --       f sMap sId = do
  --         let
  --           sIdK = (makeKey $ Just sId)
  --           s = Map.lookup sIdK sMap
  --           sNew = s & _Just . srsEntrySuspensionDate ?~ curTime
  --           sMap' = Map.update (const sNew) sIdK sMap
  --         mapM runSrsDB (updateSrsEntry <$> sNew)
  --         return sMap'

  --     nMap <- foldlM f srsEsMap ss
  --     modify (set srsEntries nMap)

  --   ResumeSrsItems -> do
  --     let
  --       f :: Map SrsEntryId SrsEntry -> SrsEntryId -> WsHandlerM (Map SrsEntryId SrsEntry)
  --       f sMap sId = do
  --         let
  --           sIdK = (makeKey $ Just sId)
  --           s = Map.lookup sIdK sMap
  --           -- s must be already suspended and has a valid review date
  --           -- Add check for grade <8 ??
  --           reviewDate :: Maybe UTCTime
  --           reviewDate = g <$> (s ^? _Just . srsEntrySuspensionDate . _Just)
  --             <*> (s ^? _Just . srsEntryNextAnswerDate . _Just)
  --           g susDate prevReviewDate = if susDate > prevReviewDate
  --             then curTime
  --             else addUTCTime (diffUTCTime prevReviewDate susDate) curTime
  --           sNew :: Maybe SrsEntry
  --           sNew = join $ (\r -> s & _Just . srsEntrySuspensionDate .~ Nothing
  --                    & _Just . srsEntryNextAnswerDate ?~ r) <$> reviewDate
  --           sMap' = maybe sMap identity ((\n -> Map.update (const (Just n)) sIdK sMap) <$> sNew)
  --         mapM runSrsDB (updateSrsEntry <$> sNew)
  --         return sMap'

  --     nMap <- foldlM f srsEsMap ss
  --     modify (set srsEntries nMap)

  --   ChangeSrsLevel l -> do
  --     let
  --       f :: Map SrsEntryId SrsEntry -> SrsEntryId -> WsHandlerM (Map SrsEntryId SrsEntry)
  --       f sMap sId = do
  --         let
  --           sIdK = (makeKey $ Just sId)
  --           s = Map.lookup sIdK sMap
  --           sNew = s & _Just . srsEntryCurrentGrade .~ l
  --           sMap' = Map.update (const sNew) sIdK sMap
  --         mapM runSrsDB (updateSrsEntry <$> sNew)
  --         return sMap'

  --     nMap <- foldlM f srsEsMap ss
  --     modify (set srsEntries nMap)

  --   ChangeSrsReviewData d -> do
  --     let
  --       f :: Map SrsEntryId SrsEntry -> SrsEntryId -> WsHandlerM (Map SrsEntryId SrsEntry)
  --       f sMap sId = do
  --         let
  --           sIdK = (makeKey $ Just sId)
  --           s = Map.lookup sIdK sMap
  --           sNew = s & _Just . srsEntryNextAnswerDate ?~ d
  --           sMap' = Map.update (const sNew) sIdK sMap
  --         mapM runSrsDB (updateSrsEntry <$> sNew)
  --         return sMap'

  --     nMap <- foldlM f srsEsMap ss
  --     modify (set srsEntries nMap)

  --   DeleteSrsItems -> do
  --     let
  --       f :: Map SrsEntryId SrsEntry -> SrsEntryId -> WsHandlerM (Map SrsEntryId SrsEntry)
  --       f sMap sId = do
  --         let
  --           sIdK = (makeKey $ Just sId)
  --           s = Map.lookup sIdK sMap
  --           sNew = s & _Just . srsEntryIsDeleted .~ True
  --           sMap' = Map.update (const sNew) sIdK sMap
  --         mapM runSrsDB (updateSrsEntry <$> sNew)
  --         return sMap'

  --     nMap <- foldlM f srsEsMap ss
  --     modify (set srsEntries nMap)

  getBrowseSrsItems filt

getSrsItem :: GetSrsItem
  -> WsHandlerM (Maybe SrsItemFull)
getSrsItem (GetSrsItem i) = do
  return Nothing

getEditSrsItem :: EditSrsItem
  -> WsHandlerM ()
getEditSrsItem (EditSrsItem sItm)= return ()

getGetNextReviewItem   :: GetNextReviewItems
  -> WsHandlerM (Maybe ReviewItem)
getGetNextReviewItem (GetNextReviewItems alreadyPresent) = do
  uId <- asks currentUserId
  today <- liftIO $ utctDay <$> getCurrentTime

  return $ Nothing

getDoReview (DoReview results) = do
  today <- liftIO $ utctDay <$> getCurrentTime
  master <- lift $ getYesod
  uId <- asks currentUserId

  let
    doUp :: (AllocM m) => Tree.Tree SrsEntryId SrsEntry
      -> (SrsEntryId, Bool)
      -> m (Tree.Tree SrsEntryId SrsEntry)
    doUp t (rId,b) = updateTreeM rId
        (\r -> return $ updateSrsEntry b today r) t

  lift $ transactSrsDB_ $
    userReviews %%~ updateTreeM uId
      (reviews %%~ (\rt -> foldlM doUp rt results))
  return True

updateSrsEntry b today r = r
  & reviewState %~ modifyState
  & stats %~ modifyStats

  where
    modifyState (NextReviewDate d i) =
      getNextReviewDate (r ^. stats) today d i b
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
  :: SrsEntryId
  -> SrsEntry
  -> ReviewItem
getReviewItem i s =
  ReviewItem i (Right $ Kanji (s ^. field)) (m,mn) (r,rn)
  where
    m = (s ^. meaning)
    mn = (s ^. meaningNotes)
    r = (s ^. readings)
    rn = (s ^. readingNotes)

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
  :: SrsEntryStats
  -> Day
  -> Day
  -> SrsInterval
  -> Bool
  -> SrsEntryState
getNextReviewDate
  stats today dueDate (SrsInterval lastInterval) success =
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
