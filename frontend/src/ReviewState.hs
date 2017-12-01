{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
module ReviewState where

import FrontendCommon
import Control.Lens

import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Data.Map as Map
import System.Random

data ReviewStatus =
  NotAnswered | AnsweredWrong
  | AnsweredWithMistake | AnsweredWithoutMistake
  deriving (Eq)

-- type ReviewState = Map RecogReview ReviewStatus -- Recog -> These ReviewStatus ReviewStatus -- Prod -> ReviewStatus
data ProdReview = ProdReview ReviewStatus
data RecogReview = RecogReview (These ReviewStatus ReviewStatus)

class SrsReviewType rt where
  data ReviewResult rt

  initState :: ReviewItem -> rt
  -- Left True -> No Mistake
  -- Left False -> Did Mistake
  -- Right rt -> Not Complete
  updateReviewState :: ReviewResult rt -> Bool -> rt -> Either Bool rt
  getAnswer :: ReviewItem -> (ReviewResult rt) -> Either
    [Meaning] [Reading]

type Result = (SrsEntryId,Bool)

-- How to fetch reviews from reviewQ and send to review widget
-- Select random reviewId then select review type
-- After doing a review whether success or failure fetch a new review
-- if reviewQ empty then do this whenever reviewQ becomes non empty
data SrsWidgetState rt = SrsWidgetState
  { _reviewQueue :: Map SrsEntryId (ReviewItem, rt)
  , _resultQueue :: Maybe Result
  , _reviewStats :: SrsReviewStats
  }

makeLenses ''SrsWidgetState

data ReviewStateEvent rt
  = DoReviewEv (SrsEntryId, ReviewResult rt, Bool)
  | AddItemsEv [ReviewItem]
  | UndoReview

-- data RecogReview =
--   RecogMeaningReview | RecogReadingReview
--   deriving (Eq, Ord, Enum, Bounded, Generic, Show)

-- isComplete :: ReviewState -> Maybe Bool
-- isComplete rSt =
--   if all done reviews
--     then Just $ not (any wrong reviews)
--     else Nothing
--   where done AnsweredWithMistake = True
--         done AnsweredWithoutMistake = True
--         done _ = False
--         wrong AnsweredWithMistake = True
--         wrong _ = False
--         reviews = map snd $ Map.toList rSt


widgetStateFun :: (SrsReviewType rt) => SrsWidgetState rt -> ReviewStateEvent rt -> SrsWidgetState rt
widgetStateFun st (AddItemsEv ri) = st
  & reviewQueue %~ Map.union
     (Map.fromList $ map (\r@(ReviewItem i _ _ _) -> (i,(r, initState r))) ri)
  & resultQueue .~ Nothing

widgetStateFun st (DoReviewEv (i,res,b)) = st
  & reviewQueue %~ (Map.update upF i)
  & resultQueue .~ ((,) <$> pure i <*> done)
  where
    stOld = snd <$> Map.lookup i (_reviewQueue st)
    stNew = updateReviewState res b <$> stOld
    upF (ri,_) = (,) <$> pure ri <*> (stNew ^? _Just . _Right)
    done = stNew ^? _Just . _Left

widgetStateFun st (UndoReview) = st

-- Result Synchronise with server
-- TODO implement feature to re-send result if no response recieved
data ResultsSyncState =
  ReadyToSend
  | WaitingForResp [Result] [Result]

sendResultEvFun (ReadyToSend) = Nothing
sendResultEvFun (WaitingForResp r _) = Just (DoReview undefined r)

handlerSendResultEv :: ResultsSyncState -> (These Result ()) -> ResultsSyncState
handlerSendResultEv ReadyToSend (This r) = WaitingForResp [r] []
handlerSendResultEv ReadyToSend (That _) = error "Got result resp, when not expecting"
handlerSendResultEv ReadyToSend (These r _) = error "Got result resp, when not expecting"
handlerSendResultEv (WaitingForResp r rs) (This rn) = WaitingForResp r (rs ++ [rn])
handlerSendResultEv (WaitingForResp _ rs) (That _) = WaitingForResp rs []
handlerSendResultEv (WaitingForResp _ []) (That _) = ReadyToSend
handlerSendResultEv (WaitingForResp _ (rs)) (These rn _) = WaitingForResp (rs ++ [rn]) []
handlerSendResultEv (WaitingForResp _ []) (These rn _) = WaitingForResp [rn] []

