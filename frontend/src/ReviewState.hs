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
import Data.List.NonEmpty (NonEmpty)

data ReviewStatus =
  NotAnswered | AnsweredWrong
  | AnsweredWithMistake | AnsweredWithoutMistake
  deriving (Eq)

data ProdReview = ProdReview ReviewStatus
data RecogReview = RecogReview (These ReviewStatus ReviewStatus)

class SrsReviewType rt where
  data ReviewResult rt
  reviewType :: proxy rt -> ReviewType

  initState :: ReviewItem -> rt
  -- Left True -> No Mistake
  -- Left False -> Did Mistake
  -- Right rt -> Not Complete
  updateReviewState :: ReviewResult rt -> Bool -> rt -> Either Bool rt
  getAnswer :: ReviewItem -> (ReviewResult rt) -> Either
    (NonEmpty Meaning) (NonEmpty Reading)

instance SrsReviewType ProdReview where
  data ReviewResult ProdReview = ReadingProdReview
  reviewType = const ReviewTypeProdReview
  initState _ = ProdReview NotAnswered
  updateReviewState _ True (ProdReview NotAnswered) = Left True
  updateReviewState _ True (ProdReview AnsweredWrong) = Left False
  updateReviewState _ False _ = Right (ProdReview AnsweredWrong)
  updateReviewState _ _ _ = error "updateReviewState: Invalid state"
  getAnswer ri _ = Right $ ri ^. reviewItemReading . _1

instance SrsReviewType RecogReview where
  data ReviewResult RecogReview = ReadingRecogReview | MeaningRecogReview
  reviewType = const ReviewTypeRecogReview
  initState _ = RecogReview (These NotAnswered NotAnswered)

  getAnswer ri ReadingRecogReview = Right $ ri ^. reviewItemReading . _1
  getAnswer ri MeaningRecogReview = Left $ ri ^. reviewItemMeaning . _1

  updateReviewState rt b (RecogReview ths) = if bothDone
    then Left $ result
    else Right $ RecogReview newThs
    where
      newThs = case rt of
        ReadingRecogReview -> mapThis f ths
        MeaningRecogReview -> mapThat f ths
      f NotAnswered = if b then AnsweredWithoutMistake else AnsweredWrong
      f _ = if b then AnsweredWithMistake else AnsweredWrong

      done r = (r == AnsweredWithMistake) || (r == AnsweredWithoutMistake)
      bothDone = mergeTheseWith done done (&&) newThs
      result = mergeTheseWith correct correct (&&) newThs
      correct = (== AnsweredWithoutMistake)

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
