{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
module ReviewState
  (SrsReviewType(..)
  , syncResultWithServer
  , RecogReview
  , ProdReview
  , ReviewStateEvent(..)
  , SrsWidgetState(..)
  , widgetStateFun)
  where

import FrontendCommon
import Control.Lens

import qualified Data.Text as T
import qualified Data.Map as Map
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import NLP.Japanese.Utils

data ReviewStatus =
  NotAnswered | AnsweredWrong
  | AnsweredWithMistake | AnsweredWithoutMistake
  deriving (Eq)

data ProdReview = ProdReview ReviewStatus
data RecogReview = RecogReview (These ReviewStatus ReviewStatus)

class SrsReviewType rt where
  data ActualReviewType rt
  reviewType :: proxy rt -> ReviewType

  initState :: ReviewItem -> rt
  -- Left True -> No Mistake
  -- Left False -> Did Mistake
  -- Right rt -> Not Complete
  updateReviewState :: ActualReviewType rt -> Bool -> rt -> Either Bool rt
  getAnswer :: ReviewItem -> (ActualReviewType rt) -> Either
    (NonEmpty Meaning) (NonEmpty Reading)
  getField :: ReviewItem -> (ActualReviewType rt) -> (NonEmpty Text, Text)
  getInputFieldStyle :: ActualReviewType rt -> Text
  getInputFieldPlaceHolder :: ActualReviewType rt -> Text
  getInputFieldId :: ActualReviewType rt -> Text

  -- Get the pending item based on the review state
  getRandomRT :: ReviewItem -> rt -> Bool -> ActualReviewType rt

instance SrsReviewType ProdReview where
  data ActualReviewType ProdReview = ReadingProdReview
  reviewType = const ReviewTypeProdReview
  initState _ = ProdReview NotAnswered
  updateReviewState _ True (ProdReview NotAnswered) = Left True
  updateReviewState _ True (ProdReview AnsweredWrong) = Left False
  updateReviewState _ False _ = Right (ProdReview AnsweredWrong)
  updateReviewState _ _ _ = error "updateReviewState: Invalid state"
  getAnswer ri _ = Right $ ri ^. reviewItemReading . _1
  getField ri _ = (,) (fmap unMeaning . fst $ ri ^. reviewItemMeaning)
    ("font-size: 2rem;")
  getInputFieldStyle _ = "background-color: antiquewhite;"
  getInputFieldPlaceHolder _ = "日本語で（かな）"
  getInputFieldId _ = "JP-TextInput-IME-Input1"
  getRandomRT _ _ _ = ReadingProdReview

hasKanaInField :: ReviewItem -> Bool
hasKanaInField ri = any (not . (any isKanji) . T.unpack)
  (NE.toList $ fst $ getField ri ReadingRecogReview)

instance SrsReviewType RecogReview where
  data ActualReviewType RecogReview = ReadingRecogReview | MeaningRecogReview
  reviewType = const ReviewTypeRecogReview
  initState ri
    | hasKanaInField ri = RecogReview (That NotAnswered)
    | otherwise = RecogReview (These NotAnswered NotAnswered)
  getField ri _ = (,) (ri ^. reviewItemField)
    ("font-size: 5rem;")

  getAnswer ri ReadingRecogReview = Right $ ri ^. reviewItemReading . _1
  getAnswer ri MeaningRecogReview = Left $ ri ^. reviewItemMeaning . _1

  getInputFieldStyle ReadingRecogReview = "background-color: palegreen;"
  getInputFieldStyle MeaningRecogReview = "background-color: aliceblue;"

  getInputFieldPlaceHolder ReadingRecogReview = "かな"
  getInputFieldPlaceHolder MeaningRecogReview = "意味（英語で）"

  getInputFieldId _ = "JP-TextInput-IME-Input2"

  getRandomRT ri rs b
    | hasKanaInField ri = MeaningRecogReview
    | otherwise = case rs of
    (RecogReview (This _)) -> ReadingRecogReview
    (RecogReview (That _)) -> MeaningRecogReview
    (RecogReview (These s1 s2))
      | f s1 && f s2 -> if b then ReadingRecogReview else MeaningRecogReview
      | f s1 -> ReadingRecogReview
      | f s2 -> MeaningRecogReview
      | otherwise -> error "Both reviews done?"
    where f NotAnswered = True
          f AnsweredWrong = True
          f _ = False

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
  = DoReviewEv (SrsEntryId, ActualReviewType rt, Bool)
  | AddItemsEv [ReviewItem] (Maybe Int)
  | UndoReview


widgetStateFun :: (SrsReviewType rt)
  => SrsWidgetState rt -> ReviewStateEvent rt -> SrsWidgetState rt
widgetStateFun st (AddItemsEv ri pendCount) = st
  & reviewQueue %~ Map.union
     (Map.fromList $ map (\r@(ReviewItem i _ _ _) -> (i,(r, initState r))) ri)
  & resultQueue .~ Nothing
  & case pendCount of
    (Just p) -> reviewStats . srsReviewStats_pendingCount .~ p
    Nothing -> identity

widgetStateFun st (DoReviewEv (i,res,b)) = st
  & reviewQueue %~ (Map.update upF i)
  & resultQueue .~ ((,) <$> pure i <*> done)
  & reviewStats %~ statsUpF
  where
    stOld = snd <$> Map.lookup i (_reviewQueue st)
    stNew = updateReviewState res b <$> stOld
    upF (ri,_) = (,) <$> pure ri <*> (stNew ^? _Just . _Right)
    done = stNew ^? _Just . _Left
    statsUpF s = case stNew of
      (Just (Left True)) -> s
        & srsReviewStats_pendingCount -~ 1
        & srsReviewStats_correctCount +~ 1
      (Just (Left False)) -> s
        & srsReviewStats_pendingCount -~ 1
        & srsReviewStats_incorrectCount +~ 1
      _ -> s

widgetStateFun st (UndoReview) = st

-- Result Synchronise with server
-- TODO implement feature to re-send result if no response recieved
data ResultsSyncState
  = ReadyToSync
  | WaitingForResp [Result] [Result]
  | DoSync [Result]

data ResultSyncEvent
  = AddResult Result
  | FetchPendingReviews
  | SendingResult
  | RespRecieved
  | RetrySendResult

syncResultWithServer :: (AppMonad t m)
  => ReviewType
  -> Event t ()
  -> Event t Result
  -> Dynamic t (SrsWidgetState rt)
  -> AppMonadT t m (Event t (ReviewStateEvent rt))
syncResultWithServer rt refreshEv addEv widgetStateDyn = do
  rec
    let
      sendResultEv =
        fmapMaybeCheap sendResultEvFun $
        attach (current widgetStateDyn) $ updated sendResultDyn

      sendResultEvFun (st, (DoSync r))
        = Just $ SyncReviewItems rt r
          $ if (5 > (Map.size $ _reviewQueue st))
               then Just (Map.keys $ _reviewQueue st)
               else Nothing
      sendResultEvFun _ = Nothing

    sendResResp <- getWebSocketResponse sendResultEv

    sendResDelayedEv <- delay 0.001 sendResultEv
    sendResultDyn <- foldDyn (flip $ foldl (flip handlerSendResultEv)) ReadyToSync
      $ mergeList [
        SendingResult <$ sendResDelayedEv
      , RespRecieved <$ sendResResp
      , AddResult <$> addEv
      , FetchPendingReviews <$ refreshEv
      , RetrySendResult <$ never ]

  return $ (\(a,b) -> AddItemsEv a (Just b))
    <$> fmapMaybeCheap identity sendResResp

handlerSendResultEv :: ResultSyncEvent -> ResultsSyncState -> ResultsSyncState
handlerSendResultEv (AddResult r) ReadyToSync = DoSync  [r]
handlerSendResultEv (AddResult r) (WaitingForResp r' rs) = WaitingForResp r' (r : rs)
handlerSendResultEv (AddResult r) (DoSync  rs) = DoSync  (r : rs)

handlerSendResultEv (FetchPendingReviews) ReadyToSync = DoSync  []
handlerSendResultEv (FetchPendingReviews) s = s


handlerSendResultEv (SendingResult) (DoSync  rs) = WaitingForResp rs []
handlerSendResultEv (SendingResult) _ = error "handlerSendResultEv 1"

handlerSendResultEv (RespRecieved) (WaitingForResp _ []) = ReadyToSync
handlerSendResultEv (RespRecieved) (WaitingForResp _ rs) = DoSync  rs
handlerSendResultEv (RespRecieved) _ = error "handlerSendResultEv 2"

handlerSendResultEv (RetrySendResult) (WaitingForResp r rs) = DoSync  (r ++ rs)
handlerSendResultEv (RetrySendResult) s = s
