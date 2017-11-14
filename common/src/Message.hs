{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
module Message
  where

import Protolude
import Data.Aeson
import Data.Default
import Data.Time (UTCTime)

import Reflex.Dom.WebSocket.Message

import Common

-- Messages

type AppRequest
  =
    -- Kanji/Vocab Browser
    KanjiFilter
  :<|> LoadMoreKanjiResults
  :<|> GetKanjiDetails
  :<|> VocabSearch


  :<|> GetSrsStats

  -- Doing Review
  :<|> GetNextReviewItem
  :<|> CheckAnswer
  :<|> DoReview

  -- Browsing Srs Items
  :<|> BrowseSrsItems
  :<|> GetSrsItem
  :<|> EditSrsItem
  :<|> BulkEditSrsItems

----------------------------------------------------------------
data KanjiFilter = KanjiFilter
  { textContent :: Text
  , kanjiAdditionalFilter :: AdditionalFilter
  , selectedRadicals :: [RadicalId]
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance Default KanjiFilter where
  def = KanjiFilter "" def []

instance WebSocketMessage AppRequest KanjiFilter where
  type ResponseT AppRequest KanjiFilter = KanjiFilterResult

data KanjiFilterResult =
  KanjiFilterResult KanjiList --
                    [RadicalId] -- Valid Radicals
  deriving (Generic, Show, ToJSON, FromJSON)

type KanjiList =
   [(KanjiId, Kanji, Maybe Rank, [Meaning])]

----------------------------------------------------------------
data GetKanjiDetails =
  GetKanjiDetails KanjiId AdditionalFilter
  deriving (Generic, Show, ToJSON, FromJSON)

instance WebSocketMessage AppRequest GetKanjiDetails where
  type ResponseT AppRequest GetKanjiDetails = Maybe KanjiSelectionDetails

data KanjiSelectionDetails =
  KanjiSelectionDetails KanjiDetails
  [VocabDispItem]
  deriving (Generic, Show, ToJSON, FromJSON)

----------------------------------------------------------------
data LoadMoreKanjiResults = LoadMoreKanjiResults
  deriving (Generic, Show, ToJSON, FromJSON)

instance WebSocketMessage AppRequest LoadMoreKanjiResults where
  type ResponseT AppRequest LoadMoreKanjiResults = KanjiFilterResult

----------------------------------------------------------------
data VocabSearch = VocabSearch AdditionalFilter
  deriving (Generic, Show, ToJSON, FromJSON)

instance WebSocketMessage AppRequest VocabSearch where
  type ResponseT AppRequest VocabSearch = [VocabDispItem]

data VocabDispItem =
  VocabDispItem Vocab
                (Maybe Rank)
                ([Meaning])
                (Maybe JlptLevel)
                (Maybe WkLevel)
                (Maybe WikiRank)
  deriving (Generic, Show, ToJSON, FromJSON)

----------------------------------------------------------------

data GetSrsStats = GetSrsStats ()
  deriving (Generic, Show, ToJSON, FromJSON)

instance WebSocketMessage AppRequest GetSrsStats where
  type ResponseT AppRequest GetSrsStats = SrsStats

data SrsStats = SrsStats
  { pendingReviewCount :: Int
  , reviewsToday :: Int
  , totalItems :: Int
  , totalReviews :: Int
  , averageSuccess :: Int
  , discoveringCount :: (Int, Int)
  , committingCount :: (Int, Int)
  , bolsteringCount :: (Int, Int)
  , assimilatingCount :: (Int, Int)
  , setInStone :: Int
  }
  deriving (Generic, Show, ToJSON, FromJSON)

----------------------------------------------------------------

data BrowseSrsItems = BrowseSrsItems [SrsLevel]
  deriving (Generic, Show, ToJSON, FromJSON)

instance WebSocketMessage AppRequest BrowseSrsItems where
  type ResponseT AppRequest BrowseSrsItems = [SrsItem]

----------------------------------------------------------------
data GetNextReviewItem = GetNextReviewItem
  deriving (Generic, Show, ToJSON, FromJSON)

instance WebSocketMessage AppRequest GetNextReviewItem where
  type ResponseT AppRequest GetNextReviewItem
    = Maybe ReviewItem

----------------------------------------------------------------
data CheckAnswer =
  CheckAnswer Reading [[(Double, Text)]]
  deriving (Generic, Show, ToJSON, FromJSON)

data CheckAnswerResult
  = AnswerCorrect
  | AnswerIncorrect Text
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

instance WebSocketMessage AppRequest CheckAnswer where
  type ResponseT AppRequest CheckAnswer = CheckAnswerResult

----------------------------------------------------------------

data DoReview
  = DoReview SrsItemId ReviewType Bool
  | UndoReview
  | AddAnswer SrsItemId ReviewType Text
  deriving (Generic, Show, ToJSON, FromJSON)

data ReviewType =
  MeaningReview | ReadingReview
  deriving (Eq, Generic, Show, ToJSON, FromJSON)

instance WebSocketMessage AppRequest DoReview where
  type ResponseT AppRequest DoReview = Maybe ReviewItem

data ReviewItem = ReviewItem
  SrsItemId
  (Either Vocab Kanji)
  (Either (Meaning, MeaningNotes) (Reading, ReadingNotes))
  SrsReviewStats
  deriving (Generic, Show, ToJSON, FromJSON)
----------------------------------------------------------------
data GetSrsItem = GetSrsItem SrsItemId
  deriving (Generic, Show, ToJSON, FromJSON)

instance WebSocketMessage AppRequest GetSrsItem where
  type ResponseT AppRequest GetSrsItem = Maybe SrsItemFull

----------------------------------------------------------------
data EditSrsItem = EditSrsItem SrsItemFull
  deriving (Generic, Show, ToJSON, FromJSON)

instance WebSocketMessage AppRequest EditSrsItem where
  type ResponseT AppRequest EditSrsItem = ()

----------------------------------------------------------------
data BulkEditSrsItems = BulkEditSrsItems [SrsItemId] BulkEditOperation BrowseSrsItems
  deriving (Generic, Show, ToJSON, FromJSON)

data BulkEditOperation
  = SuspendSrsItems
  | ResumeSrsItems
  | ChangeSrsLevel SrsLevel
  | ChangeSrsReviewData UTCTime
  | DeleteSrsItems
  deriving (Generic, Show, ToJSON, FromJSON)

instance WebSocketMessage AppRequest BulkEditSrsItems where
  type ResponseT AppRequest BulkEditSrsItems = [SrsItem]

----------------------------------------------------------------
