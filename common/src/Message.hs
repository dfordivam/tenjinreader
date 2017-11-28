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
  :<|> LoadMoreKanjiVocab

  :<|> VocabSearch
  :<|> LoadMoreVocabSearchResult


  :<|> GetSrsStats

  -- Doing Review
  :<|> GetNextReviewItems
  :<|> DoReview
  :<|> CheckAnswer

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
  [VocabDetails]
  deriving (Generic, Show, ToJSON, FromJSON)

data LoadMoreKanjiVocab = LoadMoreKanjiVocab
  deriving (Generic, Show, ToJSON, FromJSON)

instance WebSocketMessage AppRequest LoadMoreKanjiVocab where
  type ResponseT AppRequest LoadMoreKanjiVocab = [VocabDetails]

----------------------------------------------------------------
data LoadMoreKanjiResults = LoadMoreKanjiResults
  deriving (Generic, Show, ToJSON, FromJSON)

instance WebSocketMessage AppRequest LoadMoreKanjiResults where
  type ResponseT AppRequest LoadMoreKanjiResults = KanjiList

----------------------------------------------------------------
data VocabSearch = VocabSearch AdditionalFilter
  deriving (Generic, Show, ToJSON, FromJSON)

instance WebSocketMessage AppRequest VocabSearch where
  type ResponseT AppRequest VocabSearch = [VocabDetails]

data LoadMoreVocabSearchResult = LoadMoreVocabSearchResult
  deriving (Generic, Show, ToJSON, FromJSON)

instance WebSocketMessage AppRequest LoadMoreVocabSearchResult where
  type ResponseT AppRequest LoadMoreVocabSearchResult = [VocabDetails]

----------------------------------------------------------------

data GetSrsStats = GetSrsStats ()
  deriving (Generic, Show, ToJSON, FromJSON)

instance WebSocketMessage AppRequest GetSrsStats where
  type ResponseT AppRequest GetSrsStats = SrsStats

data SrsStats = SrsStats
  { reviewsToday :: Int
  , totalItems :: Int
  , totalReviews :: Int
  , averageSuccess :: Int
  }
  deriving (Generic, Show, ToJSON, FromJSON)

----------------------------------------------------------------

data BrowseSrsItems = BrowseSrsItems [SrsLevel]
  deriving (Generic, Show, ToJSON, FromJSON)

instance WebSocketMessage AppRequest BrowseSrsItems where
  type ResponseT AppRequest BrowseSrsItems = [SrsItem]

----------------------------------------------------------------
data GetNextReviewItems =
  GetNextReviewItems [SrsEntryId]
  deriving (Generic, Show, ToJSON, FromJSON)

instance WebSocketMessage AppRequest GetNextReviewItems where
  type ResponseT AppRequest GetNextReviewItems
    = [ReviewItem]

data ReviewItem = ReviewItem
  SrsEntryId
  (Either Vocab Kanji)
  ([Meaning], Maybe MeaningNotes)
  ([Reading], Maybe ReadingNotes)
  deriving (Generic, Show, ToJSON, FromJSON)

data DoReview = DoReview [(SrsEntryId, Bool)]
  deriving (Generic, Show, ToJSON, FromJSON)

instance WebSocketMessage AppRequest DoReview where
  type ResponseT AppRequest DoReview
    = Bool

----------------------------------------------------------------
data CheckAnswer =
  CheckAnswer [Reading] [[(Double, Text)]]
  deriving (Generic, Show, ToJSON, FromJSON)

data CheckAnswerResult
  = AnswerCorrect
  | AnswerIncorrect Text
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

instance WebSocketMessage AppRequest CheckAnswer where
  type ResponseT AppRequest CheckAnswer = CheckAnswerResult

----------------------------------------------------------------
data GetSrsItem = GetSrsItem SrsEntryId
  deriving (Generic, Show, ToJSON, FromJSON)

instance WebSocketMessage AppRequest GetSrsItem where
  type ResponseT AppRequest GetSrsItem = Maybe SrsItemFull

----------------------------------------------------------------
data EditSrsItem = EditSrsItem SrsItemFull
  deriving (Generic, Show, ToJSON, FromJSON)

instance WebSocketMessage AppRequest EditSrsItem where
  type ResponseT AppRequest EditSrsItem = ()

----------------------------------------------------------------
data BulkEditSrsItems = BulkEditSrsItems [SrsEntryId] BulkEditOperation BrowseSrsItems
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
