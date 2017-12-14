{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
module Message
  where

import Protolude
import Data.Aeson
import Data.Default
import Data.Time.Calendar
import Data.List.NonEmpty (NonEmpty)
import Control.Lens.TH

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

  :<|> QuickAddSrsItem

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

  :<|> GetAnnotatedText
  :<|> GetVocabDetails

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

type VocabList = [(VocabDetails, Maybe SrsEntryId)]

data KanjiSelectionDetails =
  KanjiSelectionDetails KanjiDetails (Maybe SrsEntryId) VocabList
  deriving (Generic, Show, ToJSON, FromJSON)

data LoadMoreKanjiVocab = LoadMoreKanjiVocab
  deriving (Generic, Show, ToJSON, FromJSON)

instance WebSocketMessage AppRequest LoadMoreKanjiVocab where
  type ResponseT AppRequest LoadMoreKanjiVocab = VocabList

----------------------------------------------------------------
data LoadMoreKanjiResults = LoadMoreKanjiResults
  deriving (Generic, Show, ToJSON, FromJSON)

instance WebSocketMessage AppRequest LoadMoreKanjiResults where
  type ResponseT AppRequest LoadMoreKanjiResults = KanjiList

----------------------------------------------------------------
data VocabSearch = VocabSearch AdditionalFilter
  deriving (Generic, Show, ToJSON, FromJSON)

instance WebSocketMessage AppRequest VocabSearch where
  type ResponseT AppRequest VocabSearch = VocabList

data LoadMoreVocabSearchResult = LoadMoreVocabSearchResult
  deriving (Generic, Show, ToJSON, FromJSON)

instance WebSocketMessage AppRequest LoadMoreVocabSearchResult where
  type ResponseT AppRequest LoadMoreVocabSearchResult = VocabList

----------------------------------------------------------------
data QuickAddSrsItem = QuickAddSrsItem (Either KanjiId VocabId)
  deriving (Generic, Show, ToJSON, FromJSON)

instance WebSocketMessage AppRequest QuickAddSrsItem where
  type ResponseT AppRequest QuickAddSrsItem = Maybe SrsEntryId

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
data BrowseSrsItems = BrowseSrsItems ReviewType BrowseSrsItemsFilter
  deriving (Generic, Show, ToJSON, FromJSON)

instance WebSocketMessage AppRequest BrowseSrsItems where
  type ResponseT AppRequest BrowseSrsItems = [SrsItem]

data BrowseSrsItemsFilter
  = BrowseDueItems SrsItemLevel
  | BrowseNewItems
  | BrowseSuspItems SrsItemLevel
  | BrowseOtherItems SrsItemLevel
  deriving (Generic, Show, ToJSON, FromJSON)

data SrsItemLevel = LearningLvl | IntermediateLvl | MatureLvl
  deriving (Eq, Ord, Generic, Show, ToJSON, FromJSON)

----------------------------------------------------------------
data GetNextReviewItems =
  GetNextReviewItems ReviewType [SrsEntryId]
  deriving (Generic, Show, ToJSON, FromJSON)

instance WebSocketMessage AppRequest GetNextReviewItems where
  type ResponseT AppRequest GetNextReviewItems
    = [ReviewItem]

data ReviewItem = ReviewItem
  { _reviewItemId ::  SrsEntryId
  , _reviewItemField :: SrsEntryField
  , _reviewItemMeaning :: (NonEmpty Meaning, Maybe MeaningNotes)
  , _reviewItemReading :: (NonEmpty Reading, Maybe ReadingNotes)
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data DoReview = DoReview ReviewType [(SrsEntryId, Bool)]
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
  type ResponseT AppRequest GetSrsItem
    = Maybe (SrsEntryId, SrsEntry)

----------------------------------------------------------------
data EditSrsItem = EditSrsItem SrsEntryId SrsEntry
  deriving (Generic, Show, ToJSON, FromJSON)

instance WebSocketMessage AppRequest EditSrsItem where
  type ResponseT AppRequest EditSrsItem = ()

----------------------------------------------------------------
data BulkEditSrsItems = BulkEditSrsItems ReviewType [SrsEntryId] BulkEditOperation
  deriving (Generic, Show, ToJSON, FromJSON)

data BulkEditOperation
  = SuspendSrsItems
  | MarkDueSrsItems
  | ChangeSrsReviewData Day
  | RemoveFromReviewType
  | AddBothReviewType
  | DeleteSrsItems
  deriving (Generic, Show, ToJSON, FromJSON)

instance WebSocketMessage AppRequest BulkEditSrsItems where
  type ResponseT AppRequest BulkEditSrsItems = Maybe ()

----------------------------------------------------------------
data GetAnnotatedText = GetAnnotatedText Text
  deriving (Generic, Show, ToJSON, FromJSON)

instance WebSocketMessage AppRequest GetAnnotatedText where
  type ResponseT AppRequest GetAnnotatedText = AnnotatedText

----------------------------------------------------------------
data GetVocabDetails = GetVocabDetails [VocabId]
  deriving (Generic, Show, ToJSON, FromJSON)

instance WebSocketMessage AppRequest GetVocabDetails where
  type ResponseT AppRequest GetVocabDetails =
    [(Entry, Maybe SrsEntryId)]

----------------------------------------------------------------
makeLenses ''ReviewItem
