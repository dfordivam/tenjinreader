{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Common
  where

import Protolude
-- import GHC.Generics
import Data.Aeson
import Data.Default
import Data.Time (UTCTime)

newtype Kanji = Kanji { unKanji :: Text }
  deriving (Eq, Ord, Generic, Show, ToJSON, FromJSON)

newtype Rank = Rank { unRank :: Int }
  deriving (Eq, Generic, Show, ToJSON, FromJSON)

newtype Meaning = Meaning { unMeaning :: Text }
  deriving (Eq, Generic, Show, ToJSON, FromJSON)

newtype MeaningNotes = MeaningNotes { unMeaningNotes :: Text }
  deriving (Eq, Generic, Show, ToJSON, FromJSON)

newtype Reading = Reading { unReading :: Text }
  deriving (Eq, Generic, Show, ToJSON, FromJSON)

newtype ReadingNotes = ReadingNotes { unReadingNotes :: Text }
  deriving (Eq, Generic, Show, ToJSON, FromJSON)

newtype Grade = Grade { unGrade :: Int }
  deriving (Eq, Generic, Show, ToJSON, FromJSON)

newtype StrokeCount = StrokeCount { unStrokeCount :: Int }
  deriving (Eq, Generic, Show, ToJSON, FromJSON)

newtype JlptLevel = JlptLevel { unJlptLevel :: Int }
  deriving (Eq, Generic, Show, ToJSON, FromJSON)

newtype WikiRank = WikiRank { unWikiRank :: Int }
  deriving (Eq, Generic, Show, ToJSON, FromJSON)

newtype WkLevel = WkLevel { unWkLevel :: Int }
  deriving (Eq, Generic, Show, ToJSON, FromJSON)

newtype RadicalId = RadicalId { unRadicalId :: Int }
  deriving (Eq, Ord, Generic, Show, ToJSON, FromJSON)

newtype KanjiId = KanjiId { unKanjiId :: Int }
  deriving (Eq, Ord, Generic, Show, ToJSON, FromJSON)

newtype SrsItemId = SrsItemId { unSrsItemId :: Int }
  deriving (Eq, Ord, Generic, Show, ToJSON, FromJSON)

newtype SrsLevel = SrsLevel { unSrsLevel :: Int }
  deriving (Eq, Ord, Generic, Show, ToJSON, FromJSON)

newtype Vocab = Vocab { unVocab :: [KanjiOrKana] }
  deriving (Eq, Ord, Generic, Show, ToJSON, FromJSON)

data KanjiOrKana
  = KanjiWithReading Kanji Text
  | Kana Text
  deriving (Eq, Ord, Generic, Show, ToJSON, FromJSON)

data KanjiDetails =
  KanjiDetails Kanji
               (Maybe Rank)
               ([Meaning])
               (Maybe Grade)
               (Maybe JlptLevel)
               (Maybe WkLevel)
               ([(ReadingType, Reading)])
  deriving (Eq, Generic, Show, ToJSON, FromJSON)

data AdditionalFilter = AdditionalFilter
  { readingKana :: Text
  , readingType :: ReadingType
  , meaningText :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance Default AdditionalFilter where
  def = AdditionalFilter "" KunYomi ""

data ReadingType = OnYomi | KunYomi | Nanori
  deriving (Eq, Ord, Generic, Show, ToJSON, FromJSON)

data SrsItem = SrsItem
 {
   srsItemId :: SrsItemId
 , srsVocabOrKanji :: Either Vocab Kanji
 , srsItemSuspended :: Bool
 , srsItemPendingReview :: Bool
 }
  deriving (Generic, Show, ToJSON, FromJSON)

data SrsItemFull = SrsItemFull
  { srsItemFullId :: SrsItemId
  , srsItemFullVocabOrKanji :: Either Vocab Kanji
  , srsReviewDate :: (Maybe UTCTime)
  , srsMeanings :: (Text)
  , srsReadings :: (Text)
  , srsCurrentGrade :: (Int)
  , srsMeaningNote :: (Maybe Text)
  , srsReadingNote :: (Maybe Text)
  , srsTags :: (Maybe Text)
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data SrsReviewStats = SrsReviewStats
  { srsReviewStats_pendingCount :: Int
  , srsReviewStats_correctCount :: Int
  , srsReviewStats_incorrectCount :: Int
  }
  deriving (Generic, Show, ToJSON, FromJSON)
