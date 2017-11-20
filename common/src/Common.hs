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
import Control.Lens.TH
import Data.Aeson
import Data.Default
import Data.Time (UTCTime)
import Data.Binary

newtype Kanji = Kanji { unKanji :: Text }
  deriving (Eq, Ord, Generic, Show, ToJSON, FromJSON, Binary)

newtype Rank = Rank { unRank :: Int }
  deriving (Eq, Generic, Show, ToJSON, FromJSON, Binary)

newtype Meaning = Meaning { unMeaning :: Text }
  deriving (Eq, Generic, Show, ToJSON, FromJSON, Binary)

newtype MeaningNotes = MeaningNotes { unMeaningNotes :: Text }
  deriving (Eq, Generic, Show, ToJSON, FromJSON, Binary)

newtype Reading = Reading { unReading :: Text }
  deriving (Eq, Generic, Show, ToJSON, FromJSON, Binary)

newtype ReadingNotes = ReadingNotes { unReadingNotes :: Text }
  deriving (Eq, Generic, Show, ToJSON, FromJSON, Binary)

newtype Grade = Grade { unGrade :: Int }
  deriving (Eq, Generic, Show, ToJSON, FromJSON, Binary)

newtype StrokeCount = StrokeCount { unStrokeCount :: Int }
  deriving (Eq, Generic, Show, ToJSON, FromJSON, Binary)

newtype JlptLevel = JlptLevel { unJlptLevel :: Int }
  deriving (Eq, Generic, Show, ToJSON, FromJSON, Binary)

newtype WikiRank = WikiRank { unWikiRank :: Int }
  deriving (Eq, Generic, Show, ToJSON, FromJSON, Binary)

newtype WkLevel = WkLevel { unWkLevel :: Int }
  deriving (Eq, Generic, Show, ToJSON, FromJSON, Binary)

newtype RadicalId = RadicalId { unRadicalId :: Int }
  deriving (Eq, Ord, Generic, Show, ToJSON, FromJSON, Binary)

newtype KanjiId = KanjiId { unKanjiId :: Int }
  deriving (Eq, Ord, Generic, Show, ToJSON, FromJSON, Binary)

newtype VocabId = VocabId { unVocabId :: Int }
  deriving (Eq, Ord, Generic, Show, ToJSON, FromJSON, Binary)

newtype SrsItemId = SrsItemId { unSrsItemId :: Int64 }
  deriving (Eq, Ord, Generic, Show, ToJSON, FromJSON)

newtype SrsLevel = SrsLevel { unSrsLevel :: Int }
  deriving (Eq, Ord, Generic, Show, ToJSON, FromJSON, Binary)

newtype Vocab = Vocab { unVocab :: [KanjiOrKana] }
  deriving (Eq, Ord, Generic, Show, ToJSON, FromJSON, Binary)

data KanjiOrKana
  = KanjiWithReading Kanji Text
  | Kana Text
  deriving (Eq, Ord, Generic, Show, ToJSON, FromJSON, Binary)

data KanjiDetails = KanjiDetails
  { _kanjiId             :: KanjiId
  , _kanjiCharacter      :: Kanji
  , _kanjiGrade          :: Maybe Grade
  , _kanjiMostUsedRank   :: Maybe Rank
  , _kanjiJlptLevel      :: Maybe JlptLevel
  , _kanjiOnyomi         :: [Reading]
  , _kanjiKunyomi        :: [Reading]
  , _kanjiNanori         :: [Reading]
  , _kanjiWkLevel        :: Maybe WkLevel
  , _kanjiMeanings       :: [Meaning]
  }
  deriving (Eq, Generic, Show, ToJSON, FromJSON, Binary)

makeLenses ''KanjiDetails

data VocabDetails = VocabDetails
  { _vocabId             :: VocabId
  , _vocab               :: Vocab
  , _vocabIsCommon       :: Bool
  , _vocabFreqRank       :: Maybe Rank
  , _vocabJlptLevel      :: Maybe JlptLevel
  , _vocabWkLevel        :: Maybe WkLevel
  , _vocabWikiRank       :: Maybe WikiRank
  , _vocabMeanings       :: [Meaning]
  }
  deriving (Generic, Show, ToJSON, FromJSON, Binary)

makeLenses ''VocabDetails

data AdditionalFilter = AdditionalFilter
  { readingKana :: Text
  , readingType :: ReadingType
  , meaningText :: Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance Default AdditionalFilter where
  def = AdditionalFilter "" KunYomi ""

data ReadingType = OnYomi | KunYomi | Nanori
  deriving (Eq, Ord, Generic, Show, ToJSON, FromJSON, Binary)

-- Used in Srs browse widget to show list of items
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
  { _srsReviewStats_pendingCount :: Int
  , _srsReviewStats_correctCount :: Int
  , _srsReviewStats_incorrectCount :: Int
  }
  deriving (Generic, Show, ToJSON, FromJSON)

makeLenses ''SrsReviewStats

instance Default SrsReviewStats where
  def = SrsReviewStats 0 0 0

data ReviewType =
  MeaningReview | ReadingReview
  deriving (Eq, Enum, Bounded, Generic, Show, ToJSON, FromJSON)
