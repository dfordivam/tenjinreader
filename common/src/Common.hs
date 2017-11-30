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
import Data.Aeson hiding (Value)
import Data.Default
import Data.Binary
import Data.Time.Calendar
import Data.BTree.Primitives (Value)

instance Value Int
instance Value a => Value (Maybe a)

newtype Kanji = Kanji { unKanji :: Text }
  deriving (Eq, Ord, Generic, Show, ToJSON, FromJSON, Binary, Value)

newtype Rank = Rank { unRank :: Int }
  deriving (Eq, Ord, Generic, Show, ToJSON, FromJSON, Binary, Value)

newtype Meaning = Meaning { unMeaning :: Text }
  deriving (Eq, Generic, Show, ToJSON, FromJSON, Binary, Value)

newtype MeaningNotes = MeaningNotes { unMeaningNotes :: Text }
  deriving (Eq, Generic, Show, ToJSON, FromJSON, Binary, Value)

newtype Reading = Reading { unReading :: Text }
  deriving (Eq, Generic, Show, ToJSON, FromJSON, Binary, Value)

newtype ReadingNotes = ReadingNotes { unReadingNotes :: Text }
  deriving (Eq, Generic, Show, ToJSON, FromJSON, Binary, Value)

newtype Grade = Grade { unGrade :: Int }
  deriving (Eq, Ord, Generic, Show, ToJSON, FromJSON, Binary, Value)

newtype StrokeCount = StrokeCount { unStrokeCount :: Int }
  deriving (Eq, Generic, Show, ToJSON, FromJSON, Binary, Value)

newtype JlptLevel = JlptLevel { unJlptLevel :: Int }
  deriving (Eq, Ord, Generic, Show, ToJSON, FromJSON, Binary, Value)

newtype WikiRank = WikiRank { unWikiRank :: Int }
  deriving (Eq, Ord, Generic, Show, ToJSON, FromJSON, Binary, Value)

newtype WkLevel = WkLevel { unWkLevel :: Int }
  deriving (Eq, Ord, Generic, Show, ToJSON, FromJSON, Binary, Value)

newtype RadicalId = RadicalId { unRadicalId :: Int }
  deriving (Eq, Ord, Generic, Show, ToJSON, FromJSON, Binary, Value)

newtype KanjiId = KanjiId { unKanjiId :: Int }
  deriving (Eq, Ord, Generic, Show, ToJSON, FromJSON, Binary, Value)

newtype VocabId = VocabId { unVocabId :: Int }
  deriving (Eq, Ord, Generic, Show, ToJSON, FromJSON, Binary, Value)

newtype SrsEntryId = SrsEntryId { unSrsEntryId :: Int64 }
  deriving (Eq, Ord, Generic, Show, ToJSON, FromJSON, Binary, Value)

newtype SrsLevel = SrsLevel { unSrsLevel :: Int }
  deriving (Eq, Ord, Generic, Show, ToJSON, FromJSON, Binary, Value)

newtype Vocab = Vocab { unVocab :: [KanjiOrKana] }
  deriving (Eq, Ord, Generic, Show, ToJSON, FromJSON, Binary, Value)

data KanjiOrKana
  = KanjiWithReading Kanji Text
  | Kana Text
  deriving (Eq, Ord, Generic, Show, ToJSON, FromJSON, Binary, Value)

vocabToKana :: Vocab -> Text
vocabToKana (Vocab ks) = mconcat $ map getFur ks
  where
    getFur (KanjiWithReading _ t) = t
    getFur (Kana t) = t

getVocabField:: Vocab -> Text
getVocabField (Vocab ks) = mconcat $ map f ks
  where f (Kana t) = t
        f (KanjiWithReading k _) = unKanji k

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
  deriving (Eq, Generic, Show, ToJSON, FromJSON, Binary, Value)

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
  deriving (Generic, Show, ToJSON, FromJSON, Binary, Value)

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
  deriving (Eq, Ord, Generic, Show, ToJSON, FromJSON, Binary, Value)

-- Used in Srs browse widget to show list of items
data SrsItem = SrsItem
 {
   srsItemId :: SrsEntryId
 , srsItemField :: Text
 }
  deriving (Generic, Show, ToJSON, FromJSON)

data SrsItemFull = SrsItemFull
  { srsItemFullId :: SrsEntryId
  , srsItemFullVocabOrKanji :: Either Vocab Kanji
  , srsReviewDate :: (Maybe Day)
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
  deriving (Eq, Ord, Enum, Bounded, Generic, Show, ToJSON, FromJSON)
