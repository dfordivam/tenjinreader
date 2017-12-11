{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Common
  (module Common
  , module Data.JMDict.AST)
  where

import Protolude
-- import GHC.Generics
import Control.Lens.TH
import Data.Aeson hiding (Value)
import Data.Default
import Data.Binary
import Data.Time.Calendar
import Data.BTree.Primitives (Value)
import Data.JMDict.AST
import Data.List.NonEmpty (NonEmpty(..))

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

type VocabId = EntryId
-- newtype VocabId = VocabId { unVocabId :: Int }
--   deriving (Eq, Ord, Generic, Show, ToJSON, FromJSON, Binary, Value)

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


data VocabDetails = VocabDetails
  { _vocabId             :: VocabId
  , _vocab               :: Vocab
  , _vocabIsCommon       :: Bool
  , _vocabFreqRank       :: Maybe Rank
  , _vocabMeanings       :: [Meaning]
  }
  deriving (Generic, Show, ToJSON, FromJSON, Binary, Value)

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

instance Default SrsReviewStats where
  def = SrsReviewStats 0 0 0

data ReviewType =
    ReviewTypeRecogReview
  | ReviewTypeProdReview
  deriving (Eq, Ord, Enum, Bounded, Generic, Show, ToJSON, FromJSON)


deriving instance Generic VocabId
deriving instance ToJSON VocabId
deriving instance FromJSON VocabId
deriving instance Binary VocabId
deriving instance Value VocabId

deriving instance Generic Sense
deriving instance ToJSON Sense
deriving instance FromJSON Sense
deriving instance Binary Sense
deriving instance Value Sense

deriving instance Generic Entry
deriving instance ToJSON Entry
deriving instance FromJSON Entry
deriving instance Binary Entry
deriving instance Value Entry

deriving instance Generic KanjiElement
deriving instance ToJSON KanjiElement
deriving instance FromJSON KanjiElement
deriving instance Binary KanjiElement
deriving instance Value KanjiElement

deriving instance Generic ReadingElement
deriving instance ToJSON ReadingElement
deriving instance FromJSON ReadingElement
deriving instance Binary ReadingElement
deriving instance Value ReadingElement

deriving instance Generic Priority
deriving instance ToJSON Priority
deriving instance FromJSON Priority
deriving instance Binary Priority
deriving instance Value Priority

deriving instance Generic ReadingPhrase
deriving instance ToJSON ReadingPhrase
deriving instance FromJSON ReadingPhrase
deriving instance Binary ReadingPhrase
deriving instance Value ReadingPhrase

deriving instance Generic KanjiPhrase
deriving instance ToJSON KanjiPhrase
deriving instance FromJSON KanjiPhrase
deriving instance Binary KanjiPhrase
deriving instance Value KanjiPhrase

deriving instance Generic KanjiInfo
deriving instance ToJSON KanjiInfo
deriving instance FromJSON KanjiInfo
deriving instance Binary KanjiInfo
deriving instance Value KanjiInfo

deriving instance Generic ReadingInfo
deriving instance ToJSON ReadingInfo
deriving instance FromJSON ReadingInfo
deriving instance Binary ReadingInfo
deriving instance Value ReadingInfo

deriving instance Generic PartOfSpeech
deriving instance ToJSON PartOfSpeech
deriving instance FromJSON PartOfSpeech
deriving instance Binary PartOfSpeech
deriving instance Value PartOfSpeech

deriving instance Generic Auxiliary
deriving instance ToJSON Auxiliary
deriving instance FromJSON Auxiliary
deriving instance Binary Auxiliary
deriving instance Value Auxiliary

deriving instance Generic NounType
deriving instance ToJSON NounType
deriving instance FromJSON NounType
deriving instance Binary NounType
deriving instance Value NounType

deriving instance Generic VerbType
deriving instance ToJSON VerbType
deriving instance FromJSON VerbType
deriving instance Binary VerbType
deriving instance Value VerbType

deriving instance Generic Dialect
deriving instance ToJSON Dialect
deriving instance FromJSON Dialect
deriving instance Binary Dialect
deriving instance Value Dialect

deriving instance Generic Gloss
deriving instance ToJSON Gloss
deriving instance FromJSON Gloss
deriving instance Binary Gloss
deriving instance Value Gloss

deriving instance Generic Field
deriving instance ToJSON Field
deriving instance FromJSON Field
deriving instance Binary Field
deriving instance Value Field

deriving instance Generic LanguageSource
deriving instance ToJSON LanguageSource
deriving instance FromJSON LanguageSource
deriving instance Binary LanguageSource
deriving instance Value LanguageSource

deriving instance Generic SenseMisc
deriving instance ToJSON SenseMisc
deriving instance FromJSON SenseMisc
deriving instance Binary SenseMisc
deriving instance Value SenseMisc

deriving instance Generic Adjective
deriving instance ToJSON Adjective
deriving instance FromJSON Adjective
deriving instance Binary Adjective
deriving instance Value Adjective

deriving instance Generic Adverb
deriving instance ToJSON Adverb
deriving instance FromJSON Adverb
deriving instance Binary Adverb
deriving instance Value Adverb

deriving instance Generic SpecialVerb
deriving instance ToJSON SpecialVerb
deriving instance FromJSON SpecialVerb
deriving instance Binary SpecialVerb
deriving instance Value SpecialVerb

deriving instance Generic RegularVerb
deriving instance ToJSON RegularVerb
deriving instance FromJSON RegularVerb
deriving instance Binary RegularVerb
deriving instance Value RegularVerb

deriving instance Generic IrregularVerb
deriving instance ToJSON IrregularVerb
deriving instance FromJSON IrregularVerb
deriving instance Binary IrregularVerb
deriving instance Value IrregularVerb

deriving instance Generic VerbEnding
deriving instance ToJSON VerbEnding
deriving instance FromJSON VerbEnding
deriving instance Binary VerbEnding
deriving instance Value VerbEnding

deriving instance Generic IsTransitive
deriving instance ToJSON IsTransitive
deriving instance FromJSON IsTransitive
deriving instance Binary IsTransitive
deriving instance Value IsTransitive

deriving instance Binary (NonEmpty ReadingElement)
makeLenses ''SrsReviewStats
makeLenses ''VocabDetails
makeLenses ''KanjiDetails
