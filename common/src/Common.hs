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

import Protolude hiding (to)
-- import GHC.Generics
import Control.Lens.TH
import Control.Lens hiding (reviews)
import Data.Aeson hiding (Value)
import Data.Default
import Data.Binary
import Data.Time.Calendar
import Data.BTree.Primitives (Value)
import Data.JMDict.AST
import Data.List.NonEmpty (NonEmpty(..))
import DerivingInstances ()
import qualified Data.Text as T
import NLP.Japanese.Utils
import Data.These

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

type SrsEntryField = NonEmpty Text

-- Used in Srs browse widget to show list of items
data SrsItem = SrsItem
 {
   srsItemId :: SrsEntryId
 , srsItemField :: SrsEntryField
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

type AnnotatedText = [[(Either Text (Vocab, [VocabId], Bool))]]

-- SrsEntry

newtype SrsInterval = SrsInterval { unSrsInterval :: Integer }
  deriving (Generic, Show, Typeable, ToJSON, FromJSON)

-- If the user suspends a card and then resume later
-- 1. It was due when suspended -> make immediately available for review
-- 2. not due -> no suspend?

data SrsEntryState = NewReview |
  Suspended SrsInterval | NextReviewDate Day SrsInterval
  deriving (Generic, Show, Typeable, ToJSON, FromJSON)

-- SRS algo
-- Correct Answer ->
--   (answer date - due date + last interval) * ease factor
-- Wrong Answer ->
--   last interval * ease factor

-- ease factor depends on SrsEntryStats

data SrsEntryStats = SrsEntryStats
  { _failureCount :: Int
  , _successCount :: Int
  } deriving (Generic, Show, Typeable, ToJSON, FromJSON)

-- By Default do
-- Prod + Recog(M + R) for Vocab with kanji in reading (Can be decided on FE)
-- Prod + Recog(M) for Vocab with only kana reading
-- Recog - for Kanji review
--
-- The default field will be chosen
-- 1. From user entered text
-- 2. Vocab with maximum kanjis
data SrsEntry = SrsEntry
  {  _reviewState :: These (SrsEntryState, SrsEntryStats) (SrsEntryState, SrsEntryStats)
  -- XXX Does this require grouping
  -- readings also contain other/alternate readings
   , _readings :: NonEmpty Reading
   , _meaning :: NonEmpty Meaning
   , _readingNotes :: Maybe ReadingNotes
   , _meaningNotes :: Maybe MeaningNotes
   , _field :: SrsEntryField
  } deriving (Generic, Show, Typeable, ToJSON, FromJSON)

-- APIs -- may be move from here

makeFurigana :: KanjiPhrase -> ReadingPhrase -> Either Text Vocab
makeFurigana (KanjiPhrase k) (ReadingPhrase r) = Vocab
  <$> (g (map katakanaToHiragana kgs) (katakanaToHiragana r))
  where
    g kgs r = case reverse kgs of
      (kl:krev) -> case T.stripSuffix kl r of
        (Just prfx) -> (\x -> x ++ [Kana kl]) <$> f (reverse krev) prfx
        Nothing -> f kgs r

    kgs = T.groupBy (\ a b -> (isKana a) == (isKana b)) k
    f :: [Text] -> Text -> Either Text [KanjiOrKana]
    f [] r
      | T.null r = Right []
      | otherwise = Right [Kana r]

    f (kg:[]) r
      | T.null r = Left "Found kg, but r is T.null"
      | otherwise = if kg == r
        then Right [Kana r]
        else if (isKana (T.head kg))
          then Left $ "Found kana not equal to r: " <> kg <> ", " <> r
          else Right [KanjiWithReading (Kanji kg) r]

    f (kg:kg2:kgs) r
      | T.null r = Left "r is null"
      | otherwise = if (isKana (T.head kg))
        then case (T.stripPrefix kg r) of
          (Just rs) -> ((Kana kg) :) <$> (f (kg2:kgs) rs)
          Nothing -> Left $ "stripPrefix: " <> kg <> ", " <> r
        else case (T.breakOn kg2 (T.tail r)) of
          (rk, rs)
            -> (KanjiWithReading (Kanji kg) (T.cons (T.head r) rk) :) <$> (f (kg2:kgs) rs)

testMakeFurigana = map (\(a,b) -> makeFurigana (KanjiPhrase a) (ReadingPhrase b))
  [("いじり回す", "いじりまわす")
  ,("弄りまわす", "いじりまわす")
  , ("弄り回す", "いじりまわす")
  , ("いじり回す", "いじりまわ") -- Fail
  , ("窺う", "うかがう")
  , ("黄色い", "きいろい")
  , ("額が少ない", "がくがすくない")
  -- , ("霞ヶ関", "かすみがせき")  -- Reading with no kanji
  -- , ("霞ケ関", "かすみがせき")  -- Reading with no kanji
  , ("ケント紙", "ケントし")
  , ("二酸化ケイ素", "にさんかケイそ")
  , ("ページ違反", "ぺーじいはん")
  , ("シェリー酒", "シェリーしゅ")
  ]

-- isSameAs t1 t2
--   | T.length t1 == T.length t2 = all compareChars (zip (T.unpack t1) (T.unpack t2))
--   | otherwise = False

-- isSameAsC c1 c2 = compareChars (c1, c2)

-- compareChars = f
--   where
--     f ('ヶ', c2) = elem c2 ['か', 'が','ヶ', 'ケ']
--     f ('ケ', c2) = elem c2 ['か', 'が','ヶ', 'ケ']
--     f (c1, c2) = c1 == c2
makeLenses ''SrsReviewStats
makeLenses ''VocabDetails
makeLenses ''KanjiDetails

makePrisms ''SrsEntryState
makeLenses ''SrsEntryStats
makeLenses ''SrsEntry


reviewStateL :: (Profunctor p, Contravariant f)
  => ReviewType
  -> Optic' p f SrsEntry (Maybe (SrsEntryState, SrsEntryStats))
reviewStateL ReviewTypeRecogReview
  = to (\r -> (r ^? reviewState . _This)
    <|> (r ^? reviewState . _These . _1))

reviewStateL ReviewTypeProdReview
  = to (\r -> (r ^? reviewState . _That)
    <|> (r ^? reviewState . _These . _2))
