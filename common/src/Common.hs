{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Common
  (module Common
  , module Data.JMDict.AST)
  where

import Protolude hiding (to, (&))
-- import GHC.Generics
import Control.Lens hiding (reviews)
import Data.Aeson hiding (Value)
import Data.Default
import Data.Time.Calendar
import Data.JMDict.AST
import Data.List.NonEmpty (NonEmpty(..))
import DerivingInstances ()
import qualified Data.Text as T
import NLP.Japanese.Utils
import Data.These
import Data.Vector (Vector)

data CurrentDb = CurrentDb
  deriving (Generic, Show, ToJSON, FromJSON)

data OldDb = OldDb
  deriving (Generic, Show, ToJSON, FromJSON)

newtype Kanji = Kanji { unKanji :: Text }
  deriving (Eq, Ord, Generic, Show, ToJSON, FromJSON)

newtype Rank = Rank { unRank :: Int }
  deriving (Eq, Ord, Generic, Show, ToJSON, FromJSON)

newtype Meaning = Meaning { unMeaning :: Text }
  deriving (Eq, Generic, Show, ToJSON, FromJSON)

newtype MeaningNotes = MeaningNotes { unMeaningNotes :: Text }
  deriving (Eq, Generic, Show, ToJSON, FromJSON)

newtype Reading = Reading { unReading :: Text }
  deriving (Eq, Generic, Show, ToJSON, FromJSON)

newtype ReadingNotes = ReadingNotes { unReadingNotes :: Text }
  deriving (Eq, Generic, Show, ToJSON, FromJSON)

newtype Grade = Grade { unGrade :: Int }
  deriving (Eq, Ord, Generic, Show, ToJSON, FromJSON)

newtype StrokeCount = StrokeCount { unStrokeCount :: Int }
  deriving (Eq, Generic, Show, ToJSON, FromJSON)

newtype JlptLevel = JlptLevel { unJlptLevel :: Int }
  deriving (Eq, Ord, Generic, Show, ToJSON, FromJSON)

newtype WikiRank = WikiRank { unWikiRank :: Int }
  deriving (Eq, Ord, Generic, Show, ToJSON, FromJSON)

newtype WkLevel = WkLevel { unWkLevel :: Int }
  deriving (Eq, Ord, Generic, Show, ToJSON, FromJSON)

newtype RadicalId = RadicalId { unRadicalId :: Int }
  deriving (Eq, Ord, Generic, Show, ToJSON, FromJSON)

newtype KanjiId = KanjiId { unKanjiId :: Int }
  deriving (Eq, Ord, Generic, Show, ToJSON, FromJSON)

type VocabId = EntryId
-- newtype VocabId = VocabId { unVocabId :: Int }
--   deriving (Eq, Ord, Generic, Show, ToJSON, FromJSON, Binary, Value)

newtype SrsEntryId = SrsEntryId { unSrsEntryId :: Int64 }
  deriving (Eq, Ord, Generic, Show, ToJSON, FromJSON)

newtype SentenceId = SentenceId { unSentenceId :: Int64 }
  deriving (Eq, Ord, Generic, Show, ToJSON, FromJSON)

newtype SrsLevel = SrsLevel { unSrsLevel :: Int }
  deriving (Eq, Ord, Generic, Show, ToJSON, FromJSON)

newtype Vocab = Vocab { unVocab :: [KanjiOrKana] }
  deriving (Eq, Ord, Generic, Show, ToJSON, FromJSON)

data KanjiOrKana
  = KanjiWithReading Kanji Text
  | Kana Text
  deriving (Eq, Ord, Generic, Show, ToJSON, FromJSON)

vocabToKana :: Vocab -> Text
vocabToKana (Vocab ks) = mconcat $ map getFur ks
  where
    getFur (KanjiWithReading _ t) = t
    getFur (Kana t) = t

vocabToText :: Vocab -> Text
vocabToText (Vocab ks) = mconcat $ map f ks
  where f (KanjiWithReading (Kanji k) _) = k
        f (Kana k) = k

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
  deriving (Eq, Generic, Show, ToJSON, FromJSON)


data VocabDetails = VocabDetails
  { _vocabId             :: VocabId
  , _vocab               :: Vocab
  , _vocabIsCommon       :: Bool
  , _vocabFreqRank       :: Maybe Rank
  , _vocabMeanings       :: [Meaning]
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data SentenceData = SentenceData
  { _sentenceContents :: NonEmpty AnnotatedPara
  , _sentenceLinkedEng :: [Text]
  }
  deriving (Generic, Show, ToJSON, FromJSON)

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

type AnnotatedPara = [(Either Text (Vocab, [VocabId], Bool))]

type AnnotatedDocument = Vector AnnotatedPara

newtype ReaderDocumentId = ReaderDocumentId { unReaderDocumentId :: Int }
  deriving (Eq, Ord, Generic, Show, Typeable, ToJSON, FromJSON)

newtype BookId = BookId { unBookId :: Int }
  deriving (Eq, Ord, Generic, Show, Typeable, ToJSON, FromJSON)

newtype ArticleId = ArticleId { unArticleId :: Int }
  deriving (Eq, Ord, Generic, Show, Typeable, ToJSON, FromJSON)

type family ReaderSettings t
type instance ReaderSettings t
  = ReaderSettingsTree t

data ReaderSettingsTree t = ReaderSettings
  { _fontSize  :: Int
  , _rubySize  :: Int
  , _lineHeight :: Int
  , _verticalMode :: Bool
  , _numOfLines :: Int
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance Default (ReaderSettingsTree t) where
  def = ReaderSettings 120 105 150 False 400

capitalize :: Text -> Text
capitalize m = T.unwords $ T.words m & _head  %~ f
  where
    f t
      | T.head t == ('-') = t
      | elem t ignoreList = t
      | otherwise = T.toTitle t
    ignoreList = ["to", "a", "an"]

showSense :: Sense -> Text
showSense s = mconcat
      [ showPos $ s ^.. sensePartOfSpeech . traverse
      , p $ s ^.. senseInfo . traverse
      , showGlosses $ take 5 $ s ^.. senseGlosses . traverse . glossDefinition]
  where
    p [] = ""
    p c = "(" <> (T.intercalate ", " c) <> ") "

    showGlosses ms = T.intercalate ", " $ map capitalize ms

    showPos ps = p psDesc
      where
        psDesc = catMaybes $ map f ps
        f PosNoun = Just $ "Noun"
        f PosPronoun = Just $ "Pronoun"
        f (PosVerb _ _) = Just $ "Verb"
        f (PosAdverb _) = Just $ "Adv."
        f (PosAdjective _) = Just $ "Adj."
        f PosSuffix = Just $ "Suffix"
        f PosPrefix = Just $ "Prefix"
        f _ = Nothing

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
makeFurigana (KanjiPhrase k) (ReadingPhrase rp) = Vocab
  <$> (g k_gs (katakanaToHiragana rp))
  where
    g kgs r = case reverse kgs of
      ((kl, Just klr):krev) -> case T.stripSuffix kl r of
        (Just prfx) -> (\x -> x ++ [Kana klr]) <$> f (reverse krev) prfx
        Nothing -> Left "stripSuffix issue"
      _ -> f kgs r

    k_gsF = T.groupBy (\ a b -> (isKana a) == (isKana b)) k

    k_gs :: [(Text, Maybe Text)]
    k_gs = (flip map) k_gsF (\grp -> if (isKana $ T.head grp)
                     then (katakanaToHiragana grp, Just grp) -- Store original kana form
                     else (grp, Nothing))

    f :: [(Text, Maybe Text)] -> Text -> Either Text [KanjiOrKana]
    f [] r
      | T.null r = Right []
      | otherwise = Right [Kana r]

    f ((kg, Just kgr):[]) r
      | T.null r = Left "Found kg, but r is T.null"
      | otherwise = if kg == r
        then Right [Kana kgr] -- Take the original
        else Left $ "Founf Just kgr but not equal to r"

    f ((kg, Nothing):[]) r = Right [KanjiWithReading (Kanji kg) r]

    f ((kg, Just kgr):(kg2, Nothing):kgs) r
      | T.null r = Left "r is null"
      | otherwise = case (T.stripPrefix kg r) of
          (Just rs) -> ((Kana kgr) :) <$> (f ((kg2, Nothing):kgs) rs)
          Nothing -> Left $ "stripPrefix: " <> kg <> ", " <> r

    f ((kg, Nothing):(kg2, Just kgr2):kgs) r
      | T.null r = Left "r is null"
      | otherwise = case (T.breakOn kg2 (T.tail r)) of
          (rk, rs)
            -> (KanjiWithReading (Kanji kg) (T.cons (T.head r) rk) :)
            <$> (f ((kg2, Just kgr2):kgs) rs)

    f _ _ = Left "Invalid input, should be grouped by isKana"

testMakeFurigana :: [Either Text Vocab]
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
  , ("パン屋", "ぱんや")
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
-- makeLenses ''SrsReviewStats
srsReviewStats_correctCount :: Lens' SrsReviewStats Int
srsReviewStats_correctCount
  f_a1lkE
  (SrsReviewStats x1_a1lkF x2_a1lkG x3_a1lkH)
  = fmap
      (\ y1_a1lkI -> SrsReviewStats x1_a1lkF y1_a1lkI x3_a1lkH)
      (f_a1lkE x2_a1lkG)
{-# INLINE srsReviewStats_correctCount #-}
srsReviewStats_incorrectCount :: Lens' SrsReviewStats Int
srsReviewStats_incorrectCount
  f_a1lkJ
  (SrsReviewStats x1_a1lkK x2_a1lkL x3_a1lkM)
  = fmap
      (\ y1_a1lkN -> SrsReviewStats x1_a1lkK x2_a1lkL y1_a1lkN)
      (f_a1lkJ x3_a1lkM)
{-# INLINE srsReviewStats_incorrectCount #-}
srsReviewStats_pendingCount :: Lens' SrsReviewStats Int
srsReviewStats_pendingCount
  f_a1lkO
  (SrsReviewStats x1_a1lkP x2_a1lkQ x3_a1lkR)
  = fmap
      (\ y1_a1lkS -> SrsReviewStats y1_a1lkS x2_a1lkQ x3_a1lkR)
      (f_a1lkO x1_a1lkP)
{-# INLINE srsReviewStats_pendingCount #-}

-- makeLenses ''VocabDetails
vocab :: Lens' VocabDetails Vocab
vocab
  f_a1llR
  (VocabDetails x1_a1llS x2_a1llT x3_a1llU x4_a1llV x5_a1llW)
  = fmap
      (\ y1_a1llX
         -> VocabDetails x1_a1llS y1_a1llX x3_a1llU x4_a1llV x5_a1llW)
      (f_a1llR x2_a1llT)
{-# INLINE vocab #-}
vocabFreqRank :: Lens' VocabDetails (Maybe Rank)
vocabFreqRank
  f_a1llY
  (VocabDetails x1_a1llZ x2_a1lm0 x3_a1lm1 x4_a1lm2 x5_a1lm3)
  = fmap
      (\ y1_a1lm4
         -> VocabDetails x1_a1llZ x2_a1lm0 x3_a1lm1 y1_a1lm4 x5_a1lm3)
      (f_a1llY x4_a1lm2)
{-# INLINE vocabFreqRank #-}
vocabId :: Lens' VocabDetails VocabId
vocabId
  f_a1lm5
  (VocabDetails x1_a1lm6 x2_a1lm7 x3_a1lm8 x4_a1lm9 x5_a1lma)
  = fmap
      (\ y1_a1lmb
         -> VocabDetails y1_a1lmb x2_a1lm7 x3_a1lm8 x4_a1lm9 x5_a1lma)
      (f_a1lm5 x1_a1lm6)
{-# INLINE vocabId #-}
vocabIsCommon :: Lens' VocabDetails Bool
vocabIsCommon
  f_a1lmc
  (VocabDetails x1_a1lmd x2_a1lme x3_a1lmf x4_a1lmg x5_a1lmh)
  = fmap
      (\ y1_a1lmi
         -> VocabDetails x1_a1lmd x2_a1lme y1_a1lmi x4_a1lmg x5_a1lmh)
      (f_a1lmc x3_a1lmf)
{-# INLINE vocabIsCommon #-}
vocabMeanings :: Lens' VocabDetails [Meaning]
vocabMeanings
  f_a1lmj
  (VocabDetails x1_a1lmk x2_a1lml x3_a1lmm x4_a1lmn x5_a1lmo)
  = fmap
      (\ y1_a1lmp
         -> VocabDetails x1_a1lmk x2_a1lml x3_a1lmm x4_a1lmn y1_a1lmp)
      (f_a1lmj x5_a1lmo)
{-# INLINE vocabMeanings #-}

-- makeLenses ''KanjiDetails
kanjiCharacter :: Lens' KanjiDetails Kanji
kanjiCharacter
  f_a1lnU
  (KanjiDetails x1_a1lnV
                x2_a1lnW
                x3_a1lnX
                x4_a1lnY
                x5_a1lnZ
                x6_a1lo0
                x7_a1lo1
                x8_a1lo2
                x9_a1lo3
                x10_a1lo4)
  = fmap
      (\ y1_a1lo5
         -> KanjiDetails
              x1_a1lnV
              y1_a1lo5
              x3_a1lnX
              x4_a1lnY
              x5_a1lnZ
              x6_a1lo0
              x7_a1lo1
              x8_a1lo2
              x9_a1lo3
              x10_a1lo4)
      (f_a1lnU x2_a1lnW)
{-# INLINE kanjiCharacter #-}
kanjiGrade :: Lens' KanjiDetails (Maybe Grade)
kanjiGrade
  f_a1lo6
  (KanjiDetails x1_a1lo7
                x2_a1lo8
                x3_a1lo9
                x4_a1loa
                x5_a1lob
                x6_a1loc
                x7_a1lod
                x8_a1loe
                x9_a1lof
                x10_a1log)
  = fmap
      (\ y1_a1loh
         -> KanjiDetails
              x1_a1lo7
              x2_a1lo8
              y1_a1loh
              x4_a1loa
              x5_a1lob
              x6_a1loc
              x7_a1lod
              x8_a1loe
              x9_a1lof
              x10_a1log)
      (f_a1lo6 x3_a1lo9)
{-# INLINE kanjiGrade #-}
kanjiId :: Lens' KanjiDetails KanjiId
kanjiId
  f_a1loi
  (KanjiDetails x1_a1loj
                x2_a1lok
                x3_a1lol
                x4_a1lom
                x5_a1lon
                x6_a1loo
                x7_a1lop
                x8_a1loq
                x9_a1lor
                x10_a1los)
  = fmap
      (\ y1_a1lot
         -> KanjiDetails
              y1_a1lot
              x2_a1lok
              x3_a1lol
              x4_a1lom
              x5_a1lon
              x6_a1loo
              x7_a1lop
              x8_a1loq
              x9_a1lor
              x10_a1los)
      (f_a1loi x1_a1loj)
{-# INLINE kanjiId #-}
kanjiJlptLevel :: Lens' KanjiDetails (Maybe JlptLevel)
kanjiJlptLevel
  f_a1lou
  (KanjiDetails x1_a1lov
                x2_a1low
                x3_a1lox
                x4_a1loy
                x5_a1loz
                x6_a1loA
                x7_a1loB
                x8_a1loC
                x9_a1loD
                x10_a1loE)
  = fmap
      (\ y1_a1loF
         -> KanjiDetails
              x1_a1lov
              x2_a1low
              x3_a1lox
              x4_a1loy
              y1_a1loF
              x6_a1loA
              x7_a1loB
              x8_a1loC
              x9_a1loD
              x10_a1loE)
      (f_a1lou x5_a1loz)
{-# INLINE kanjiJlptLevel #-}
kanjiKunyomi :: Lens' KanjiDetails [Reading]
kanjiKunyomi
  f_a1loG
  (KanjiDetails x1_a1loH
                x2_a1loI
                x3_a1loJ
                x4_a1loK
                x5_a1loL
                x6_a1loM
                x7_a1loN
                x8_a1loO
                x9_a1loP
                x10_a1loQ)
  = fmap
      (\ y1_a1loR
         -> KanjiDetails
              x1_a1loH
              x2_a1loI
              x3_a1loJ
              x4_a1loK
              x5_a1loL
              x6_a1loM
              y1_a1loR
              x8_a1loO
              x9_a1loP
              x10_a1loQ)
      (f_a1loG x7_a1loN)
{-# INLINE kanjiKunyomi #-}
kanjiMeanings :: Lens' KanjiDetails [Meaning]
kanjiMeanings
  f_a1loS
  (KanjiDetails x1_a1loT
                x2_a1loU
                x3_a1loV
                x4_a1loW
                x5_a1loX
                x6_a1loY
                x7_a1loZ
                x8_a1lp0
                x9_a1lp1
                x10_a1lp2)
  = fmap
      (\ y1_a1lp3
         -> KanjiDetails
              x1_a1loT
              x2_a1loU
              x3_a1loV
              x4_a1loW
              x5_a1loX
              x6_a1loY
              x7_a1loZ
              x8_a1lp0
              x9_a1lp1
              y1_a1lp3)
      (f_a1loS x10_a1lp2)
{-# INLINE kanjiMeanings #-}
kanjiMostUsedRank :: Lens' KanjiDetails (Maybe Rank)
kanjiMostUsedRank
  f_a1lp4
  (KanjiDetails x1_a1lp5
                x2_a1lp6
                x3_a1lp7
                x4_a1lp8
                x5_a1lp9
                x6_a1lpa
                x7_a1lpb
                x8_a1lpc
                x9_a1lpd
                x10_a1lpe)
  = fmap
      (\ y1_a1lpf
         -> KanjiDetails
              x1_a1lp5
              x2_a1lp6
              x3_a1lp7
              y1_a1lpf
              x5_a1lp9
              x6_a1lpa
              x7_a1lpb
              x8_a1lpc
              x9_a1lpd
              x10_a1lpe)
      (f_a1lp4 x4_a1lp8)
{-# INLINE kanjiMostUsedRank #-}
kanjiNanori :: Lens' KanjiDetails [Reading]
kanjiNanori
  f_a1lpg
  (KanjiDetails x1_a1lph
                x2_a1lpi
                x3_a1lpj
                x4_a1lpk
                x5_a1lpl
                x6_a1lpm
                x7_a1lpn
                x8_a1lpo
                x9_a1lpp
                x10_a1lpq)
  = fmap
      (\ y1_a1lpr
         -> KanjiDetails
              x1_a1lph
              x2_a1lpi
              x3_a1lpj
              x4_a1lpk
              x5_a1lpl
              x6_a1lpm
              x7_a1lpn
              y1_a1lpr
              x9_a1lpp
              x10_a1lpq)
      (f_a1lpg x8_a1lpo)
{-# INLINE kanjiNanori #-}
kanjiOnyomi :: Lens' KanjiDetails [Reading]
kanjiOnyomi
  f_a1lps
  (KanjiDetails x1_a1lpt
                x2_a1lpu
                x3_a1lpv
                x4_a1lpw
                x5_a1lpx
                x6_a1lpy
                x7_a1lpz
                x8_a1lpA
                x9_a1lpB
                x10_a1lpC)
  = fmap
      (\ y1_a1lpD
         -> KanjiDetails
              x1_a1lpt
              x2_a1lpu
              x3_a1lpv
              x4_a1lpw
              x5_a1lpx
              y1_a1lpD
              x7_a1lpz
              x8_a1lpA
              x9_a1lpB
              x10_a1lpC)
      (f_a1lps x6_a1lpy)
{-# INLINE kanjiOnyomi #-}
kanjiWkLevel :: Lens' KanjiDetails (Maybe WkLevel)
kanjiWkLevel
  f_a1lpE
  (KanjiDetails x1_a1lpF
                x2_a1lpG
                x3_a1lpH
                x4_a1lpI
                x5_a1lpJ
                x6_a1lpK
                x7_a1lpL
                x8_a1lpM
                x9_a1lpN
                x10_a1lpO)
  = fmap
      (\ y1_a1lpP
         -> KanjiDetails
              x1_a1lpF
              x2_a1lpG
              x3_a1lpH
              x4_a1lpI
              x5_a1lpJ
              x6_a1lpK
              x7_a1lpL
              x8_a1lpM
              y1_a1lpP
              x10_a1lpO)
      (f_a1lpE x9_a1lpN)
{-# INLINE kanjiWkLevel #-}

-- makePrisms ''SrsEntryState
_NewReview :: Prism' SrsEntryState ()
_NewReview
  = prism
      (\ () -> NewReview)
      (\ x_a1lwC
         -> case x_a1lwC of
              NewReview -> Right ()
              _ -> Left x_a1lwC )
_Suspended :: Prism' SrsEntryState SrsInterval
_Suspended
  = prism
      (\ x1_a1lwD -> Suspended x1_a1lwD)
      (\ x_a1lwE
         -> case x_a1lwE of
              Suspended y1_a1lwF -> Right y1_a1lwF
              _ -> Left x_a1lwE )
_NextReviewDate :: Prism' SrsEntryState (Day, SrsInterval)
_NextReviewDate
  = prism
      (\ (x1_a1lwG, x2_a1lwH) -> NextReviewDate x1_a1lwG x2_a1lwH)
      (\ x_a1lwI
         -> case x_a1lwI of
              NextReviewDate y1_a1lwJ y2_a1lwK -> Right (y1_a1lwJ, y2_a1lwK)
              _ -> Left x_a1lwI )

-- makeLenses ''SrsEntryStats
failureCount :: Lens' SrsEntryStats Int
failureCount f_a1lJm (SrsEntryStats x1_a1lJn x2_a1lJo)
  = fmap
      (\ y1_a1lJp -> SrsEntryStats y1_a1lJp x2_a1lJo) (f_a1lJm x1_a1lJn)
{-# INLINE failureCount #-}
successCount :: Lens' SrsEntryStats Int
successCount f_a1lJq (SrsEntryStats x1_a1lJr x2_a1lJs)
  = fmap
      (\ y1_a1lJt -> SrsEntryStats x1_a1lJr y1_a1lJt) (f_a1lJq x2_a1lJs)
{-# INLINE successCount #-}

-- makeLenses ''SrsEntry
field :: Lens' SrsEntry SrsEntryField
field
  f_a1lK9
  (SrsEntry x1_a1lKa x2_a1lKb x3_a1lKc x4_a1lKd x5_a1lKe x6_a1lKf)
  = fmap
      (\ y1_a1lKg
         -> SrsEntry x1_a1lKa x2_a1lKb x3_a1lKc x4_a1lKd x5_a1lKe y1_a1lKg)
      (f_a1lK9 x6_a1lKf)
{-# INLINE field #-}
meaning :: Lens' SrsEntry (NonEmpty Meaning)
meaning
  f_a1lKh
  (SrsEntry x1_a1lKi x2_a1lKj x3_a1lKk x4_a1lKl x5_a1lKm x6_a1lKn)
  = fmap
      (\ y1_a1lKo
         -> SrsEntry x1_a1lKi x2_a1lKj y1_a1lKo x4_a1lKl x5_a1lKm x6_a1lKn)
      (f_a1lKh x3_a1lKk)
{-# INLINE meaning #-}
meaningNotes :: Lens' SrsEntry (Maybe MeaningNotes)
meaningNotes
  f_a1lKp
  (SrsEntry x1_a1lKq x2_a1lKr x3_a1lKs x4_a1lKt x5_a1lKu x6_a1lKv)
  = fmap
      (\ y1_a1lKw
         -> SrsEntry x1_a1lKq x2_a1lKr x3_a1lKs x4_a1lKt y1_a1lKw x6_a1lKv)
      (f_a1lKp x5_a1lKu)
{-# INLINE meaningNotes #-}
readingNotes :: Lens' SrsEntry (Maybe ReadingNotes)
readingNotes
  f_a1lKx
  (SrsEntry x1_a1lKy x2_a1lKz x3_a1lKA x4_a1lKB x5_a1lKC x6_a1lKD)
  = fmap
      (\ y1_a1lKE
         -> SrsEntry x1_a1lKy x2_a1lKz x3_a1lKA y1_a1lKE x5_a1lKC x6_a1lKD)
      (f_a1lKx x4_a1lKB)
{-# INLINE readingNotes #-}
readings :: Lens' SrsEntry (NonEmpty Reading)
readings
  f_a1lKF
  (SrsEntry x1_a1lKG x2_a1lKH x3_a1lKI x4_a1lKJ x5_a1lKK x6_a1lKL)
  = fmap
      (\ y1_a1lKM
         -> SrsEntry x1_a1lKG y1_a1lKM x3_a1lKI x4_a1lKJ x5_a1lKK x6_a1lKL)
      (f_a1lKF x2_a1lKH)
{-# INLINE readings #-}
reviewState ::
  Lens' SrsEntry (These (SrsEntryState,
                         SrsEntryStats) (SrsEntryState, SrsEntryStats))
reviewState
  f_a1lKN
  (SrsEntry x1_a1lKO x2_a1lKP x3_a1lKQ x4_a1lKR x5_a1lKS x6_a1lKT)
  = fmap
      (\ y1_a1lKU
         -> SrsEntry y1_a1lKU x2_a1lKP x3_a1lKQ x4_a1lKR x5_a1lKS x6_a1lKT)
      (f_a1lKN x1_a1lKO)
{-# INLINE reviewState #-}

-- makeLenses ''ReaderSettingsTree
fontSize ::
  Lens (ReaderSettingsTree t_a1hFt) (ReaderSettingsTree t_a1lMG) Int Int
fontSize
  f_a1lML
  (ReaderSettings x1_a1lMM x2_a1lMN x3_a1lMO x4_a1lMP x5_a1lMQ)
  = fmap
      (\ y1_a1lMR
         -> ReaderSettings y1_a1lMR x2_a1lMN x3_a1lMO x4_a1lMP x5_a1lMQ)
      (f_a1lML x1_a1lMM)
{-# INLINE fontSize #-}
lineHeight ::
  Lens (ReaderSettingsTree t_a1hFt) (ReaderSettingsTree t_a1lMH) Int Int
lineHeight
  f_a1lMS
  (ReaderSettings x1_a1lMT x2_a1lMU x3_a1lMV x4_a1lMW x5_a1lMX)
  = fmap
      (\ y1_a1lMY
         -> ReaderSettings x1_a1lMT x2_a1lMU y1_a1lMY x4_a1lMW x5_a1lMX)
      (f_a1lMS x3_a1lMV)
{-# INLINE lineHeight #-}
numOfLines ::
  Lens (ReaderSettingsTree t_a1hFt) (ReaderSettingsTree t_a1lMI) Int Int
numOfLines
  f_a1lMZ
  (ReaderSettings x1_a1lN0 x2_a1lN1 x3_a1lN2 x4_a1lN3 x5_a1lN4)
  = fmap
      (\ y1_a1lN5
         -> ReaderSettings x1_a1lN0 x2_a1lN1 x3_a1lN2 x4_a1lN3 y1_a1lN5)
      (f_a1lMZ x5_a1lN4)
{-# INLINE numOfLines #-}
rubySize ::
  Lens (ReaderSettingsTree t_a1hFt) (ReaderSettingsTree t_a1lMJ) Int Int
rubySize
  f_a1lN6
  (ReaderSettings x1_a1lN7 x2_a1lN8 x3_a1lN9 x4_a1lNa x5_a1lNb)
  = fmap
      (\ y1_a1lNc
         -> ReaderSettings x1_a1lN7 y1_a1lNc x3_a1lN9 x4_a1lNa x5_a1lNb)
      (f_a1lN6 x2_a1lN8)
{-# INLINE rubySize #-}
verticalMode ::
  Lens (ReaderSettingsTree t_a1hFt) (ReaderSettingsTree t_a1lMK) Bool Bool
verticalMode
  f_a1lNd
  (ReaderSettings x1_a1lNe x2_a1lNf x3_a1lNg x4_a1lNh x5_a1lNi)
  = fmap
      (\ y1_a1lNj
         -> ReaderSettings x1_a1lNe x2_a1lNf x3_a1lNg y1_a1lNj x5_a1lNi)
      (f_a1lNd x4_a1lNh)
{-# INLINE verticalMode #-}

-- makeLenses ''SentenceData
sentenceContents :: Lens' SentenceData (NonEmpty AnnotatedPara)
sentenceContents f_a1lPx (SentenceData x1_a1lPy x2_a1lPz)
  = fmap
      (\ y1_a1lPA -> SentenceData y1_a1lPA x2_a1lPz) (f_a1lPx x1_a1lPy)
{-# INLINE sentenceContents #-}
sentenceLinkedEng :: Lens' SentenceData [Text]
sentenceLinkedEng f_a1lPB (SentenceData x1_a1lPC x2_a1lPD)
  = fmap
      (\ y1_a1lPE -> SentenceData x1_a1lPC y1_a1lPE) (f_a1lPB x2_a1lPD)
{-# INLINE sentenceLinkedEng #-}

reviewStateL :: (Profunctor p, Contravariant f)
  => ReviewType
  -> Optic' p f SrsEntry (Maybe (SrsEntryState, SrsEntryStats))
reviewStateL ReviewTypeRecogReview
  = to (\r -> (r ^? reviewState . _This)
    <|> (r ^? reviewState . _These . _1))

reviewStateL ReviewTypeProdReview
  = to (\r -> (r ^? reviewState . _That)
    <|> (r ^? reviewState . _These . _2))
