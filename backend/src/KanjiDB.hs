{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module KanjiDB where

import Common
import Model

import Protolude
import Control.Lens.TH
import Data.Binary
import Data.SearchEngine
import Data.Ix

type KanjiDb = Map KanjiId KanjiData

data KanjiData = KanjiData
  { _kanjiDetails    :: KanjiDetails
  , _kanjiVocabSet   :: Set VocabId
  , _kanjiRadicalSet :: Set RadicalId
  }
  deriving (Generic, Binary)

makeLenses ''KanjiData

type VocabDb = Map VocabId VocabData

data VocabData = VocabData
  { _vocabDetails    :: VocabDetails
  , _vocabVocabSet   :: Set VocabId
  , _vocabKanjiSet   :: Set KanjiId
  }
  deriving (Generic, Binary)

makeLenses ''VocabData

type RadicalDb = Map RadicalId (Set KanjiId)
--
type KanjiSearchEngine = SearchEngine KanjiDetails KanjiId KanjiSearchFields NoFeatures

type VocabSearchEngine = SearchEngine VocabDetails VocabId VocabSearchFields NoFeatures

data KanjiSearchFields =
  KanjiCharacter
  | KanjiOnReading
  | KanjiKuReading
  | KanjiNaReading
  | KanjiMeanings
  deriving (Eq, Ord, Enum, Bounded, Ix, Show)

data VocabSearchFields =
  VocabKanji
  | VocabFurigana
  | VocabMeanings
  deriving (Eq, Ord, Enum, Bounded, Ix, Show)


-- This is imported in Foundation so need separate module
data SrsReviewData = SrsReviewData
  -- The reviewQueue contains a small number of pending reviews
  -- so that the user is able to complete (reading and meaning)
  -- in prescence of a large number of pending reviews.
  { _reviewQueue :: Map SrsEntryId ReviewState
  , _undoQueue   :: [UndoQueueData]
  , _reviewStats :: SrsReviewStats
  }

data ReviewStatus =
  NotAnswered | AnsweredWrong
  | AnsweredWithMistake | AnsweredWithoutMistake
  deriving (Eq)

type ReviewState = Map ReviewType ReviewStatus

type UndoQueueData = (Maybe SrsEntry, SrsEntryId
                   , ReviewState, ReviewType)

reviewQueueLength = 10 :: Int
undoQueueLength = 20 :: Int
