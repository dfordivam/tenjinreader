{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module SrsDB where

import Common
import Model hiding (Key)

import Control.Applicative (Applicative, (<$>))
import Control.Lens
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.BTree.Alloc (AllocM, AllocReaderM)
import Data.BTree.Impure (Tree, insertTree, lookupTree, toList)
import Data.BTree.Primitives (Value, Key)
import Data.Binary (Binary)
import Data.Text (Text, unpack)
import Data.Int (Int64)
import Data.Typeable (Typeable)
import qualified Data.BTree.Impure as Tree

import Control.Monad.Haskey
import Database.Haskey.Alloc.Concurrent (Root)
import Data.Binary.Orphans
import GHC.Generics (Generic)
import Data.These
import Data.List.NonEmpty (NonEmpty)

import Data.Time.Calendar (Day)

instance Value Day
instance Value a => Value (NonEmpty a)

newtype SrsInterval = SrsInterval { unSrsInterval :: Integer }
  deriving (Generic, Show, Typeable, Binary, Value)

-- If the user suspends a card and then resume later
-- 1. It was due when suspended -> make immediately available for review
-- 2. not due -> no suspend?

data SrsEntryState = NewReview |
  Suspended SrsInterval | NextReviewDate Day SrsInterval
  deriving (Generic, Show, Typeable, Binary, Value)

makePrisms ''SrsEntryState
-- SRS algo
-- Correct Answer ->
--   (answer date - due date + last interval) * ease factor
-- Wrong Answer ->
--   last interval * ease factor

-- ease factor depends on SrsEntryStats

data SrsEntryStats = SrsEntryStats
  { _failureCount :: Int
  , _successCount :: Int
  } deriving (Generic, Show, Typeable, Binary, Value)

makeLenses ''SrsEntryStats

instance (Value a, Value b) => Value (Either a b)
instance (Value a, Value b) => Value (These a b)

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
   , _field :: NonEmpty Text
  } deriving (Generic, Show, Typeable, Binary, Value)

makeLenses ''SrsEntry

instance Key SrsEntryId
instance Key KanjiId
instance Key VocabId

data SrsReviewData = SrsReviewData
  { _reviews :: Tree SrsEntryId SrsEntry
  , _kanjiSrsMap :: Tree KanjiId SrsEntryId
  , _vocabSrsMap :: Tree VocabId SrsEntryId
  -- An Srs Item may not have an entry in this map
  , _srsKanjiVocabMap :: Tree SrsEntryId (Either KanjiId VocabId)
  } deriving (Generic, Show, Typeable, Binary, Value)

makeLenses ''SrsReviewData

-- Haskey based db schema
data AppSrsReviewState = AppSrsReviewState
  { _userReviews :: Tree Int64 SrsReviewData
  } deriving (Generic, Show, Typeable, Binary, Value, Root)

makeLenses ''AppSrsReviewState

openSrsDB :: FilePath -> IO (ConcurrentDb AppSrsReviewState)
openSrsDB fp =
  flip runFileStoreT defFileStoreConfig $
    openConcurrentDb hnds >>= \case
      Nothing -> createConcurrentDb hnds (AppSrsReviewState Tree.empty)
      Just db -> return db
  where
    hnds = concurrentHandles fp
