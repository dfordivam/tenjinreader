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

import Database.Haskey.Alloc.Concurrent (Root)
import Data.Binary.Orphans
import GHC.Generics (Generic)

import Data.Time.Calendar (Day)

instance Value Day

-- If the user suspends a card and then resume later
-- 1. It was due when suspended -> make immediately available for review
-- 2. not due -> no suspend?

newtype SrsInterval = SrsInterval { unSrsInterval :: Integer }
  deriving (Generic, Show, Typeable, Binary, Value)

data SrsEntryState =
  Suspended SrsInterval | NextReviewDate Day SrsInterval
  deriving (Generic, Show, Typeable, Binary, Value)

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

data SrsEntry = SrsEntry
  {  _reviewState :: SrsEntryState
   , _stats :: SrsEntryStats
   , _readings :: [Reading]
   , _meaning :: [Meaning]
   , _readingNotes :: Maybe ReadingNotes
   , _meaningNotes :: Maybe MeaningNotes
   , _field :: Text
  } deriving (Generic, Show, Typeable, Binary, Value)

makeLenses ''SrsEntry

data SrsReviewData = SrsReviewData
  { _reviews :: Tree SrsEntryId SrsEntry
  } deriving (Generic, Show, Typeable, Binary, Value)

makeLenses ''SrsReviewData

-- Haskey based db schema
data AppSrsReviewState = AppSrsReviewState
  { _userReviews :: Tree Int64 SrsReviewData
  } deriving (Generic, Show, Typeable, Binary, Value, Root)

makeLenses ''AppSrsReviewState
