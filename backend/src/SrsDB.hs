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
import KanjiDB
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

data SrsReviewData = SrsReviewData
  { _reviews :: Tree SrsEntryId SrsEntry
  , _readerDocuments :: Tree ReaderDocumentId ReaderDocument
  , _kanjiSrsMap :: Tree KanjiId SrsEntryId
  , _vocabSrsMap :: Tree VocabId SrsEntryId
  -- An Srs Item may not have an entry in this map
  , _srsKanjiVocabMap :: Tree SrsEntryId (Either KanjiId VocabId)
  } deriving (Generic, Show, Typeable, Binary, Value)

-- Haskey based db schema
data AppSrsReviewState = AppSrsReviewState
  { _userReviews :: Tree Int64 SrsReviewData
  } deriving (Generic, Show, Typeable, Binary, Value, Root)

openSrsDB :: FilePath -> IO (ConcurrentDb AppSrsReviewState)
openSrsDB fp =
  flip runFileStoreT defFileStoreConfig $
    openConcurrentDb hnds >>= \case
      Nothing -> createConcurrentDb hnds (AppSrsReviewState Tree.empty)
      Just db -> return db
  where
    hnds = concurrentHandles fp

instance Value Int
instance Value a => Value (Maybe a)

instance Value Day
instance Value a => Value (NonEmpty a)
instance (Value a, Value b) => Value (Either a b)
instance (Value a, Value b) => Value (These a b)


instance Binary SrsEntryState
instance Value SrsEntryState
instance Binary SrsEntryStats
instance Value SrsEntryStats
instance Binary SrsEntry
instance Value SrsEntry
instance Binary SrsInterval
instance Value SrsInterval
instance Binary ReaderDocumentId
instance Value ReaderDocumentId
instance Binary ReaderDocument
instance Value ReaderDocument

instance Binary SrsEntryId
instance Value SrsEntryId

instance Key SrsEntryId
instance Key ReaderDocumentId
instance Key KanjiId
instance Key VocabId

makeLenses ''SrsReviewData
makeLenses ''AppSrsReviewState
