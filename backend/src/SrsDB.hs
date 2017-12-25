{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

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
import Data.Typeable (Typeable, Typeable1)
import qualified Data.BTree.Impure as Tree

import Control.Monad.Haskey
import Database.Haskey.Alloc.Concurrent (Root)
import Data.Binary.Orphans
import GHC.Generics (Generic)
import Data.These
import Data.List.NonEmpty (NonEmpty)

import Data.Time.Calendar (Day)

-- Haskey based db schema
newtype AppConcurrentDb = AppConcurrentDb
  { unAppConcurrentDb :: DbSchema CurrentDb }
  deriving (Generic, Show, Typeable, Binary, Value, Root)

type family DbSchema t
type instance DbSchema t = AppConcurrentDbTree t

data AppConcurrentDbTree t = AppConcurrentDbTree
  { _userData :: Tree Int64 (AppUserData t)
  } deriving (Generic, Typeable, Binary)

deriving instance Show (AppConcurrentDbTree CurrentDb)
deriving instance Value (AppConcurrentDbTree CurrentDb)
deriving instance Root (AppConcurrentDbTree CurrentDb)


type family AppUserData t
type instance AppUserData t = AppUserDataTree t

data AppUserDataTree t = AppUserDataTree
  { _reviews :: Tree SrsEntryId SrsEntry
  , _readerDocuments :: Tree ReaderDocumentId (ReaderDocument t)
  , _kanjiSrsMap :: Tree KanjiId SrsEntryId
  , _vocabSrsMap :: Tree VocabId SrsEntryId
  -- An Srs Item may not have an entry in this map
  , _srsKanjiVocabMap :: Tree SrsEntryId (Either KanjiId VocabId)
  , _readerSettings :: ReaderSettings t
  } deriving (Generic, Typeable, Binary)

instance Value (AppUserDataTree CurrentDb)
instance Value (AppUserDataTree OldDb)
instance Show (AppUserDataTree CurrentDb)
instance Show (AppUserDataTree OldDb)

openSrsDB :: FilePath -> IO (ConcurrentDb AppConcurrentDb)
openSrsDB fp =
  flip runFileStoreT defFileStoreConfig $
    openConcurrentDb hnds >>= \case
      Nothing -> createConcurrentDb hnds (AppConcurrentDb (AppConcurrentDbTree Tree.empty))
      Just db -> return db
  where
    hnds = concurrentHandles fp

instance Value Int
instance Value a => Value (Maybe a)

instance Value Day
instance Value a => Value (NonEmpty a)
instance (Value a, Value b) => Value (Either a b)
instance (Value a, Value b) => Value (These a b)

instance Binary (ReaderSettingsTree t)
instance (Typeable t) => Value (ReaderSettingsTree t)
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
instance Binary (ReaderDocumentTree t)
instance (Typeable t) => Value (ReaderDocumentTree t)
instance Binary (ReaderDocumentOldTree t)
instance (Typeable t) => Value (ReaderDocumentOldTree t)

instance Binary SrsEntryId
instance Value SrsEntryId

instance Key SrsEntryId
instance Key ReaderDocumentId
instance Key KanjiId
instance Key VocabId

makeLenses ''AppUserDataTree
makeLenses ''AppConcurrentDbTree
