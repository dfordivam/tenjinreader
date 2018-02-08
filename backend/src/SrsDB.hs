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
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text, unpack)
import Data.Int (Int64)
import Data.Typeable (Typeable, Typeable1)
import qualified Data.BTree.Impure as Tree
import Data.Default

import Control.Monad.Haskey
import Database.Haskey.Alloc.Concurrent (Root, ConcurrentHandles)
import Database.Haskey.Alloc.Concurrent.Database
import Data.Binary.Orphans
import GHC.Generics (Generic)
import Data.These
import Data.List.NonEmpty (NonEmpty)

import Data.Time.Calendar (Day)

newtype UserConcurrentDb = UserConcurrentDb
  { unUserConcurrentDb :: AppUserData CurrentDb }
  deriving (Generic, Show, Typeable, Binary, Value, Root)

type family AppUserData t
type instance AppUserData CurrentDb = AppUserDataTree CurrentDb

data AppUserDataTree t = AppUserDataTree
  { _reviews :: Tree SrsEntryId SrsEntry
  , _readerDocuments :: Tree ReaderDocumentId (ReaderDocument t)
  , _kanjiSrsMap :: Tree KanjiId (Either () SrsEntryId)
  , _vocabSrsMap :: Tree VocabId (Either () SrsEntryId)
  -- An Srs Item may not have an entry in this map
  , _srsKanjiVocabMap :: Tree SrsEntryId (Either KanjiId VocabId)
  , _readerSettings :: ReaderSettings t
  , _favouriteSentences :: Set SentenceId
  , _documentAccessOrder :: [ReaderDocumentId]
  } deriving (Generic, Typeable, Binary)

deriving instance Root (AppUserDataTree CurrentDb)
instance Value (AppUserDataTree CurrentDb)
instance Value (AppUserDataTree OldDb)
instance Show (AppUserDataTree CurrentDb)
instance Show (AppUserDataTree OldDb)

type family ReaderDocument t
type instance ReaderDocument CurrentDb = ReaderDocumentTree CurrentDb

data ReaderDocumentTree t = ReaderDocument
  { _readerDocId :: ReaderDocumentId
  , _readerDoc :: ReaderDocumentType t
  , _readerDocProgress :: (Int, Maybe Int) -- (Para, offset)
  }
  deriving (Generic, Show)

type family ReaderDocumentType t
type instance ReaderDocumentType t = ReaderDocumentTypeCurrent

data ReaderDocumentTypeCurrent
  = MyDocument Text AnnotatedDocument
  | Book BookId
  | Article ArticleId
  deriving (Generic, Show)

openUserDB :: FilePath -> IO (ConcurrentDb UserConcurrentDb, ConcurrentHandles)
openUserDB fp =
  flip runFileStoreT defFileStoreConfig $
    openConcurrentDb hnds >>= \case
      Nothing -> (,) <$> createConcurrentDb hnds (UserConcurrentDb d) <*> pure hnds
      Just db -> return (db, hnds)
  where
    d = AppUserDataTree Tree.empty Tree.empty Tree.empty Tree.empty Tree.empty def
      Set.empty []
    hnds = concurrentHandles fp

closeUserDB :: ConcurrentHandles -> IO ()
closeUserDB hnds =
  flip runFileStoreT defFileStoreConfig (closeConcurrentHandles hnds)

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
instance Binary (ReaderDocumentTypeCurrent)
-- instance Binary (ReaderDocumentOldTree t)
-- instance (Typeable t) => Value (ReaderDocumentOldTree t)

instance Binary SrsEntryId
instance Value SrsEntryId

instance Key SrsEntryId
instance Key ReaderDocumentId
instance Key KanjiId
instance Key VocabId

makeLenses ''AppUserDataTree
makeLenses ''ReaderDocumentTree
