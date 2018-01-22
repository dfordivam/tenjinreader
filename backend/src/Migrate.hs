{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Migrate where

import Common
import KanjiDB
import SrsDB
import Model hiding (Key)

import Protolude hiding ((&))

import Control.Lens hiding (reviews)
import Data.Coerce
import Data.Default
import Data.BTree.Alloc (AllocM, AllocReaderM)
import Data.BTree.Primitives (Value, Key)
import Data.Binary (Binary)
import Data.Text (Text, unpack)
import Data.Int (Int64)
import Data.Typeable (Typeable)
import qualified Data.BTree.Impure as Tree
import Data.BTree.Impure (Tree)

import Control.Monad.Haskey
import Database.Haskey.Alloc.Concurrent (Root)
import Data.Binary.Orphans
import GHC.Generics (Generic)
import Data.These
import Data.List.NonEmpty (NonEmpty)

import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector as V
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Time.Calendar (Day)


data AppUserDataTreeOld t = AppUserDataTreeOld
  { _reviewsOld :: Tree SrsEntryId SrsEntry
  , _readerDocumentsOld :: Tree ReaderDocumentId (ReaderDocument t)
  , _kanjiSrsMapOld :: Tree KanjiId (SrsEntryId)
  , _vocabSrsMapOld :: Tree VocabId (SrsEntryId)
  -- An Srs Item may not have an entry in this map
  , _srsKanjiVocabMapOld :: Tree SrsEntryId (Either KanjiId VocabId)
  , _readerSettingsOld :: ReaderSettings t
  } deriving (Generic, Show, Typeable, Binary, Value)

type instance AppUserData OldDb = AppUserDataTreeOld OldDb

makeLenses ''AppUserDataTreeOld

newtype AppOldConcurrentDb = AppOldConcurrentDb
  { unAppOldConcurrentDb :: DbSchema OldDb }
  deriving (Generic, Show, Typeable, Binary, Value, Root)

deriving instance Show (AppConcurrentDbTree OldDb)
deriving instance Value (AppConcurrentDbTree OldDb)
deriving instance Root (AppConcurrentDbTree OldDb)

migrateMain = do
  db <- openSrsDB "srsdb"
  dbOld <- openOldSrsDB "oldsrsdb"
  putStrLn $ ("Doing Migration" :: Text)
  migrateFun dbOld db
  putStrLn $ ("Migration Done" :: Text)

migrateFun dbOld db = do
  let
    runNewDb :: (_)
      => HaskeyT (AppConcurrentDb) m a
      -> m a
    runNewDb action = runHaskeyT action db
      defFileStoreConfig

    runOldDb :: (_)
      => HaskeyT (AppOldConcurrentDb) m a
      -> m a
    runOldDb action = runHaskeyT action dbOld
      defFileStoreConfig

    getUserData uId =
      runOldDb $ transactReadOnly
        (\(AppOldConcurrentDb db) -> do
           ud <- Tree.lookupTree uId (db ^. userData)
           let
             -- tup :: (AllocReaderM n)
             --   => AppUserDataTreeOld (OldDb)
             --   -> n (TType)
             tup d =
               ( (d ^. reviewsOld)
               , (d ^. readerDocumentsOld)
               , (d ^. kanjiSrsMapOld)
               , (d ^. vocabSrsMapOld)
               , (d ^. srsKanjiVocabMapOld)
               , (d ^. readerSettingsOld))
               &   (_1 %%~ Tree.toList)
               >>= (_2 %%~ Tree.toList)
               >>= (_3 %%~ Tree.toList)
               >>= (_4 %%~ Tree.toList)
               >>= (_5 %%~ Tree.toList)

           mapM tup ud)

    addUserData uId d =
      runNewDb $ transact $ \(AppConcurrentDb tree) -> do
        newD <- AppUserDataTree
          <$> (Tree.fromList $ d ^. _1)
          <*> (Tree.fromList $ coerce (d ^. _2))
          <*> (Tree.fromList $ modifyMap $ d ^. _3)
          <*> (Tree.fromList $ modifyMap $ d ^. _4)
          <*> (Tree.fromList $ d ^. _5)
          <*> pure (coerce (d ^. _6))

        newTree <- tree & userData %%~
          Tree.insertTree uId newD
        commit () (AppConcurrentDb newTree)

    modifyMap :: [(a,b)] -> [(a,Either () b)]
    modifyMap = each . _2 %~ Right
    -- modifyRD (rId, rd) = (,) rId $ ReaderDocument
    --   (rd ^. readerDocOldId)
    --   (rd ^. readerDocOldTitle)
    --   (rd ^. readerDocOldContent)
    --   (0,Nothing)
  let
    migrateAction = do
      uds <- runOldDb $ transactReadOnly $
        (\(AppOldConcurrentDb db) -> do
          us <- Tree.toList (db ^. userData)
          return (map fst us))
      mapM addFun (uds :: [Int64])

    addFun uId = do
      getUserData uId
        >>= mapM (addUserData uId)

  migrateAction

openOldSrsDB :: FilePath -> IO (ConcurrentDb AppOldConcurrentDb)
openOldSrsDB fp =
  flip runFileStoreT defFileStoreConfig $
    openConcurrentDb hnds >>= \case
      Nothing -> error "Old DB not found"
      Just db -> return db
  where
    hnds = concurrentHandles fp
