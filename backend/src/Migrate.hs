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
import qualified Data.Set as Set
import Data.Binary (Binary)
import Data.Text (Text, unpack)
import Data.Int (Int64)
import Data.Typeable (Typeable)
import qualified Data.BTree.Impure as Tree
import Data.BTree.Impure (Tree)

import Control.Monad.Haskey
import Database.Haskey.Alloc.Concurrent (Root, ConcurrentHandles)
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


newtype UserConcurrentDbOld = UserConcurrentDbOld
  { unUserConcurrentDb :: AppUserData OldDb }
  deriving (Generic, Show, Typeable, Binary, Value, Root)

type instance AppUserData OldDb = AppUserDataTreeOld OldDb

data AppUserDataTreeOld t = AppUserDataTreeOld
  { _reviewsOld :: Tree SrsEntryId SrsEntry
  , _readerDocumentsOld :: Tree ReaderDocumentId (ReaderDocument t)
  , _kanjiSrsMapOld :: Tree KanjiId (Either () SrsEntryId)
  , _vocabSrsMapOld :: Tree VocabId (Either () SrsEntryId)
  -- An Srs Item may not have an entry in this map
  , _srsKanjiVocabMapOld :: Tree SrsEntryId (Either KanjiId VocabId)
  , _readerSettingsOld :: ReaderSettings t
  } deriving (Generic, Show, Typeable, Binary, Value)

deriving instance Root (AppUserDataTreeOld OldDb)
makeLenses ''AppUserDataTreeOld

openUserDBOld :: FilePath -> IO (ConcurrentDb UserConcurrentDbOld, ConcurrentHandles)
openUserDBOld fp =
  flip runFileStoreT defFileStoreConfig $
    openConcurrentDb hnds >>= \case
      Nothing -> error "Old Db not found"
      Just db -> return (db, hnds)
  where
    hnds = concurrentHandles fp

migrateMain = do
  (db,_) <- openUserDB "userData/1"
  -- dbOld <- openOldSrsDB "oldsrsdb"
  (dbOld,_) <- openUserDBOld "userDataOld/1"
  putStrLn $ ("Doing Migration" :: Text)
  migrateFun dbOld db
  putStrLn $ ("Migration Done" :: Text)

migrateFun dbOld db = do
  let
    runNewDb :: (_)
      => HaskeyT (UserConcurrentDb) m a
      -> m a
    runNewDb action = runHaskeyT action db
      defFileStoreConfig

    runOldDb :: (_)
      => HaskeyT (UserConcurrentDbOld) m a
      -> m a
    runOldDb action = runHaskeyT action dbOld
      defFileStoreConfig

    getUserData uId =
      runOldDb $ transactReadOnly
        (\(UserConcurrentDbOld d) -> do
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

           tup d)

    addUserData uId d =
      runNewDb $ transact $ \_ -> do
        newD <- AppUserDataTree
          <$> (Tree.fromList $ d ^. _1)
          <*> (Tree.fromList $ coerce (d ^. _2))
          <*> (Tree.fromList $ d ^. _3)
          <*> (Tree.fromList $ d ^. _4)
          <*> (Tree.fromList $ d ^. _5)
          <*> pure (coerce (d ^. _6))
          <*> pure (Set.empty)
          <*> pure ([])

        commit () (UserConcurrentDb newD)

    -- modifyMap :: [(a,b)] -> [(a,Either () b)]
    -- modifyMap = each . _2 %~ Right
    -- modifyRD (rId, rd) = (,) rId $ ReaderDocument
    --   (rd ^. readerDocOldId)
    --   (rd ^. readerDocOldTitle)
    --   (rd ^. readerDocOldContent)
    --   (0,Nothing)
  let
    migrateAction = do
      -- uds <- runOldDb $ transactReadOnly $
      --   (\(AppOldConcurrentDb db) -> do
      --     us <- Tree.toList (db ^. userData)
      --     return (map fst us))
      -- mapM addFun (uds :: [Int64])
      addFun 1

    addFun uId = do
      getUserData uId
        >>= addUserData uId

  migrateAction
