{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.WebSocketHandler.Utils where

import Import
import Control.Lens
import qualified Data.Text as T
import Data.Char
import Common
import Model
import SrsDB
import Text.Pretty.Simple
import Yesod.WebSockets
import Text.MeCab
import KanjiDB
import NLP.Romkan
import NLP.Japanese.Utils
import Control.Monad.State hiding (foldM)
import qualified Data.Array as A
import Data.Array (Array)
import Data.Ix (inRange)

import Control.Monad.Haskey
import Database.Haskey.Store.File (defFileStoreConfig)
import Data.BTree.Alloc (AllocM, AllocReaderM)
import qualified Data.BTree.Impure as Tree

type WsHandlerM = ReaderT WsHandlerEnv Handler

data WsHandlerEnv = WsHandlerEnv
  { kanjiSearchResult :: IORef ([KanjiId], Int)
  , kanjiVocabResult  :: IORef ([VocabId], Int)
  , vocabSearchResult :: IORef ([VocabId], Int)
  , currentUserId     :: Int64
  , userConcurrentDb  :: ConcurrentDb UserConcurrentDb
  }

-- Convert Kanji to furigana (hiragana)
getKana :: Text -> Handler (Text)
getKana t = do
  m <- asks appMecabPtr
  nodes <- liftIO $ parseToNodes m t
  let feats = map nodeFeature nodes
      -- get the last part of the mecab output, this will include '*'
      readings :: [Text]
      readings = map getR feats
      getR t = r2
        where (_:r2:_) = (reverse . (T.splitOn ",")) t
      -- convert valid characters to hiragana
      f = map katakanaToHiragana $ filter hasKanaOrKanji readings

  return $ mconcat f

arrayLookup :: (A.Ix i) => Array i e -> [i] -> [e]
arrayLookup a ids = map (a A.!) validIds
  where validIds = filter (inRange $ A.bounds a) ids

arrayLookupMaybe :: (A.Ix i) => Array i e -> i -> Maybe e
arrayLookupMaybe a i = listToMaybe $ arrayLookup a [i]

getVocabSrsState Nothing = NotInSrs
getVocabSrsState (Just (Right s)) = InSrs s
getVocabSrsState (Just (Left ())) = IsWakaru

type SrsDB = HaskeyT UserConcurrentDb WsHandlerM

runSrsDB
  :: SrsDB a
  -> WsHandlerM a
runSrsDB action = do
  db <- asks userConcurrentDb
  runHaskeyT action db defFileStoreConfig

transactSrsDB_ ::
  (forall m . (AllocM m)
    => AppUserData CurrentDb
    -> m (AppUserData CurrentDb))
  -> WsHandlerM ()
transactSrsDB_ action = transactSrsDB
  (\t -> action t >>= (\nt -> return (nt,())))

transactSrsDB ::
  (forall m . (AllocM m)
    => AppUserData CurrentDb
    -> m (AppUserData CurrentDb, a))
  -> WsHandlerM a
transactSrsDB action = do
  runSrsDB $ transact $ \(UserConcurrentDb d) -> do
    (nd, a) <- action d
    commit a (UserConcurrentDb nd)

transactReadOnlySrsDB ::
  (forall m . (AllocReaderM m)
    => AppUserData CurrentDb -> m a)
  -> WsHandlerM a
transactReadOnlySrsDB action = do
  runSrsDB $ transactReadOnly $ \(UserConcurrentDb d) -> action d

updateTreeM :: _
  => k -> (v -> m v) -> Tree.Tree k v -> m (Tree.Tree k v)
updateTreeM k fun tree = do
  Tree.lookupTree k tree
  >>= mapM fun
  >>= mapM (\v -> Tree.insertTree k v tree)
  >>= (\t -> return $ maybe tree id t)

updateTreeLiftedM :: (AllocM m, _)
  => k -> (v -> t m v) -> Tree.Tree k v -> t m (Tree.Tree k v)
updateTreeLiftedM k fun tree = do
  lift $ Tree.lookupTree k tree
  >>= mapM fun
  >>= mapM (\v -> lift $ Tree.insertTree k v tree)
  >>= (\t -> return $ maybe tree id t)

runStateWithNothing m s = (flip runStateT Nothing) $ m s
