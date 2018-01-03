{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.WebSocketHandler.Utils where

import Import
import Control.Lens
import qualified Data.Text as T
import Data.Char
import Common
import Model
import Text.Pretty.Simple
import Yesod.WebSockets
import Text.MeCab
import KanjiDB
import NLP.Romkan
import NLP.Japanese.Utils
import qualified Data.BTree.Impure as Tree
import Data.BTree.Alloc (AllocM, AllocReaderM)
import Control.Monad.State hiding (foldM)

type WsHandlerM = ReaderT WsHandlerEnv Handler

data WsHandlerEnv = WsHandlerEnv
  { kanjiSearchResult :: IORef ([KanjiId], Int)
  , kanjiVocabResult  :: IORef ([VocabId], Int)
  , vocabSearchResult :: IORef ([VocabId], Int)
  , currentUserId :: Int64
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
