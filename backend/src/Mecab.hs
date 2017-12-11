{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}

module KanjiDB.Mecab
  where

import Protolude hiding (to, (&))
import           Control.Lens
import qualified Data.JMDict.XML.Parser as X
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Text as T
import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Data.Map as Map
import Data.Maybe
import Text.Read
import Data.JMDict.AST
import Data.JMDict.AST.Parser
import Data.Char
import Text.Pretty.Simple
import qualified Data.Conduit.List
import Text.XML.Stream.Parse hiding (anyOf)
import Control.Monad.Trans.Resource
import Data.IORef
import Data.Conduit
import Control.Monad.IO.Class
import Control.Monad
import KanjiDB.KanaTable
import Common

import Data.Ix
import Text.MeCab
import Data.SearchEngine

parseAndSearch es se m t = do
  feats <- catMaybes <$> parseMecab m t
  let
    fun feat = (term, gs)
      where
        term = (_mecabNodeFeat7 feat)
        eIds = query se [term]
        e = catMaybes $
          fmap ((flip Map.lookup) es) eIds
        gs = e ^.. (traverse . entrySenses . traverse
                    . senseGlosses . traverse
                    . glossDefinition)
  return $ fmap fun feats


parseMecab m t = do
  nodes <- liftIO $ parseToNodes m t
  let feats = map nodeFeature nodes
  return $ fmap makeMecabFeat feats

makeMecabFeat :: Text -> Maybe MecabNodeFeatures
makeMecabFeat n = case T.splitOn "," n of
  ("BOS/EOS":_) -> Nothing
  (t1:t2:t3:t4:t5:t6:t7:t8:t9:[]) -> Just $
    MecabNodeFeatures
      (getFeat t1)
      (getMaybeFeat t2)
      (getMaybeFeat t3)
      (getMaybeFeat t4)
      (getMaybeFeat t5)
      (getMaybeFeat t6)
      (getFeat t7)
      (getFeat t8)
      (getFeat t9)
  _ -> Nothing
  where
    getFeat tf
      | tf == "*" = error $ "getFeat-> " <> n
      | otherwise = tf
    getMaybeFeat tf
      | tf == "*" = Nothing
      | otherwise = Just tf

data MecabNodeFeatures = MecabNodeFeatures
  { _mecabNodeFeat1 :: Text
  , _mecabNodeFeat2 :: Maybe Text
  , _mecabNodeFeat3 :: Maybe Text
  , _mecabNodeFeat4 :: Maybe Text
  , _mecabNodeFeat5 :: Maybe Text
  , _mecabNodeFeat6 :: Maybe Text
  , _mecabNodeFeat7 :: Text
  , _mecabNodeFeat8 :: Text
  , _mecabNodeFeat9 :: Text
  }
  deriving (Show)

makeLenses ''MecabNodeFeatures
