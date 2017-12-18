{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}

module Mecab
  where

import Protolude hiding (to, (&))
import           Control.Lens
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Text as T
import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Data.Map as Map
import Data.Maybe
import Text.Read
import Data.JMDict.AST
import Data.Char
import Text.Pretty.Simple
import Common
import KanjiDB
import NLP.Japanese.Utils
import KanjiDB.JMDict

import Data.Ix
import Text.MeCab
import Data.SearchEngine
import qualified Data.Vector as V

parseAndSearch :: Map EntryId VocabData
  -> VocabSearchEngineNoGloss
  -> MeCab
  -> Text
  -> IO AnnotatedDocument
parseAndSearch es se m t = do
  feats <- liftIO $ mapM (parseMecab m) (T.lines t)
  let
    fun ("", _) = Nothing
    fun (surf, Nothing) = Just (Left surf)
    fun (surf, Just feat) = Just $ Right $
      (\v -> (v, eIds, True))
      (getVocabFurigana (surf,reading))
      where
        term = (_mecabNodeFeat7 feat)
        reading = (_mecabNodeFeat8 feat)
        eIds = query se [term]
        e = catMaybes $
          fmap ((flip Map.lookup) es) eIds
        gs = take 5 $ e ^.. (traverse . vocabEntry
                    . entrySenses . traverse
                    . senseGlosses . traverse
                    . glossDefinition)
  return $ V.fromList $ map (catMaybes . (fmap fun)) feats

isKanaOnly :: T.Text -> Bool
isKanaOnly = (all f) . T.unpack
  where f = not . isKanji
          -- isKana c || (elem c ['、', '〜', 'ー'])

getVocabFurigana (surf, reading)
  | isKanaOnly surf = Vocab [Kana surf]
  | otherwise = case makeFurigana (KanjiPhrase surf) (ReadingPhrase reading) of
    (Left _) -> Vocab [Kana surf]
    (Right v) -> v

parseMecab :: MeCab -> Text -> IO ([(Text, Maybe MecabNodeFeatures)])
parseMecab m t = do
  nodes <- parseToNodes m t
  pPrint $ nodes
  let feats = map nodeFeature nodes
  return $ zip (map nodeSurface nodes)
    (fmap makeMecabFeat feats)

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
