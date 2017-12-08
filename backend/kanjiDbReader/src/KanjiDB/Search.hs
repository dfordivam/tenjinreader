{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}

module KanjiDB.Search
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
import Data.JMDict.AST.AST
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
import Text.MeCab (new)
import Data.SearchEngine

type VocabSearchEngine = SearchEngine Entry EntryId VocabSearchFields NoFeatures


data VocabSearchFields
  = VocabReadings
  | VocabGloss
  deriving (Eq, Ord, Enum, Bounded, Ix, Show)

getVocabSE :: [Entry] -> VocabSearchEngine
getVocabSE docs = insertDocs docs init
  where
    init = initSearchEngine conf vocabSearchRankParams
    conf = SearchConfig _entryUniqueId extractTerms transformQry (const noFeatures)
    extractTerms :: Entry -> VocabSearchFields -> [Term]
    extractTerms e sf = case sf of
      VocabReadings ->
        (e ^.. entryKanjiElements . traverse . kanjiPhrase . to unKanjiPhrase)
        ++ (e ^.. entryReadingElements . traverse . readingPhrase . to unReadingPhrase)
      VocabGloss -> e ^..
        (entrySenses . traverse . senseGlosses . traverse . X.glossDefinition)

    transformQry :: Term -> VocabSearchFields -> Term
    transformQry t _ = t

vocabSearchRankParams :: SearchRankParameters VocabSearchFields NoFeatures
vocabSearchRankParams =
    SearchRankParameters {
      paramK1,
      paramB,
      paramFieldWeights,
      paramFeatureWeights     = noFeatures,
      paramFeatureFunctions   = noFeatures,
      paramResultsetSoftLimit = 200,
      paramResultsetHardLimit = 400,
      paramAutosuggestPrefilterLimit  = 500,
      paramAutosuggestPostfilterLimit = 500
    }
  where
    paramK1 :: Float
    paramK1 = 1.5

    paramB :: VocabSearchFields -> Float
    paramB VocabReadings   = 0.9
    paramB _    = 0.5

    paramFieldWeights :: VocabSearchFields -> Float
    paramFieldWeights VocabReadings        = 20
    paramFieldWeights _ = 1
