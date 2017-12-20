{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
module KanjiDB where

import Common
import Model
import Radicals
import KanjiDB.Interface
import KanjiDB.JMDict

import Protolude hiding (to, (&))
import Control.Lens
import Control.Lens.TH
import Data.Binary
import Data.SearchEngine
import Data.Ix
import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Data.Map as Map
import Text.MeCab
import NLP.Romkan as NLP
import NLP.Snowball as NLP
import NLP.Tokenize.Text as NLP
import NLP.Japanese.Utils as Utils
import System.Directory
import qualified Data.ByteString.Lazy
import Data.JMDict.AST

type KanjiDb = Map KanjiId KanjiData

data KanjiData = KanjiData
  { _kanjiDetails    :: KanjiDetails
  , _kanjiVocabSet   :: Set VocabId
  , _kanjiRadicalSet :: Set RadicalId
  }
  deriving (Generic, Binary)

makeLenses ''KanjiData

type VocabDb = Map VocabId VocabData

data VocabData = VocabData
  { _vocabDetails    :: VocabDetails
  , _vocabEntry      :: Entry
  , _vocabKanjiSet   :: Set KanjiId
  }
  deriving (Generic, Binary)

makeLenses ''VocabData

type RadicalDb = Map RadicalId (Set KanjiId)
--
createDBs ::
  MeCab
  -> IO (KanjiDb, VocabDb, RadicalDb)
createDBs mecab = do
  let kanjibin = "kanjidb.bin"
      jmDictFilePath = "/home/divam/nobup/jmdict/JMdict_e"
  ex <- doesFileExist kanjibin
  if ex
    then do
      bs <- Data.ByteString.Lazy.readFile kanjibin
      return $ Data.Binary.decode bs
    else do
      conn <- openKanjiDB
      es <- getJMDictEntries jmDictFilePath

      let
        f = do
          ks <- KanjiDB.Interface.getKanjis

          let
            ksMap = Map.fromList $
              ks & each %~ (view swapped) . (_2 %~ _kanjiCharacter)
            vDb = Map.fromList $
              map (getVocabData ksMap) es

          kDb <- Map.fromList <$> getKanjiData vDb ks

          rDb <- Map.fromList <$> mapM
            (\r -> do
                ks <- getRadicalKanjis r
                return (r, Set.fromList ks))
            (Map.keys radicalTable)

          return (kDb, vDb, rDb)
      ret <- runReaderT f conn
      Data.ByteString.Lazy.writeFile kanjibin (Data.Binary.encode ret)
      return ret

getVocabData
  :: Map Kanji KanjiId
  -> (Entry, VocabDetails)
  -> (VocabId, VocabData)
getVocabData ksMap (e,v) =
  (v ^. vocabId, VocabData v e kSet)
  where
    kSet = Set.fromList $ e ^.. entryKanjiElements . traverse
      . kanjiPhrase . to kFind . traverse
    kFind (KanjiPhrase kp) = catMaybes $ map (flip Map.lookup $ ksMap)
      $ map Kanji $ (Utils.getKanjis kp)

getKanjiData
  :: VocabDb
  -> [(KanjiId, KanjiDetails)]
  -> _ [(KanjiId, KanjiData)]
getKanjiData vDb ks = mapM getKD ks
  where
    ksMap :: Map KanjiId (Set VocabId)
    ksMap = Map.foldl' f (Map.empty) vDb
    f m v = Set.foldr' (Map.alter p) m (v ^. vocabKanjiSet)
      where
        p (Just s) = Just $ Set.insert vId s
        p Nothing = Just $ Set.singleton vId
        vId = (v ^. vocabEntry . entryUniqueId)

    getKD (kId,k) = do
      rs <- getKanjiRadicals kId
      return (kId, KanjiData k vSet (Set.fromList rs))
      where vSet = maybe Set.empty identity $ Map.lookup kId ksMap

type KanjiSearchEngine = SearchEngine KanjiDetails KanjiId KanjiSearchFields NoFeatures

data KanjiSearchFields =
  KanjiCharacter
  | KanjiOnReading
  | KanjiKuReading
  | KanjiNaReading
  | KanjiMeanings
  deriving (Eq, Ord, Enum, Bounded, Ix, Show)

getKanjiSE :: KanjiDb -> KanjiSearchEngine
getKanjiSE kDb = insertDocs docs init
  where
    init = initSearchEngine conf kanjiSearchRankParams
    conf = SearchConfig _kanjiId extractTerms transformQry (const noFeatures)
    docs = map _kanjiDetails $ Map.elems kDb
    extractTerms :: KanjiDetails -> KanjiSearchFields -> [Term]
    extractTerms ks kf = case kf of
      KanjiCharacter -> [unKanji $ ks ^. kanjiCharacter]
      KanjiOnReading -> unReading <$> ks ^. kanjiOnyomi
      KanjiKuReading -> unReading <$> ks ^. kanjiKunyomi
      KanjiNaReading -> unReading <$> ks ^. kanjiNanori
      KanjiMeanings ->  ks ^.. kanjiMeanings . traverse . to unMeaning
          . to extractMeaningTerms . traverse

    transformQry :: Term -> KanjiSearchFields -> Term
    transformQry t KanjiMeanings = maybe "" identity $ headMay $ extractMeaningTerms t
    transformQry t _ = t

kanjiSearchRankParams :: SearchRankParameters KanjiSearchFields NoFeatures
kanjiSearchRankParams =
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

    paramB :: KanjiSearchFields -> Float
    paramB KanjiCharacter   = 0.9
    paramB _    = 0.5

    paramFieldWeights :: KanjiSearchFields -> Float
    paramFieldWeights KanjiCharacter        = 20
    paramFieldWeights _ = 1


type VocabSearchEngineNoGloss = SearchEngine Entry EntryId VocabElements VocabFeatures

type VocabSearchEngine = SearchEngine Entry EntryId VocabSearchFields VocabFeatures

data VocabElements
  = VocabReadingElem
  | VocabKanjiElem
  deriving (Eq, Ord, Enum, Bounded, Ix, Show)

data VocabSearchFields
  = VocabReadings
  | VocabGloss
  deriving (Eq, Ord, Enum, Bounded, Ix, Show)

getVocabSE :: VocabDb -> VocabSearchEngine
getVocabSE vDb = insertDocs docs init
  where
    docs = vDb ^.. traverse . vocabEntry
    init = initSearchEngine conf vocabSearchRankParams
    conf = SearchConfig _entryUniqueId extractTerms transformQry featureFun
    extractTerms :: Entry -> VocabSearchFields -> [Term]
    extractTerms e sf = case sf of
      VocabReadings ->
        (e ^.. entryKanjiElements . traverse . kanjiPhrase . to unKanjiPhrase)
        ++ (e ^.. entryReadingElements . traverse . readingPhrase . to unReadingPhrase)
      VocabGloss -> e ^..
        (entrySenses . traverse . senseGlosses . traverse .
         glossDefinition . to extractMeaningTerms . traverse)

    transformQry :: Term -> VocabSearchFields -> Term
    transformQry t VocabReadings = t
    transformQry t VocabGloss = maybe "" identity $ headMay $ extractMeaningTerms t

getVocabSENG :: VocabDb -> VocabSearchEngineNoGloss
getVocabSENG vDb = insertDocs docs init
  where
    docs = vDb ^.. traverse . vocabEntry
    init = initSearchEngine conf vocabSearchRankParamsNG
    conf = SearchConfig _entryUniqueId extractTerms transformQry featureFun
    extractTerms :: Entry -> VocabElements -> [Term]
    extractTerms e sf = case sf of
      VocabReadingElem ->
        (e ^.. entryReadingElements . traverse . readingPhrase . to unReadingPhrase)
      VocabKanjiElem ->
        (e ^.. entryKanjiElements . traverse . kanjiPhrase . to unKanjiPhrase)

    transformQry :: Term -> VocabElements -> Term
    transformQry t _ = t

extractMeaningTerms :: Text -> [Text]
extractMeaningTerms =
      NLP.stems NLP.English
    . filter (`Set.notMember` stopWords)
    . map (T.toCaseFold)
    -- . concatMap splitTok
    -- . filter (not . ignoreTok)
    . NLP.tokenize

stopWords :: Set Term
stopWords =
  Set.fromList ["a","the","to","of","an","on","as","by","is"]

data VocabFeatures
  = VocabPriority
  | VocabInfo
  deriving (Eq, Ord, Enum, Bounded, Ix, Show)

featureFun :: Entry -> VocabFeatures -> Float
featureFun e VocabPriority = sum $ map f allPr
  where allPr = e ^.. entryKanjiElements . traverse . kanjiPriority . traverse
          ++ e ^.. entryReadingElements . traverse . readingPriority . traverse
        f (FreqOfUse r) = 1000 /(5 + fromIntegral r)
        f p
          | p == News1 || p == Ichi1 || p == Spec1 = 50
          | p == News2 || p == Ichi2 || p == Spec2 = 20
          | otherwise = 10

featureFun e VocabInfo = case (ki,ri) of
  ([],[]) -> 10
  ([],ri) -> 5
  (_,_) -> 1
  where ki = e ^.. entryKanjiElements . traverse . kanjiInfo . traverse
        ri = e ^.. entryReadingElements . traverse . readingInfo . traverse

vocabSearchRankParams :: SearchRankParameters VocabSearchFields VocabFeatures
vocabSearchRankParams =
    SearchRankParameters {
      paramK1,
      paramB,
      paramFieldWeights,
      paramFeatureWeights     = featWeights,
      paramFeatureFunctions   = featFun,
      paramResultsetSoftLimit = 200,
      paramResultsetHardLimit = 400,
      paramAutosuggestPrefilterLimit  = 500,
      paramAutosuggestPostfilterLimit = 500
    }
  where
    paramK1 :: Float
    paramK1 = 1.5

    paramB :: VocabSearchFields -> Float
    -- paramB VocabReadings   = 0.9
    paramB _    = 0.5

    paramFieldWeights :: VocabSearchFields -> Float
    -- paramFieldWeights VocabReadings        = 20
    paramFieldWeights _ = 1

    featWeights VocabPriority = 5
    featWeights VocabInfo = 1
    featFun VocabPriority = LogarithmicFunction 1
    featFun VocabInfo = LogarithmicFunction 1

vocabSearchRankParamsNG :: SearchRankParameters VocabElements VocabFeatures
vocabSearchRankParamsNG =
    SearchRankParameters {
      paramK1,
      paramB,
      paramFieldWeights,
      paramFeatureWeights     = featWeights,
      paramFeatureFunctions   = featFun,
      paramResultsetSoftLimit = 200,
      paramResultsetHardLimit = 400,
      paramAutosuggestPrefilterLimit  = 500,
      paramAutosuggestPostfilterLimit = 500
    }
  where
    paramK1 :: Float
    paramK1 = 1.5

    paramB :: VocabElements -> Float
    -- paramB VocabReadings   = 0.9
    paramB _    = 0.5

    paramFieldWeights :: VocabElements -> Float
    -- paramFieldWeights VocabReadings        = 20
    paramFieldWeights _ = 1

    featWeights VocabPriority = 5
    featWeights VocabInfo = 1
    featFun VocabPriority = LogarithmicFunction 1
    featFun VocabInfo = LogarithmicFunction 1
