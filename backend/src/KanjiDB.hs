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
import NLP.Romkan
import NLP.Snowball
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
      jmDictFilePath = "/home/divam/nobup/jmdict/JMdict"
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
      KanjiMeanings -> unMeaning <$> ks ^. kanjiMeanings

    transformQry :: Term -> KanjiSearchFields -> Term
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
        (entrySenses . traverse . senseGlosses . traverse . glossDefinition)

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
