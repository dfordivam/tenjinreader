{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
module KanjiDB where

import Common
import Model
import Radicals
import KanjiDB.Interface

import Protolude
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
import System.Directory
import qualified Data.ByteString.Lazy

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
  , _vocabVocabSet   :: Set VocabId
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
  ex <- doesFileExist kanjibin
  if ex
    then do
      bs <- Data.ByteString.Lazy.readFile kanjibin
      return $ Data.Binary.decode bs
    else do
      conn <- openKanjiDB
      let
        getKanjiData (kId,k) = do
          vs <- getKanjiVocabs kId
          rs <- getKanjiRadicals kId
          return $ (kId
                   , KanjiData k (Set.fromList vs)
                     (Set.fromList rs))

        getVocabData (vId,v) = do
          ks <- getVocabKanjis vId
          return $ (vId
                   , VocabData v (Set.empty)
                     (Set.fromList ks))
        f = do
          ks <- KanjiDB.Interface.getKanjis
          liftIO $ putStrLn $ "Got Kanjis: " <> (T.pack $ show (length ks))
          liftIO $ putStrLn $ "Encode Size: " <> (T.pack $ show (Data.ByteString.Lazy.length $ Data.Binary.encode ks))
          kDb <- Map.fromList <$> mapM getKanjiData ks

          liftIO $ putStrLn $ "Got KanjiDB: " <> (T.pack $ show (Map.size kDb))
          liftIO $ putStrLn $ "Encode Size: " <> (T.pack $ show (Data.ByteString.Lazy.length $ Data.Binary.encode kDb))

          vs <- getVocabs
          liftIO $ putStrLn $ "Got Vocab: " <> (T.pack $ show (length vs))
          liftIO $ putStrLn $ "Encode Size: " <> (T.pack $ show (Data.ByteString.Lazy.length $ Data.Binary.encode vs))

          vDb <- Map.fromList <$> mapM getVocabData vs

          liftIO $ putStrLn $ "Got VocabDB: " <> (T.pack $ show (Map.size vDb))
          liftIO $ putStrLn $ "Encode Size: " <> (T.pack $ show (Data.ByteString.Lazy.length $ Data.Binary.encode vDb))

          rDb <- Map.fromList <$> mapM
            (\r -> do
                ks <- getRadicalKanjis r
                return (r, Set.fromList ks))
            (Map.keys radicalTable)

          return (kDb, vDb, rDb)
      ret <- runReaderT f conn
      Data.ByteString.Lazy.writeFile kanjibin (Data.Binary.encode ret)
      return ret

type KanjiSearchEngine = SearchEngine KanjiDetails KanjiId KanjiSearchFields NoFeatures

type VocabSearchEngine = SearchEngine VocabDetails VocabId VocabSearchFields NoFeatures

data KanjiSearchFields =
  KanjiCharacter
  | KanjiOnReading
  | KanjiKuReading
  | KanjiNaReading
  | KanjiMeanings
  deriving (Eq, Ord, Enum, Bounded, Ix, Show)

data VocabSearchFields =
  VocabFurigana
  | VocabMeanings
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

getVocabSE :: VocabDb -> VocabSearchEngine
getVocabSE db = insertDocs docs init
  where
    init = initSearchEngine conf vocabSearchRankParams
    conf = SearchConfig _vocabId extractTerms transformQry (const noFeatures)
    docs = map _vocabDetails $ Map.elems db
    extractTerms :: VocabDetails -> VocabSearchFields -> [Term]
    extractTerms v sf = case sf of
      VocabFurigana -> [vocabToKana $ v ^. vocab]
      VocabMeanings -> unMeaning <$> v ^. vocabMeanings

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
    paramB VocabFurigana   = 0.9
    paramB _    = 0.5

    paramFieldWeights :: VocabSearchFields -> Float
    paramFieldWeights VocabFurigana        = 20
    paramFieldWeights _ = 1

-- Utility Functions

-- Hiragana ( 3040 - 309f)
-- Katakana ( 30a0 - 30ff)
--  Full-width roman characters and half-width katakana ( ff00 - ffef)
--   CJK unifed ideographs - Common and uncommon kanji ( 4e00 - 9faf)
--   CJK unified ideographs Extension A - Rare kanji ( 3400 - 4dbf)

-- Filter valid Kanji (no hiragana or katakana)
getKanjis :: Text -> [Text]
getKanjis inp = map T.pack $ map (:[]) $ filter isKanji $ T.unpack inp

isJP :: Text -> Bool
isJP = (all f) . T.unpack
  where f c = isKana c || isKanji c

-- 3040 - 30ff
isKana c = c > l && c < h
  where l = chr $ 12352
        h = chr $ 12543

-- 3400 - 9faf
isKanji c = c > l && c < h
 where l = chr $ 13312
       h = chr $ 40879
