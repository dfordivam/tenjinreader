{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module KanjiDB where

import Common
import Model
import Radicals
import KanjiDB.Interface

import Protolude
import Control.Lens.TH
import Data.Binary
import Data.SearchEngine
import Data.Ix
import qualified Data.Set as Set
import qualified Data.Map as Map
import Text.MeCab

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
  VocabKanji
  | VocabFurigana
  | VocabMeanings
  deriving (Eq, Ord, Enum, Bounded, Ix, Show)

createDBs ::
  MeCab
  -> IO (KanjiDb, VocabDb, RadicalDb)
createDBs mecab = do
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
      ks <- getKanjis
      kDb <- Map.fromList <$> mapM getKanjiData ks

      vs <- getVocabs
      vDb <- Map.fromList <$> mapM getVocabData vs

      rDb <- Map.fromList <$> mapM
        (\r -> do
            ks <- getRadicalKanjis r
            return (r, Set.fromList ks))
        (Map.keys radicalTable)

      return (kDb, vDb, rDb)
  runReaderT f conn
