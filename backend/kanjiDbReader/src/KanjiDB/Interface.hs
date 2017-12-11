{-# LANGUAGE FlexibleContexts #-}

module KanjiDB.Interface
  ( openKanjiDB
  , getKanjis
  , getRadicalKanjis
  , getKanjiRadicals)
  where

import Common

import qualified KanjiDB.Model as DB
import qualified KanjiDB.DB as DB

import Protolude
import Database.Beam
import Database.SQLite.Simple
import Database.Beam.Sqlite
import qualified Data.Set as Set
import Data.Time (LocalTime)
import qualified Data.Text as T
import Control.Lens

openKanjiDB = open "KanjiDatabase.sqlite"

type DBMonad = ReaderT Connection IO

selectListQuery query = do
  conn <- ask
  liftIO $
    withDatabase conn (runSelectReturningList $ select $ query)

makeKanjiDetails
  :: DB.Kanji
  -> Int
  -> [DB.KanjiMeaning]
  -> KanjiDetails
makeKanjiDetails k i ms = KanjiDetails
  (KanjiId i)
  (Kanji $ k ^. DB.kanjiCharacter)
  (Grade <$> k ^. DB.kanjiGrade)
  (Rank <$> k ^. DB.kanjiMostUsedRank)
  (JlptLevel <$> k ^. DB.kanjiJlptLevel)
  (getReadings $ k ^. DB.kanjiOnyomi)
  (getReadings $ k ^. DB.kanjiKunyomi)
  (getReadings $ k ^. DB.kanjiNanori)
  (WkLevel <$> k ^. DB.kanjiWkLevel)
  (getMeanings (DB._kanjiMeaningMeaning <$> ms))
  where
    getReadings :: Maybe Text -> [Reading]
    getReadings t = maybe [] (map Reading) (T.splitOn "," <$> t)
    getMeanings :: [Text] -> [Meaning]
    getMeanings ms = map Meaning $ map T.strip $ mconcat $ map (T.splitOn ";") ms



getKanjis :: DBMonad [(KanjiId, KanjiDetails)]
getKanjis = do
  let query = (all_ (DB._jmdictKanji DB.jmdictDb))
  ks <- selectListQuery $ query
  let fun k = case (DB.getKey $ primaryKey k) of
                (Just i) -> Just (i,k)
                Nothing -> Nothing
      kis :: [(Int, DB.Kanji)]
      kis = mapMaybe fun ks

      query2 kId = filter_ (\k ->
          (k ^. DB.kanjiMeaningKanji) ==. val_ kId) $
        (all_ (DB._jmdictKanjiMeaning DB.jmdictDb))

      fun2 (i,k) = do
        ms <- selectListQuery $ query2 (primaryKey k)
        return $ (KanjiId i, makeKanjiDetails k i ms)

  mapM fun2 kis

getRadicalKanjis
  :: RadicalId
  -> DBMonad [KanjiId]
getRadicalKanjis r = do
  ks <- selectListQuery $ query (DB.makeKey $ Just r)
  return (DB.getKeys ks)
  where query rId = DB._kanjiRadicalKanji <$>
          filter_ (\r -> r ^. DB.kanjiRadicalRadical ==. val_ rId)
            (all_ (DB._jmdictKanjiRadical DB.jmdictDb))

getKanjiRadicals
  :: KanjiId
  -> DBMonad [RadicalId]
getKanjiRadicals k = do
  rs <- selectListQuery $ query (DB.makeKey $ Just k)
  return (DB.getKeys rs)
  where query kId = DB._kanjiRadicalRadical <$>
          filter_ (\r -> r ^. DB.kanjiRadicalKanji ==. val_ kId)
            (all_ (DB._jmdictKanjiRadical DB.jmdictDb))

-- getVocabKanjis
--   :: VocabId
--   -> DBMonad [KanjiId]
-- getVocabKanjis v = do
--   ks <- selectListQuery $ query (DB.makeKey $ Just v)
--   return (DB.getKeys ks)
--   where
--     query v = map DB._vocabKanjiKanji $
--               filter_ (\r -> r ^. DB.vocabKanjiVocab ==. val_ v)
--                 (all_ (DB._jmdictVocabKanji DB.jmdictDb))


-- getKanjiVocabs
--   :: KanjiId
--   -> DBMonad [VocabId]
-- getKanjiVocabs k = do
--   vs <- selectListQuery $ query (DB.makeKey $ Just k)
--   return (DB.getKeys vs)
--   where
--     query k = map DB._vocabKanjiVocab $
--               filter_ (\r -> r ^. DB.vocabKanjiKanji ==. val_ k)
--                 (all_ (DB._jmdictVocabKanji DB.jmdictDb))
