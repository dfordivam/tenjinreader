{-# LANGUAGE FlexibleContexts #-}

module KanjiDB.Interface where

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
import Data.JMDict.AST.AST

import Text.MeCab (new)

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

makeVocabDetails
  :: DB.Vocab
  -> Int
  -> [DB.VocabMeaning]
  -> VocabDetails
makeVocabDetails v i ms = VocabDetails
  (VocabId i)
  (Vocab $ [Kana $ v ^. DB.vocabKanaWriting]) -- TODO Fix this
  ("")
  ("")
  (v ^. DB.vocabIsCommon)
  (Rank <$> v ^. DB.vocabFreqRank)
  (JlptLevel <$> v ^. DB.vocabJlptLevel)
  (WkLevel <$> v ^. DB.vocabWkLevel)
  (WikiRank <$> v ^. DB.vocabWikiRank)
  (getMeanings (DB._vocabMeaningMeaning <$> ms))

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

getVocabs :: DBMonad [(VocabId, VocabDetails)]
getVocabs = do
  let query = (all_ (DB._jmdictVocab DB.jmdictDb))
  vs <- selectListQuery $ query
  let fun k = case (DB.getKey $ primaryKey k) of
                (Just i) -> Just (i,k)
                Nothing -> Nothing
      vis :: [(Int, DB.Vocab)]
      vis = mapMaybe fun vs

      query2 vId = map DB._vocabVocabMeaningMeaning $
        filter_ (\v ->
          (v ^. DB.vocabVocabMeaningVocab) ==. val_ vId) $
        (all_ (DB._jmdictVocabVocabMeaning DB.jmdictDb))

      fun2 (i,v) = do
        msIds <- selectListQuery $ query2 (primaryKey v)
        ms <- catMaybes <$> mapM fun3 msIds
        return $ (VocabId i, makeVocabDetails v i ms)

      fun3 mId = do
        conn <- ask
        liftIO $ withDatabase conn (runSelectReturningOne $
          lookup (DB._jmdictVocabMeaning DB.jmdictDb) mId)

  mapM fun2 vis

getKanjiVocabs
  :: KanjiId
  -> DBMonad [VocabId]
getKanjiVocabs k = do
  vs <- selectListQuery $ query (DB.makeKey $ Just k)
  return (DB.getKeys vs)
  where
    query k = map DB._vocabKanjiVocab $
              filter_ (\r -> r ^. DB.vocabKanjiKanji ==. val_ k)
                (all_ (DB._jmdictVocabKanji DB.jmdictDb))

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

getVocabKanjis
  :: VocabId
  -> DBMonad [KanjiId]
getVocabKanjis v = do
  ks <- selectListQuery $ query (DB.makeKey $ Just v)
  return (DB.getKeys ks)
  where
    query v = map DB._vocabKanjiKanji $
              filter_ (\r -> r ^. DB.vocabKanjiVocab ==. val_ v)
                (all_ (DB._jmdictVocabKanji DB.jmdictDb))

getMecab =  new ["mecab", "-d"
      , "/home/divam/nobup/mecab-tools/mecab-ipadic-neologd-output-files"]

isKana c = c > l && c < h
  where l = chr $ 12352
        h = chr $ 12543

makeFurigana :: KanjiPhrase -> ReadingPhrase -> Either Text Vocab
makeFurigana (KanjiPhrase k) (ReadingPhrase r) = Vocab <$> (f kgs r)
  where
    kgs = T.groupBy (\ a b -> (isKana a) == (isKana b)) k
    f :: [Text] -> Text -> Either Text [KanjiOrKana]
    f [] r
      | T.null r = Right []
      | otherwise = Right [Kana r]

    f (kg:[]) r
      | T.null r = Left "Found kg, but r is T.null"
      | otherwise = if r == kg
        then Right [Kana r]
        else if (isKana (T.head kg))
          then Left $ "Found kana not equal to r: " <> kg <> ", " <> r
          else Right [KanjiWithReading (Kanji kg) r]

    f (kg:kg2:kgs) r
      | T.null r = Left "r is T.null"
      | otherwise = if (isKana (T.head kg))
        then case (T.stripPrefix kg r) of
          (Just rs) -> ((Kana kg) :) <$> (f (kg2:kgs) rs)
          Nothing -> Left $ "stripPrefix: " <> kg <> ", " <> r
        else case (T.breakOn kg2 r) of
          (rk, rs)
            | T.null rk -> Left "breakOn fst T.null"
            | otherwise -> ((KanjiWithReading (Kanji kg) rk) :) <$> (f (kg2:kgs) rs)

testMakeFurigana = map (\(a,b) -> makeFurigana (KanjiPhrase a) (ReadingPhrase b))
  [("いじり回す", "いじりまわす")
  ,("弄りまわす", "いじりまわす")
  , ("弄り回す", "いじりまわす")
  , ("いじり回す", "いじりまわ") -- Fail
  ]


