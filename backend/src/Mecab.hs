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
import qualified Data.Array as A
import Data.Array (Array)
import Handler.WebSocketHandler.Utils

parseAndSearch :: Array EntryId VocabData
  -> VocabSearchEngineNoGloss
  -> MeCab
  -> Text
  -> IO AnnotatedDocument
parseAndSearch es se m t = do
  feats <- liftIO $ mapM (parseMecab m) (T.lines t)
  let
    fun ("", _) = Nothing
    fun (surf, Nothing) = Just (Left surf)
    fun (surf, Just feat) = Just $ Right $ (voc, filter f eIds , True)
      where
        voc = getVocabFurigana (surf,reading)
        term = (_mecabNodeFeat7 feat)
        reading = (_mecabNodeFeat8 feat)
        eIds = query se [term]
        origRead = getOriginalReading term voc
        f eId
          | isKanaOnly surf = True
          | otherwise = case arrayLookupMaybe es eId of
          Nothing -> False
          (Just e) -> elem origRead (e ^.. vocabEntry . entryReadingElements . traverse
                                    . readingPhrase . to (unReadingPhrase))
  return $ V.fromList $ map (catMaybes . (fmap fun)) feats

isKanaOnly :: T.Text -> Bool
isKanaOnly = (all f) . T.unpack
  where f = not . isKanji
          -- isKana c || (elem c ['、', '〜', 'ー'])

-- getOriginalReading "分かる" -> Vocab "分かり" -> "わかる"
getOriginalReading :: Text -> Vocab -> Text
getOriginalReading term (Vocab ks) = mconcat $ map f $ zip kgs1 ks
  where
    f (_,(KanjiWithReading _ r)) = r
    f (k, _) = katakanaToHiragana k
    kgs1 = T.groupBy (\ a b -> (isKana a) == (isKana b)) term

testGetOriginalReading = map (\((a,b), (c,d)) ->
  (\v -> Right $ v == d) =<< (getOriginalReading c <$> makeFurigana (KanjiPhrase a) (ReadingPhrase b)))
  [ (("いじり回す", "いじりまわす")
   , ("いじり回す", "いじりまわす"))
  , (("弄りまわし", "いじりまわし")
   , ("弄りまわす", "いじりまわす"))
  , (("弄り回せ", "いじりまわせ")
   , ("弄り回す", "いじりまわす"))
  , (("窺い", "うかがい")
   , ("窺う", "うかがう"))
  , (("黄色く", "きいろく")
   , ("黄色い", "きいろい"))
  , (("額が少なく", "がくがすくなく")
   , ("額が少ない", "がくがすくない"))
  , (("ケント紙", "ケントし")
   , ("ケント紙", "けんとし"))
  , (("二酸化ケイ素", "にさんかケイそ")
   , ("二酸化ケイ素", "にさんかけいそ"))
  , (("ページ違反", "ぺーじいはん")
   , ("ページ違反", "ぺーじいはん"))
  , (("シェリー酒", "シェリーしゅ")
   , ("シェリー酒", "しぇりーしゅ"))
  , (("パン屋", "ぱんや")
   , ("パン屋", "ぱんや"))
  , (("命", "いのち")
   , ("命", "いのち"))
  ]

getVocabFurigana (surf, reading)
  | isKanaOnly surf = Vocab [Kana surf]
  | otherwise = case makeFurigana (KanjiPhrase surf) (ReadingPhrase reading) of
    (Left _) -> Vocab [Kana surf]
    (Right v) -> v

parseMecab :: MeCab -> Text -> IO ([(Text, Maybe MecabNodeFeatures)])
parseMecab m t = do
  let spaceReplaced = T.map rep t
      rep ' ' = '�'
      rep c = c
  nodes <- parseToNodes m spaceReplaced
  let feats = map nodeFeature nodes
      unReplaceSpace (t,n)
        | T.any (== '�') t  = (T.replicate (T.length t) " "
                            , Nothing)
        | otherwise = (t,n)
  return $ map unReplaceSpace $ zip (map nodeSurface nodes)
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
