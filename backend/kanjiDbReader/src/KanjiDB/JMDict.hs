{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module KanjiDB.JMDict
  where

import Protolude hiding (to)
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
import KanjiDB.Search
import Common

import Text.MeCab (new)

test :: IO [Entry]
test = do
  ref <- newIORef 0
  es <- runResourceT $ parseFile parseSetting "/home/divam/nobup/jmdict/JMdict"
    $$ X.parseJMDict
    .| Data.Conduit.List.map makeAST
    .| Data.Conduit.List.mapM (\x -> liftIO $ printLeft ref x)
    .| Data.Conduit.List.take 10000000000
  print =<< readIORef ref
  return $ catMaybes $ fmap rightToMaybe es
    -- >>= pPrint
-- has _Left (Left 12)
-- anyOf (folded . _1 . _Just) (== 1) [(Just 5, Just 4),(Just 1, Nothing)]
printLeft ref v@(Right e) = do
  modifyIORef' ref (+ 1)
  let cond1 = anyOf (entrySenses . folded) fun e
      fun s = (not (null (s ^. senseRestrictKanji)))
        && (not (null (s ^. senseRestrictReading)))
  -- when cond (pPrint v)

  -- The reading are obsolete or sounding same, some valid cases
  -- when ((not $ checkReadingElementMatches e) && (not (null (e ^. entryKanjiElements))))
  --   (pPrint e)
  unless (checkFurigana e) (pPrint (e ^. entryKanjiElements, e ^. entryReadingElements))
  return v
printLeft ref v@(Left t) = do
  putStrLn (T.unpack t)
  return v

-- Separate ReadingElement (without Hiragana <-> Katakana isomorphism)
checkReadingElementMatches e =  f (e ^. entryReadingElements)
  where
    f (r:|[]) = True
    f (r:|rs) = all (== r') rs'
      where r' = g r
            rs' = fmap g rs
            g = (ReadingPhrase . toHiragana . unReadingPhrase . _readingPhrase)

checkFurigana e = case (e ^. entryKanjiElements) of
  [] -> True
  (ks) -> any (f (e ^. entryReadingElements)) ks
  where
    f (r:|_) k = case (r ^. readingRestrictKanji) of
      [] -> isRight $ makeFurigana (k ^. kanjiPhrase) (r ^. readingPhrase)
      (k:_) -> isRight $ makeFurigana (k) (r ^. readingPhrase)

-- getVocabs :: IO [(VocabId, VocabData)]
-- getVocabs = do
--   es <- runResourceT $ parseFile parseSetting "/home/divam/nobup/jmdict/JMdict"
--     $$ X.parseJMDict
--     .| Data.Conduit.List.map makeAST
--     .| Data.Conduit.List.consume


--   :: Entry
--   -> VocabDetails
makeVocabDetails e = VocabDetails
  (VocabId $ unEntryId $ e ^. entryUniqueId)
  (makeVocab e)
  False -- TODO
  Nothing -- TODO
  -- (getMeanings (DB._vocabMeaningMeaning <$> ms))

-- Furigana reading for vocab item
-- Pick first KanjiElement and first ReadingElement as its reading
-- For entries without KanjiElement, use the ReadingElement
makeVocab :: Entry -> Vocab
makeVocab e = case e ^. entryKanjiElements of
  [] -> kanaVocab
  (ks) -> case (asum $ fmap (rightToMaybe . (f (e ^. entryReadingElements))) ks) of
    (Just v) -> v
    (Nothing) -> kanaVocab
  where
    f (r:|_) k = case (r ^. readingRestrictKanji) of
      [] ->  makeFurigana (k ^. kanjiPhrase) (r ^. readingPhrase)
      (k:_) -> makeFurigana (k) (r ^. readingPhrase)
    kanaVocab = Vocab [Kana $ unReadingPhrase $
               e ^. entryReadingElements . to (NE.head) . readingPhrase]


getMecab =  new ["mecab", "-d"
      , "/home/divam/nobup/mecab-tools/mecab-ipadic-neologd-output-files"]

makeFurigana :: KanjiPhrase -> ReadingPhrase -> Either Text Vocab
makeFurigana (KanjiPhrase k) (ReadingPhrase r) = Vocab
  <$> (g (map toHiragana kgs) (toHiragana r))
  where
    g kgs r = case reverse kgs of
      (kl:krev) -> case T.stripSuffix kl r of
        (Just prfx) -> (\x -> x ++ [Kana kl]) <$> f (reverse krev) prfx
        Nothing -> f kgs r

    kgs = T.groupBy (\ a b -> (isKana a) == (isKana b)) k
    f :: [Text] -> Text -> Either Text [KanjiOrKana]
    f [] r
      | T.null r = Right []
      | otherwise = Right [Kana r]

    f (kg:[]) r
      | T.null r = Left "Found kg, but r is T.null"
      | otherwise = if kg `isSameAs` r
        then Right [Kana r]
        else if (isKana (T.head kg))
          then Left $ "Found kana not equal to r: " <> kg <> ", " <> r
          else Right [KanjiWithReading (Kanji kg) r]

    f (kg:kg2:kgs) r
      | T.null r = Left "r is null"
      | otherwise = if (isKana (T.head kg))
        then case (T.stripPrefix kg r) of
          (Just rs) -> ((Kana kg) :) <$> (f (kg2:kgs) rs)
          Nothing -> Left $ "stripPrefix: " <> kg <> ", " <> r
        else case (T.breakOn kg2 (T.tail r)) of
          (rk, rs)
            -> (KanjiWithReading (Kanji kg) (T.cons (T.head r) rk) :) <$> (f (kg2:kgs) rs)

testMakeFurigana = map (\(a,b) -> makeFurigana (KanjiPhrase a) (ReadingPhrase b))
  [("いじり回す", "いじりまわす")
  ,("弄りまわす", "いじりまわす")
  , ("弄り回す", "いじりまわす")
  , ("いじり回す", "いじりまわ") -- Fail
  , ("窺う", "うかがう")
  , ("黄色い", "きいろい")
  , ("額が少ない", "がくがすくない")
  -- , ("霞ヶ関", "かすみがせき")  -- Reading with no kanji
  -- , ("霞ケ関", "かすみがせき")  -- Reading with no kanji
  , ("ケント紙", "ケントし")
  , ("二酸化ケイ素", "にさんかケイそ")
  , ("ページ違反", "ぺーじいはん")
  , ("シェリー酒", "シェリーしゅ")
  ]

    -- f (kg:[]) r
    --   | T.null r = Left "Found kg, but r is T.null"
    --   | otherwise = if kg `isSameAs` r
    --     then Right [Kana r]
    --     else if (isKana (T.head kg))
    --       then Left $ "Found kana not equal to r: " <> kg <> ", " <> r
    --       else Right [KanjiWithReading (Kanji kg) r]

    -- f (kg:kg2:kgs) r
    --   | T.null r = Left "r is null"
    --   | otherwise = if (isKana (T.head kg))
    --     then case (T.splitAt (T.length kg) r) of
    --       (rf, rs) -> if isSameAs kg rf
    --         then ((Kana kg) :) <$> (f (kg2:kgs) rs)
    --         else Left $ "stripPrefix: " <> kg <> ", " <> r

    --     else case
    --       ((T.breakOn kg2 (T.tail r)),
    --         (T.break (isSameAsC $ T.head kg2) (T.tail r))) of
    --       ((rk1, rs1) , (rk2, rs2)) ->
    --         let (rk, rs) = if T.length rk1 > T.length rk2 then (rk1,rs1) else (rk2,rs2)
    --         in (KanjiWithReading (Kanji kg) (T.cons (T.head r) rk) :)
    --                    <$> (f (kg2:kgs) rs)


isSameAs t1 t2
  | T.length t1 == T.length t2 = all compareChars (zip (T.unpack t1) (T.unpack t2))
  | otherwise = False

isSameAsC c1 c2 = compareChars (c1, c2)

compareChars = f
  where
    f ('ヶ', c2) = elem c2 ['か', 'が','ヶ', 'ケ']
    f ('ケ', c2) = elem c2 ['か', 'が','ヶ', 'ケ']
    f (c1, c2) = c1 == c2
