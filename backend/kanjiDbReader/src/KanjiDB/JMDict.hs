{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module KanjiDB.JMDict
  (getJMDictEntries)
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
import Data.JMDict.AST
import Data.JMDict.AST.Parser hiding (isKana)
import Data.Char
import Text.Pretty.Simple
import qualified Data.Conduit.List
import Text.XML.Stream.Parse hiding (anyOf)
import Control.Monad.Trans.Resource
import Data.IORef
import Data.Conduit
import Control.Monad.IO.Class
import Control.Monad
import NLP.Japanese.Utils
import Common

import Text.MeCab (new)

getJMDictEntries :: FilePath -> IO [(Entry, VocabDetails)]
getJMDictEntries fp =
  runResourceT $ parseFile parseSetting fp
    $$ X.parseJMDict
    .| Data.Conduit.List.map makeAST
    .| Data.Conduit.List.mapMaybe (rightToMaybe)
    .| Data.Conduit.List.map (\x -> (x, makeVocabDetails x))
    .| Data.Conduit.List.consume

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
            g = (ReadingPhrase . katakanaToHiragana . unReadingPhrase . _readingPhrase)

checkFurigana e = case (e ^. entryKanjiElements) of
  [] -> True
  (ks) -> any (f (e ^. entryReadingElements)) ks
  where
    f (r:|_) k = case (r ^. readingRestrictKanji) of
      [] -> isRight $ makeFurigana (k ^. kanjiPhrase) (r ^. readingPhrase)
      (k:_) -> isRight $ makeFurigana (k) (r ^. readingPhrase)


makeVocabDetails :: Entry
  -> VocabDetails
makeVocabDetails e = VocabDetails
  (e ^. entryUniqueId)
  (makeVocab e)
  False -- TODO
  Nothing -- TODO
  (e ^.. entrySenses . traverse . senseGlosses . traverse . glossDefinition . to Meaning)

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
