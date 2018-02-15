{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}

module Mecab
  where

import Protolude hiding (to, (&))
import           Control.Lens
import qualified Data.Text as T
import Data.JMDict.AST
import Common
import KanjiDB
import NLP.Japanese.Utils
import Text.Earley as E
import Data.Char

import Control.Exception (assert)
import Text.MeCab
import Data.SearchEngine
import qualified Data.Vector as V
import qualified Data.Array as A
import Data.Array (Array)
import Handler.WebSocketHandler.Utils

-- Parse ruby characters and create Vocab using them
--
-- Example
-- どの天皇様の御代《みよ》であったか、女御《にょご》とか更衣《こうい》とかいわれる後宮《こうきゅう》がおおぜいいた中に
-- 御代《みよ》
-- 女御《にょご》
-- 更衣《こうい》
-- 後宮《こうきゅう》

-- 1. Strip the ruby info from original text and replace the surface (kanjis) with an identifier
-- 2. Do mecab
-- 3. parse the mecab output and patch/replace the identifier with the Vocab using ruby

newtype Surface = Surface { unSurface :: Text}
  deriving (Show, Eq)

newtype OriginalForm = OriginalForm { unOriginalForm :: Text}
  deriving (Show, Eq)

data MecabOutputTok = MecabOutputTok Surface (Maybe (OriginalForm, ReadingPhrase))
  deriving (Eq)

parseAndSearch :: Array EntryId VocabData
  -> VocabSearchEngineNoGloss
  -> MeCab
  -> Text
  -> IO AnnotatedDocument
parseAndSearch es se m t = do
  -- remove ruby and store data
  let
    paras = T.lines t
    onePara p = do
      let (pt, rMap) = stripRubyInfo p
      toks <- parseMecab m pt
      let toks2 = replaceRubyTokens toks rMap
      return (getParaData es se toks2)

  V.fromList <$> mapM onePara paras

getParaData
  :: Array EntryId VocabData
  -> VocabSearchEngineNoGloss
  -> [MecabOutputTok]
  -> AnnotatedPara
getParaData es se = map g
  where
    g (MecabOutputTok (Surface s) Nothing) = Left s
    g (MecabOutputTok surf (Just (o, r))) = Right (voc, vIds, True)
      where
        voc = getVocabFurigana (surf,r)

        ffIds = filter f eIds
        vIds = if null ffIds then eIds else ffIds
        eIds = query se [unOriginalForm o]
        origRead = getOriginalReading o voc
        f eId
          | isKanaOnly (unSurface surf) = True
          | otherwise = case arrayLookupMaybe es eId of
          Nothing -> False
          (Just e) -> elem origRead (e ^.. vocabEntry . entryReadingElements . traverse
                                    . readingPhrase . to (unReadingPhrase))

type RubyData = [(RubyIdentifier, (Surface, ReadingPhrase))]

newtype RubyIdentifier = RubyIdentifier Int
  deriving (Eq, Ord)


data TextTok = SimpleText Text | RubyDef Surface ReadingPhrase
  deriving (Show)

--
-- 重い外戚《がいせき》が背景になっていて、
-- 重い《0》が背景になっていて
stripRubyInfo :: Text -> (Text, RubyData)
stripRubyInfo tOrig = (foldl' ff ("", []) toks) & _2 %~ reverse
  where
    toks = parseTextForRuby tOrig
    ff (t, rs) (SimpleText t1) = (t <> t1, rs)
    ff (t, rs) (RubyDef s r) = (t <> t1, rd:rs)
      where t1 = "《" <> (T.pack $ show n) <> "》"
            rd = (RubyIdentifier n, (s,r))
            n = length rs

parseTextForRuby :: Text -> [TextTok]
parseTextForRuby tOrig = reverse $ loop $ reverse $ T.unpack tOrig
  where

    loop [] = []
    loop toks = case reverse $ ps of
      [] -> []
      (p:_) -> (fst p) : loop unc
      where
        (ps, (Report _ _ unc)) = allParses (E.parser gr) toks

    gr :: Grammar r (Prod r Char Char _)
    gr = mdo
      kana <- rule $ satisfy isKana
      kanji <- rule $ satisfy (\c -> ((isKanji) c) && (c /= '》') && (c /= '《'))
      notKanji <- rule $ satisfy (\c -> ((not . isKanji) c) && (c /= '》') && (c /= '《'))

      ruby <- rule $ (\r s -> RubyDef (Surface $ T.pack $ reverse s)
                       (ReadingPhrase $ T.pack $ reverse r))
        <$> (token '》' *> many kana <* token '《') <*> (many kanji)

      nonRuby <- rule $ (\t -> SimpleText (T.pack $ reverse t))
        <$> (many (notKanji <|> kanji))

      rule $ ruby <|> nonRuby


-- 重い    形容詞,自立,*,*,形容詞・アウオ段,基本形,重い,オモイ,オモイ
-- 《      記号,括弧開,*,*,*,*,《,《,《
-- 0       名詞,数,*,*,*,*,*
-- 》      記号,括弧閉,*,*,*,*,》,》,》
-- が      助詞,格助詞,一般,*,*,*,が,ガ,ガ

replaceRubyTokens :: [MecabOutputTok] -> RubyData -> [MecabOutputTok]
replaceRubyTokens toks rData
  | null rData = toks
  | otherwise = loop toks rData
  where
    loop [] _ = []
    loop (t1:n:t3:ts) (r:rs)
      | (t1 == startTok) && (t3 == endTok) = (getTok n r) : (loop ts rs)
      | otherwise = t1 : (loop (n:t3:ts) (r:rs))
    loop (t:ts) rs = t : (loop ts rs)

    startTok = tokF "《"
    tokF t = MecabOutputTok (Surface t) (Just (OriginalForm t, ReadingPhrase t))
    endTok = tokF "》"

    getTok n (rId, (s,r)) = assert (Just rId == (numTok n))
      (MecabOutputTok s (Just (OriginalForm (unSurface s), r)))

    numTok (MecabOutputTok (Surface n) Nothing) = RubyIdentifier <$> readMaybe (T.unpack n)
    numTok _ = Nothing

isKanaOnly :: T.Text -> Bool
isKanaOnly = (all f) . T.unpack
  where f = not . isKanji
          -- isKana c || (elem c ['、', '〜', 'ー'])

-- getOriginalReading "分かる" -> Vocab "分かり" -> "わかる"
getOriginalReading :: OriginalForm -> Vocab -> Text
getOriginalReading (OriginalForm term) (Vocab ks) = mconcat $ map f $ zip kgs1 ks
  where
    f (_,(KanjiWithReading _ r)) = r
    f (k, _) = katakanaToHiragana k
    kgs1 = T.groupBy (\ a b -> (isKana a) == (isKana b)) term

testGetOriginalReading :: [Either Text Bool]
testGetOriginalReading = map (\((a,b), (c,d)) ->
  (\v -> Right $ v == d) =<< (getOriginalReading (OriginalForm c) <$> makeFurigana (KanjiPhrase a) (ReadingPhrase b)))
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

getVocabFurigana :: (Surface, ReadingPhrase) -> Vocab
getVocabFurigana (Surface surf, reading)
  | isKanaOnly surf = Vocab [Kana surf]
  | otherwise = case makeFurigana (KanjiPhrase surf) (reading) of
    (Left _) -> Vocab [Kana surf]
    (Right v) -> v

parseMecab :: MeCab -> Text -> IO [MecabOutputTok]
parseMecab m txt = do
  let spaceReplaced = T.map rep txt
      rep ' ' = '�'
      rep c = c
  nodes <- parseToNodes m spaceReplaced
  let feats = map nodeFeature nodes
      unReplaceSpace (t,n)
        | T.any (== '�') t  = (T.replicate (T.length t) " "
                            , Nothing)
        | otherwise = (t,n)

  let
    makeTok ("", _) = Nothing
    makeTok (surf, Nothing) = Just (MecabOutputTok (Surface surf) Nothing)
    makeTok (surf, Just feat) = Just (MecabOutputTok (Surface surf) v)
      where v = Just (OriginalForm (_mecabNodeFeat7 feat)
                     , ReadingPhrase (_mecabNodeFeat8 feat))

  return $ catMaybes $ map (makeTok . unReplaceSpace) $ zip (map nodeSurface nodes)
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
