{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
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
import qualified Data.Vector as V
import qualified Data.Array as A
import Data.Array (Array)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Set as Set
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Text.MeCab
import NLP.Romkan as NLP
import NLP.Snowball as NLP
import NLP.Tokenize.Text as NLP
import NLP.Japanese.Utils as Utils
import System.Directory
import qualified Data.ByteString.Lazy
import Data.JMDict.AST
import Data.BTree.Primitives (Value, Key)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Binary.Orphans

type KanjiDb = Array KanjiId KanjiData

data KanjiData = KanjiData
  { _kanjiDetails    :: KanjiDetails
  , _kanjiVocabSet   :: Set VocabId
  , _kanjiRadicalSet :: Set RadicalId
  }
  deriving (Generic, Binary)

type VocabDb = Array VocabId VocabData

data VocabData = VocabData
  { _vocabDetails    :: VocabDetails
  , _vocabEntry      :: Entry
  , _vocabKanjiSet   :: Set KanjiId
  }
  deriving (Generic, Binary)

type RadicalDb = Array RadicalId (Set KanjiId)

type SentenceDb = Array SentenceId SentenceData

type VocabSentenceDb = Map EntryId (Set SentenceId)

type BooksDb = Array BookId (Text, AnnotatedDocument)

type ArticlesDb = Array ArticleId (Text, AnnotatedDocument)

-- Sentence db
-- Each sentence -> [Vocab] / [Entry]
-- Entry -> [Sentence]
instance Binary Kanji
instance Value Kanji
instance Binary Rank
instance Value Rank
instance Binary Meaning
instance Value Meaning
instance Binary MeaningNotes
instance Value MeaningNotes
instance Binary Reading
instance Value Reading
instance Binary ReadingNotes
instance Value ReadingNotes
instance Binary Grade
instance Value Grade
instance Binary StrokeCount
instance Value StrokeCount
instance Binary JlptLevel
instance Value JlptLevel
instance Binary WikiRank
instance Value WikiRank
instance Binary WkLevel
instance Value WkLevel
instance Binary RadicalId
instance Value RadicalId
instance Binary KanjiId
instance Value KanjiId
instance Binary SrsLevel
instance Value SrsLevel
instance Binary Vocab
instance Value Vocab
instance Binary KanjiOrKana
instance Value KanjiOrKana
instance Binary KanjiDetails
instance Value KanjiDetails
instance Binary VocabDetails
instance Value VocabDetails
instance Binary ReadingType
instance Value ReadingType


instance Binary Sense
instance Value Sense
instance Binary EntryId
instance Value EntryId

instance Binary Entry
instance Value Entry
instance Binary KanjiElement
instance Value KanjiElement
instance Binary ReadingElement
instance Value ReadingElement
instance Binary Priority
instance Value Priority
instance Binary ReadingPhrase
instance Value ReadingPhrase
instance Binary KanjiPhrase
instance Value KanjiPhrase
instance Binary KanjiInfo
instance Value KanjiInfo
instance Binary ReadingInfo
instance Value ReadingInfo
instance Binary PartOfSpeech
instance Value PartOfSpeech
instance Binary Auxiliary
instance Value Auxiliary
instance Binary NounType
instance Value NounType
instance Binary VerbType
instance Value VerbType
instance Binary Dialect
instance Value Dialect
instance Binary Gloss
instance Value Gloss
instance Binary RegularVerb
instance Value RegularVerb
instance Binary IrregularVerb
instance Value IrregularVerb
instance Binary VerbEnding
instance Value VerbEnding
instance Binary IsTransitive
instance Value IsTransitive
instance Binary SpecialVerb
instance Value SpecialVerb
instance Binary Adverb
instance Value Adverb
instance Binary Adjective
instance Value Adjective
instance Binary SenseMisc
instance Value SenseMisc
instance Binary LanguageSource
instance Value LanguageSource
instance Binary SenseField
instance Value SenseField

instance Binary SentenceId
instance Binary BookId
instance Binary ArticleId
instance Binary SentenceData
instance Value SentenceId
deriving instance Ix SentenceId
deriving instance Ix RadicalId
deriving instance Ix EntryId
deriving instance Ix KanjiId
deriving instance Ix BookId
deriving instance Ix ArticleId
makeLenses ''KanjiData
makeLenses ''VocabData
--
makeArray ls = A.array (minimum $ map fst ls, maximum $ map fst ls) ls

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
            vDb = makeArray $
              map (getVocabData ksMap) (zip [1..] es)

          kDb <- makeArray <$> getKanjiData vDb ks

          rDb <- makeArray <$> mapM
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
  -> (Int, (Entry, VocabDetails))
  -> (VocabId, VocabData)
getVocabData ksMap (i, (e,v)) =
  (EntryId i, VocabData nv ne kSet)
  where
    nv = v & vocabId .~ (EntryId i)
    ne = e & entryUniqueId .~ (EntryId i)
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
    ksMap = foldl' f (Map.empty) vDb
    f m v = Set.foldr' (Map.alter p) m (v ^. vocabKanjiSet)
      where
        p (Just s) = Just $ Set.insert vId s
        p Nothing = Just $ Set.singleton vId
        vId = (v ^. vocabEntry . entryUniqueId)

    getKD (kId,k) = do
      rs <- getKanjiRadicals kId
      return (kId, KanjiData k vSet (Set.fromList rs))
      where vSet = maybe Set.empty identity $ Map.lookup kId ksMap

type KanjiSearchEngine = SearchEngine KanjiDetails KanjiId KanjiSearchFields KanjiFeatures

data KanjiFeatures
  = KanjiMostUsedRank
  deriving (Eq, Ord, Enum, Bounded, Ix, Show)

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
    conf = SearchConfig _kanjiId extractTerms transformQry kanjiFeatureFun
    docs = map _kanjiDetails $ A.elems kDb
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

kanjiFeatureFun :: KanjiDetails -> KanjiFeatures -> Float
kanjiFeatureFun k _ = 5000 - (fromIntegral $ maybe 5000 unRank (k ^. kanjiMostUsedRank))

kanjiSearchRankParams :: SearchRankParameters KanjiSearchFields KanjiFeatures
kanjiSearchRankParams =
    SearchRankParameters {
      paramK1,
      paramB,
      paramFieldWeights,
      paramFeatureWeights     = const 1,
      paramFeatureFunctions   = const (LogarithmicFunction 1),
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

getSentenceDbs parseF sFp linkFp = do
  let binFile = "sentencedb.bin"
  readDbOrCreate binFile $ createSentenceDbs parseF sFp linkFp

createSentenceDbs :: (Text -> IO AnnotatedDocument)
  -> Text
  -> Text
  -> IO (SentenceDb, VocabSentenceDb)
createSentenceDbs parseF sFp linkFp = do
  sT <- T.readFile $ T.unpack sFp
  linkT <- T.readFile $ T.unpack linkFp

  let
    ffor = flip fmap
    treadMaybe = readMaybe . T.unpack
    ss :: [(Text,Text,Text)]
    ss = ffor (T.lines sT) $ \t -> case T.splitOn "\t" t of
      (t1:t2:t3:_) -> (t1,t2,t3)
      _ -> error "Error parsing sentence.csv"

    lks :: [(Int64, Int64)]
    lks = ffor (T.lines linkT) $ \t -> case T.splitOn "\t" t of
      (t1:t2:_) -> case (treadMaybe t1, treadMaybe t2) of
        (Just t1, Just t2) -> (t1,t2)
        _ -> error "Error parsing links.csv"
      _ -> error "Error parsing links.csv"

  ss2' <- forM ss $ \(i,l,s) -> case (l, treadMaybe i) of
      ("jpn", Just i) -> do
        a <- parseF s
        return $ Just $ Right (i, a V.! 0)
      ("eng", Just i) -> return $ Just $ Left (i, s)
      _ -> return $ Nothing

  let
    ss2 = catMaybes ss2'
    -- lkMapInit :: Map Int64 (Set Int64) -- empty sets, only create for jp sentences
    -- lkMapInit = Map.fromList $ [(fst x, Set.empty) | Right x <- ss2]

    -- lkMap :: Map SentenceId (Set Int64)
    -- lkMap = foldl' (\m (k,v) -> Map.adjust (Set.insert v) (SentenceId k) m)
    --   lkMapInit lks

    allValidLinks :: Map Int64 (Set Int64)
    allValidLinks = foldl' ff Map.empty lks
      where
        ff m (i,l) = case (Map.lookup i combinedMap, Map.lookup l combinedMap) of
          (Just _, Just _) -> Map.alter (alterInsert l) i m
          _ -> m
        alterInsert l Nothing = Just $ Set.singleton l
        alterInsert l (Just s) = Just $ Set.insert l s

    combinedMap :: Map Int64 (Either Text AnnotatedPara)
    combinedMap = Map.fromList $ ffor ss2 $ either
      (\(i,v) -> (i, Left v))
      (\(i,v) -> (i, Right v))

    allValidSets :: [Set Int64]
    allValidSets = snd $ foldl' ff ((allValidLinks, Set.empty), []) [(fst x) | Right x <- ss2]
      where
        ff ((m,done), sls) i = if Set.member i done
          then ((m,done), sls)
          else case Map.lookup i m of
            Nothing -> ((m, Set.insert i done), Set.singleton i : sls)
            (Just s) -> ((nm, Set.union done fullSet), fullSet : sls)
              where rSets = snd $ foldl' ff ((Map.delete i m, Set.insert i done), [])
                      (Set.toList s)
                    fullSet = Set.unions ((Set.insert i s):rSets)
                    nm = foldr Map.delete m (Set.toList fullSet)

    makeSd s = SentenceData <$> jp <*> pure njp
      where
        jp = NE.nonEmpty [x | (Right x) <- els]
        njp = [x | (Left x) <- els]
        els = catMaybes $ map (\i -> Map.lookup i combinedMap) $ Set.toList s

    sDb = makeArray $ zip (map SentenceId [1..]) $ catMaybes $ map makeSd allValidSets
    vocabSentDb = foldl' getVocabSentenceDb Map.empty $ A.assocs sDb
  return (sDb, vocabSentDb)

getVocabSentenceDb :: VocabSentenceDb -> (SentenceId, SentenceData) -> VocabSentenceDb
getVocabSentenceDb m (sId, s) = foldl' (flip (Map.alter f)) m es
  where
    es = s ^.. sentenceContents . traverse . traverse . _Right . _2 . traverse
    f Nothing = Just $ Set.singleton sId
    f (Just s) = Just $ Set.insert sId s

getBooksDb :: (Text -> IO AnnotatedDocument)
  -> IO BooksDb
getBooksDb parseF =
  readDbOrCreate "booksdb.bin" (createBooksArticleDb parseF "books" BookId)

getArticlesDb :: (Text -> IO AnnotatedDocument)
  -> IO ArticlesDb
getArticlesDb parseF =
  readDbOrCreate "articlesdb.bin" (createBooksArticleDb parseF "articles" ArticleId)

createBooksArticleDb parseF dirP c = do
  files <- listDirectory dirP
  let
    p f = do
      t <- T.readFile (dirP <> "/" <> f)
      ad <- parseF t
      let title = T.replace "_" " " $ snd $ T.breakOn "_" $ T.pack f
      return (title, ad)
  vs <- mapM p files
  return $ makeArray $ zip (map c [1..]) vs

readDbOrCreate :: (Binary a) => FilePath -> IO a -> IO a
readDbOrCreate binFile createF = do
  ex <- doesFileExist binFile
  if ex
    then do
      bs <- Data.ByteString.Lazy.readFile binFile
      return $ Data.Binary.decode bs
    else do
      ret <- createF
      Data.ByteString.Lazy.writeFile binFile (Data.Binary.encode ret)
      return ret
