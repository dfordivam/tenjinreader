{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Common.Types where

import Data.Text
import qualified Data.Text as T
import Data.Aeson
import GHC.Generics
import Control.Lens

tshow :: (Show a) => a -> Text
tshow = T.pack . show

data ReaderControls = ReaderControls
  { _readerControls_fontSize :: Int
  , _readerControls_lineGap :: Int
  , _readerControls_isVertical :: Bool
  , _readerControls_lineCount :: Int
  , _readerControls_charPerLine :: Int
  , _readerControls_rowCount :: Int
  }
  deriving (Show, Eq, Generic)

newtype Vocab = Vocab { unVocab :: [KanjiOrKana] }
  deriving (Eq, Ord, Generic, Show)

data KanjiOrKana
  = KanjiWithReading Kanji Text
  | Kana Text
  deriving (Eq, Ord, Generic, Show)

newtype Kanji = Kanji { unKanji :: Text }
  deriving (Eq, Ord, Generic, Show)

type VocabId = EntryId
-- newtype VocabId = VocabId { unVocabId :: Int }
--   deriving (Eq, Ord, Generic, Show, ToJSON, FromJSON, Binary, Value)

type EntryId = Int

type AnnotatedPara = [(Either Text (Vocab, [VocabId], Bool))]