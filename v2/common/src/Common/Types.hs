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
  , _readerControls_fontGap :: Int
  , _readerControls_isVertical :: Bool
  , _readerControls_lineCount :: Int
  , _readerControls_wordPerLine :: Int
  , _readerControls_rowCount :: Int
  }
  deriving (Show, Eq, Generic)
