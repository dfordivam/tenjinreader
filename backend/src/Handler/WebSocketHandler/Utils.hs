{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.WebSocketHandler.Utils where

import Import
import Control.Lens
import qualified Data.Text as T
import Data.Char
import Common
import Text.Pretty.Simple
import Yesod.WebSockets
import Text.MeCab
import NLP.Romkan
import Data.SearchEngine
import NLP.Snowball

type WsHandlerM = ReaderT WsHandlerEnv Handler

data WsHandlerEnv = WsHandlerEnv
  { kanjiSearchResult :: IORef [(KanjiId, KanjiDetails)]
  }

-- Hiragana ( 3040 - 309f)
-- Katakana ( 30a0 - 30ff)
--  Full-width roman characters and half-width katakana ( ff00 - ffef)
--   CJK unifed ideographs - Common and uncommon kanji ( 4e00 - 9faf)
--   CJK unified ideographs Extension A - Rare kanji ( 3400 - 4dbf)

-- Filter valid Kanji (no hiragana or katakana)
getKanjis :: Text -> [Text]
getKanjis inp = map T.pack $ map (:[]) $ filter isKanji $ T.unpack inp

isJP :: Text -> Bool
isJP = (all f) . T.unpack
  where f c = isKana c || isKanji c

-- 3040 - 30ff
isKana c = c > l && c < h
  where l = chr $ 12352
        h = chr $ 12543

-- 3400 - 9faf
isKanji c = c > l && c < h
 where l = chr $ 13312
       h = chr $ 40879

-- Convert Kanji to furigana (hiragana)
getKana :: Text -> Handler (Text)
getKana t = do
  m <- asks appMecabPtr
  nodes <- liftIO $ parseToNodes m t
  let feats = map nodeFeature nodes
      -- get the last part of the mecab output, this will include '*'
      readings :: [Text]
      readings = catMaybes $ map (headMay . reverse . (T.splitOn ",")) feats
      -- convert valid characters to hiragana
      f = map (toHiragana . toRoma) $ filter isJP readings

  return $ mconcat f
