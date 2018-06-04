{-# LANGUAGE OverloadedStrings #-}

module Main where

import Language.Javascript.JSaddle.Warp
import Reflex.Dom.Core (mainWidget
                       , mainWidgetWithHead
                       , mainWidgetWithCss)
-- import Reflex.Dom hiding (mainWidget, run)
-- import Reflex.Dom
-- import Data.FileEmbed
import TopWidget
-- import BulmaMinCss
-- import FontAwesomeCss
import Protolude
-- main :: IO ()
-- main = mainWidget $ text "hi"

main :: IO ()
main =
  -- mainWidget $ topWidget
  run 3911 $
    mainWidgetWithHead headWidget
    -- mainWidgetWithCss (bulmaCssBS <> "\n" <> fontAwesomeCssBs)
      topWidget

