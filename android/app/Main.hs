{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Language.Javascript.JSaddle.Warp
-- import Reflex.Dom.Core (mainWidget, mainWidgetWithCss)
-- import Reflex.Dom hiding (mainWidget, run)
import Reflex.Dom
-- import Data.FileEmbed
import TopWidget
import Protolude
-- import BulmaMinCss
-- main :: IO ()
-- main = mainWidget $ text "hi"

main :: IO ()
main =
    mainWidgetWithHead'
      (headWidget
      , (\_ -> topWidget))
