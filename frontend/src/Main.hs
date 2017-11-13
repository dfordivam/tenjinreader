{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Language.Javascript.JSaddle.Warp
-- import Reflex.Dom.Core (mainWidget)
-- import Reflex.Dom hiding (mainWidget, run)
import Reflex.Dom

import TopWidget

-- main :: IO ()
-- main = mainWidget $ text "hi"

main :: IO ()
main =
  mainWidget $ topWidget
  -- run 3911 $ mainWidget $ topWidget
