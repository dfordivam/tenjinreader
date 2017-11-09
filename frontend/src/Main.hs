{-# LANGUAGE OverloadedStrings #-}

module Main where

import Language.Javascript.JSaddle.Warp
import Reflex.Dom.Core (mainWidget)
import Reflex.Dom hiding (mainWidget, run)

-- import Reflex.Dom

-- main :: IO ()
-- main = mainWidget $ text "hi"

main :: IO ()
main = run 3911 $ mainWidget $ text "hi"
