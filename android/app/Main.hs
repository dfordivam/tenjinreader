module Main where

import Reflex.Dom
import TopWidget

main :: IO ()
main =
    mainWidgetWithHead'
      (headWidget
      , (\_ -> topWidget))
