{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
module ImportWidget where

import FrontendCommon

import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector as V
import Data.Vector (Vector)
import NLP.Japanese.Utils
import Data.Char

separatorChoices = ((";" :: Text) =: ("SemiColon ;" :: Text))
  <> ("," =: "Comma ,")
  <> ("\t" =: "Tab")
  <> (" " =: "Space")
  <> (":" =: "Colon")

importWidgetTop
  :: AppMonad t m
  => AppMonadT t m ()
importWidgetTop = do
  ta <- textArea def

  ev <- btn "btn-default" "Parse"
  sep <- dropdown ";" (constDyn separatorChoices) def
  sep2 <- dropdown "," (constDyn separatorChoices) def
  let d = ((\a b c -> (a,b,c)) <$> value ta <*> value sep <*> value sep2)

  widgetHold (return ())
    (parseInput <$> (tagDyn d ev))
  return ()

parseInput (t, sep, sep2) = do
  let
    ls = map (map ((map T.strip) . (T.splitOn sep2))) $
      map (T.splitOn sep) $ filter filtLines $ T.lines t
    filtLines t
      | T.null t = False
      | T.head t == '#' = False
      | otherwise = True

    vs :: Vector (Vector [Text])
    vs = V.fromList (map V.fromList ls)
    colNum = length <$> headMay ls

  case colNum of
    Nothing -> text "Parse Error"
    (Just 0) -> text "Parse Error"
    (Just n) -> do
      let fail = V.filter (\v -> V.length v /= n) vs
          correctCount = V.length vs - V.length fail


      el "p" $ text $ "Number of Fields/Columns : " <> tshow n
      el "p" $ text $ "Parsed " <> tshow correctCount <> " records correctly."
      if (not $ V.null fail)
        then do
          text $ "Failed to parse " <> tshow (V.length fail) <> " records."
          V.forM_ fail $ \v -> do
            text $ tshow v
        else coloumnSelectionWidget vs

data FieldType
  = MainField -- Having Kanji
  | ReadingField
  | MeaningField
  | ReadingNoteField
  | MeaningNoteField
  | NextReviewDateField
  | IgnoreField -- Ignore this
  deriving (Eq, Ord, Enum, Bounded, Show)

guessType :: [Text] -> FieldType
guessType f
  | null f = IgnoreField
  | all (T.all isDigit) f = IgnoreField
  | all (\t -> (not $ T.any isAscii t) && T.any isKanji t) f = MainField
  | all (\t -> (not $ T.any isAscii t) && T.any isKana t) f = ReadingField
  | all (\t -> T.all isAscii t) f = MeaningField
  | otherwise = IgnoreField

validChoices f = Map.fromList $ map (\t -> (t, tshow t)) allT
  where
    allT = [MainField .. IgnoreField]

isValid fs = (nn mf && nn mn)
  where
    nn v = not (V.null v)
    mf = V.elemIndices MainField fs
    -- rf = V.elemIndices ReadingField fs
    mn = V.elemIndices MeaningField fs
    -- rnf = V.elemIndices ReadingNoteField fs
    -- mnf = V.elemIndices MeaningNoteField fs
    -- nrd = V.elemIndices NextReviewDateField fs

coloumnSelectionWidget vs = do
  let v = vs V.! 0
      cnum = V.length v
      attr = ("style" =: "width: 100%; overflow-x: auto;")

  elAttr "div" attr $ elClass "table" "table table-striped table-bordered" $ do
    fDynV <- el "thead" $ do
      V.forM v $ \f -> do
        d <- el "th" $
          dropdown (guessType f) (constDyn $ validChoices f) $ def
            & dropdownConfig_attributes .~ (constDyn ("style" =: "width: 8em;"))
        return (value d)

    let fDyn = sequence fDynV

    c <- dyn ((\f -> if (isValid f)
           then btn "btn-primary" "Next"
           else never <$ (text "Please select main field and meaning")) <$> fDyn)
    checkEv <- switchPromptly never c

    el "tbody" $ do
      V.forM_ (V.take 10 vs) $ \v -> do
        el "tr" $ V.forM_ v $ \f -> do
          el "td" $ text $ mconcat $ intersperse ", " f

    widgetHold (return ())
      (checkDataWidget vs <$> tagDyn fDyn checkEv)
  return ()

checkDataWidget vs fs = do
  let
    mf = V.elemIndices MainField fs
    rf = V.elemIndices ReadingField fs
    mn = V.elemIndices MeaningField fs
    rnf = V.elemIndices ReadingNoteField fs
    mnf = V.elemIndices MeaningNoteField fs
    nrd = V.elemIndices NextReviewDateField fs

    checkOne :: Vector [Text] -> Bool
    checkOne v = c1 && c2 && c3 && c4
      where
        g :: Vector Int -> [Text]
        g ii = concat $ map (v V.!) (V.toList ii)
        c1 = not $ all T.null (g mf)
        c2 = not $ all T.null (g mn)
        c3 = not $ any isNonJp (g mf)
        c4 = not $ any isNonJp (g rf)
        isNonJp t = not $ T.all (\c -> isKana c || isKanji c) t

    invalid = V.filter (not . checkOne) vs

  void $ if V.null invalid
    then text $ "All Good"
    else do
      el "table" $ el "tbody" $ do
        V.forM_ (V.take 10 invalid) $ \v -> do
          el "tr" $ V.forM_ v $ \f -> do
            el "td" $ text $ mconcat $ intersperse ", " f
