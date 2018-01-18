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
    ls = map (T.splitOn sep) $ filter filtLines $ T.lines t
    filtLines t
      | T.null t = False
      | T.head t == '#' = False
      | otherwise = True

    vs :: Vector (Vector Text)
    vs = V.fromList (map V.fromList ls)
    colNum = length <$> headMay ls

  case colNum of
    Nothing -> text "Parse Error"
    (Just 0) -> text "Parse Error"
    (Just n) -> do
      let fail = V.filter (\v -> V.length v /= n) vs
          correctCount = V.length vs - V.length fail


      text $ "Number of Fields/Columns : " <> tshow n
      text $ "Parsed " <> tshow correctCount <> " records correctly."
      if (not $ V.null fail)
        then do
          text $ "Failed to parse " <> tshow (V.length fail) <> " records."
          V.forM_ fail $ \v -> do
            text $ mconcat $ intersperse sep $ V.toList v
        else coloumnSelectionWidget vs sep2

data FieldType
  = MainField -- Having Kanji
  | ReadingField
  | MeaningField
  | ReadingNoteField
  | MeaningNoteField
  | NextReviewDateField
  | OtherField -- Ignore this
  deriving (Eq, Ord, Enum, Bounded, Show)

guessType f
  | T.null f = OtherField
  | T.all isDigit f = OtherField
  | (not $ T.any isLetter f) && T.any isKanji f = MainField
  | T.any isKana f = ReadingField
  | T.all isLetter f = MeaningField
  | otherwise = OtherField

validChoices f = Map.fromList $ map (\t -> (t, tshow t)) allT
  where
    allT = [MainField .. OtherField]

isValid fs = (nn mf && nn mn)
  where
    nn v = not (V.null v)
    mf = V.elemIndices MainField fs
    -- rf = V.elemIndices ReadingField fs
    mn = V.elemIndices MeaningField fs
    -- rnf = V.elemIndices ReadingNoteField fs
    -- mnf = V.elemIndices MeaningNoteField fs
    -- nrd = V.elemIndices NextReviewDateField fs

coloumnSelectionWidget vs sep2 = do
  let v = vs V.! 0
      cnum = V.length v
      attr = ("style" =: "width: 100%; overflow-x: auto;")

  elAttr "div" attr $ elClass "table" "table table-striped table-bordered" $ do
    checkEv <- btn "btn-primary" "Check Data"
    fDynV <- el "thead" $ do
      V.forM v $ \f -> do
        d <- el "th" $
          dropdown (guessType f) (constDyn $ validChoices f) $ def
            & dropdownConfig_attributes .~ (constDyn ("style" =: "width: 8em;"))
        return (value d)

    let fDyn = sequence fDynV

    dyn ((\f -> unless (isValid f) (text "Please select main field and meaning")) <$> fDyn)

    el "tbody" $ do
      V.forM_ (V.take 10 vs) $ \v -> do
        el "tr" $ V.forM_ v $ \f -> do
          el "td" $ text f

    widgetHold (return ())
      (checkDataWidget vs sep2 <$> tagDyn fDyn checkEv)
  return ()

checkDataWidget vs sep2 fs = do
  let
    mf = V.elemIndices MainField fs
    rf = V.elemIndices ReadingField fs
    mn = V.elemIndices MeaningField fs
    rnf = V.elemIndices ReadingNoteField fs
    mnf = V.elemIndices MeaningNoteField fs
    nrd = V.elemIndices NextReviewDateField fs

    checkOne :: Vector Text -> Bool
    checkOne v = c1 && c2 && c3 && c4
      where
        g :: Vector Int -> [Text]
        g ii = map T.strip $ concat $ map (\t -> T.splitOn sep2 t)
                       $ map (v V.!) (V.toList ii)
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
            el "td" $ text f
