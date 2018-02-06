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
import GHCJS.DOM.Types (File, fromJSVal, liftJSM, unStringOrArrayBuffer)
import GHCJS.DOM.EventM as DOM
import GHCJS.DOM.FileReader

separatorChoices = ((";" :: Text) =: ("SemiColon ;" :: Text))
  <> ("," =: "Comma ,")
  <> ("\t" =: "Tab")
  <> (" " =: "Space")
  <> (":" =: "Colon")

importWidgetTop
  :: AppMonad t m
  => AppMonadT t m ()
importWidgetTop = do
  -- ta <- textArea def

  -- ev <- btn "btn-default" "Parse"
  -- sep <- dropdown ";" (constDyn separatorChoices) def
  -- sep2 <- dropdown "," (constDyn separatorChoices) def
  -- let d = ((\a b c -> (a,b,c)) <$> value ta <*> value sep <*> value sep2)

  -- widgetHold (return ())
  --   (parseInput <$> (tagDyn d ev))

  importWaniKaniVocab
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

  rec
    c <- dyn ((\f -> if (isValid f)
           then btn "btn-primary" "Next"
           else never <$ (text "Please select main field and meaning")) <$> fDyn)
    checkEv <- switchPromptly never c

    fDyn <- elAttr "div" attr $
      elClass "table" "table table-striped table-bordered" $ do
        fDynV <- el "thead" $ do
          V.forM v $ \f -> do
            d <- el "th" $
              dropdown (guessType f) (constDyn $ validChoices f) $ def
                & dropdownConfig_attributes .~ (constDyn ("style" =: "width: 8em;"))
            return (value d)

        let fDyn = sequence fDynV

        el "tbody" $ do
          V.forM_ (V.take 10 vs) $ \v -> do
            el "tr" $ V.forM_ v $ \f -> do
              el "td" $ text $ mconcat $ intersperse ", " f
        return fDyn

  widgetHold (return ())
    (previewDataWidget vs <$> tagDyn fDyn checkEv)
  return ()

previewDataWidget vs fs = do
  let
    mf = V.elemIndices MainField fs
    rf = V.elemIndices ReadingField fs
    mn = V.elemIndices MeaningField fs
    rnf = V.elemIndices ReadingNoteField fs
    mnf = V.elemIndices MeaningNoteField fs
    nrd = V.elemIndices NextReviewDateField fs

    isNonJp t = not $ T.all (\c -> isKana c || isKanji c) t
    makeF :: Vector [Text] -> Either (FieldType, Vector [Text]) NewEntryUserData
    makeF v = NewEntryUserData <$> get1 <*> get2 <*> get3
      <*> pure (g rnf) <*> pure (g mnf)
      where
        g :: Vector Int -> [Text]
        g ii = concat $ map (v V.!) (V.toList ii)
        get1 = case NE.nonEmpty (g mf) of
          Nothing -> Left (MainField, v)
          Just xs -> if any isNonJp xs
            then Left (MainField, v)
            else Right xs

        get2 = case NE.nonEmpty (g mn) of
          Nothing -> Left (MeaningField,v)
          Just xs -> Right xs

        get3 = if any isNonJp (g rf)
          then Left (ReadingField,v)
          else Right (g rf)

    eitherV = fmap makeF $ V.toList vs
    (invalid, valid) = partitionEithers eitherV

  void $ if null invalid
    then do
      text $ "All Good"
      previewEntriesWidget valid

    else do
      el "h3" $ text "Problem in these values"
      elClass "table" "table table-striped table-bordered" $ el "tbody" $ do
        forM_ (take 10 invalid) $ \(ft,v) -> do
          let ii = V.elemIndices ft fs
          el "tr" $ (flip V.imapM_) v $ \i f -> do
            let attr = if V.elem i ii then ("style" =: "background: #FF7043;") else Map.empty
            elAttr "td" attr $ text $ mconcat $ intersperse ", " f

-- Choices for each Entry
-- SrsEntry -> Either [VocabId] NewEntry
-- Wakaru -> Only if (NonEntry VocabId)
-- Ignore - Do Nothing
previewEntriesWidget ds = do
  let
    preview ds = do
      elClass "table" "table table-striped table-bordered" $ do
        el "thead" $ do
          el "th" $ text ""
          el "th" $ text "Main"
          el "th" $ text "Reading"
          el "th" $ text "Meaning"
          el "th" $ text "RNotes"
          el "th" $ text "MNotes"

        let f l = if T.length t > 25 then (T.take 25 t) <> ".." else t
              where t = mconcat $ intersperse ", " l
        el "tbody" $ forM (zip [1..] ds) $ \(i,d) -> el "tr" $ do
          el "td" $ text $ tshow i
          el "td" $ text $ f (NE.toList $ mainField d)
          el "td" $ text $ f (readingField d)
          el "td" $ text $ f (NE.toList $ meaningField d)
          el "td" $ text $ f (readingNotesField d)
          el "td" $ text $ f (meaningNotesField d)

  preview ds
  -- searchEv <- btn "btn-primary" "Next2"

  -- let req = 
  -- resp <- getWebSocketResponse req
  -- widgetHold (return ())
  --   (searchResultWidget <$> resp)
  return ()

getTextFromFile :: (MonadWidget t m) => Event t File -> m (Event t Text)
getTextFromFile fEv = do
  fileReader <- liftJSM newFileReader
  performEvent_ (fmap (\f -> readAsText fileReader (Just f) (Nothing :: Maybe Text)) fEv)
  e <- wrapDomEvent fileReader (`DOM.on` load) . liftJSM $ do
    v <- getResult fileReader
    s <- mapM fromJSVal $ fmap unStringOrArrayBuffer v
    return . fmap T.pack $ join s
  return (fmapMaybe identity e)

importWaniKaniVocab
  :: AppMonad t m
  => AppMonadT t m ()
importWaniKaniVocab = do
  el "div" $ do
    el "h3" $ text "Import from Wanikani"
    el "p" $ text "Please create vocabulary.txt or kanji.txt using https://wanikanitoanki.com/"
    el "p" $ text "Then import either of these files using below widget"

  tEv <- el "div" $ do
    files <- value <$> fileInput def
    ev <- btn "btn-default" "Read File"
    let
      fEv = fmapMaybe headMay (tagDyn files ev)
    getTextFromFile fEv

  let
    sep = ";"
    sep2 = ","
    makeReq t = ImportSearchFields $
      zip [1..] $ catMaybes $ map getMainField ls
      where
        ls :: [[[Text]]]
        ls = map (map ((map T.strip) . (T.splitOn sep2))) $
          map (T.splitOn sep) $ filter filtLines $ T.lines t
    filtLines t
      | T.null t = False
      | T.head t == '#' = False
      | otherwise = True

    getMainField :: [[Text]] -> Maybe (NonEmpty Text)
    getMainField (_:_:f:_)
      | all (\t -> (not $ T.any isAscii t)
          && T.any isKanji t) f = NE.nonEmpty f
      | otherwise = Nothing
    getMainField _ = Nothing
    req = makeReq <$> tEv

  reqDyn <- holdDyn (ImportSearchFields []) req
  resp <- getWebSocketResponse req
  showWSProcessing req resp
  impDone <- widgetHoldWithRemoveAfterEvent
    (importSelectionWidget <$> resp)
  void $ widgetHoldWithRemoveAfterEvent
    ((text "Import Successful!" >> return req) <$ impDone)

instance Ord NewEntryOp where
  compare (AddVocabs _) (AddVocabs _) = EQ
  compare (MarkWakaru _) (MarkWakaru _) = EQ
  compare (MarkWakaru _) (AddVocabs _) = LT
  compare (AddVocabs _) (MarkWakaru _) = GT
  compare _ _ = EQ

importChoices vIds = ((MarkWakaru vIds) =: ("Mark 知る"))
  <> ((AddVocabs vIds) =: ("Add to Srs"))

importSelectionWidget :: AppMonad t m
  => ([(Text, Either () SrsEntryId)]
     , [(NonEmpty VocabId, Text)]
     , [NonEmpty Text])
  -> AppMonadT t m (Event t ())
importSelectionWidget (alreadyInDb, newEs, notFound) = el "div" $ do
  let showNewEntry (vIds, mf) = el "tr" $ do
        el "td" $ text mf
        dd <- el "td" $ dropdown (MarkWakaru vIds)
          (constDyn $ importChoices vIds) def
        return $ value dd

      showNotFound [] = return ()
      showNotFound es = do
        el "div" $ do
          el "h5" $ text "Not found in database: "
          el "p" $ text $ mconcat $ intersperse ", " $ map fold es
      showAlreadyInDb [] = return ()
      showAlreadyInDb es = do
        let
          isSrs = [x | (x, Right _) <- es]
          wakaru = [x | (x, Left _) <- es]
        el "div" $ do
          el "h5" $ text "Already in Srs: "
          el "p" $ text $ mconcat $ intersperse ", " $ isSrs
        el "div" $ do
          el "h5" $ text "Already marked 知る: "
          el "p"  $ text $ mconcat $ intersperse ", " $ wakaru

  showNotFound notFound
  showAlreadyInDb alreadyInDb
  newEsDyn <- sequence <$> do
    if null newEs
      then return ([])
      else el "div" $ do
        el "h5" $ text "New Entries"
        el "div" $ elClass "table" "table table-striped table-bordered" $ el "tbody" $
          mapM showNewEntry newEs
  addEv <- if null newEs
    then return never
    else btn "btn-primary" "Import"
  getWebSocketResponse $ ImportData <$> tagDyn newEsDyn addEv
