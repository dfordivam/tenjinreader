{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
module TextReader where

import FrontendCommon

import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector as V

data TextReaderWidgetView
  = ListOfDocumentsView
  | EditDocumentView
  | ReadingView
  deriving (Eq)

textReaderTop
  :: AppMonad t m
  => AppMonadT t m ()
textReaderTop = do
  rec
    let visEv = leftmost
          [ListOfDocumentsView <$ closeEv
          , EditDocumentView <$ editEv
          , ReadingView <$ viewEv]
        viewEv = leftmost [viewEv1, viewEv2]
        editEv = leftmost [Nothing <$ newDocEv
                          , Just <$> editEv2]

    vis <- holdDyn ListOfDocumentsView visEv

    (viewEv1, newDocEv) <- handleVisibility ListOfDocumentsView vis $
      (documentListViewer (closeEv))

    (viewEv2) <- handleVisibility EditDocumentView vis $
      documentEditor editEv

    (closeEv, editEv2) <- handleVisibility ReadingView vis $
      readingPane viewEv
  return ()

documentListViewer
  :: AppMonad t m
  => Event t () -- refresh
  -> AppMonadT t m (Event t ReaderDocument
                   , Event t ())
documentListViewer refreshEv = do
  ev <- getPostBuild
  listEv <- getWebSocketResponse
    (ListDocuments <$ (leftmost [ev,refreshEv]))

  newDocEv <- button "New"
  evDyn <- widgetHold (return [])
    (viewList <$> listEv)
  resp <- getWebSocketResponse
    (switchPromptlyDyn (leftmost <$> evDyn))
  return $ (fmapMaybe identity resp
           , newDocEv)

viewList lss = do
  elClass "table" "table table-striped" $ do
    el "thead" $ do
      el "tr" $ do
        el "th" $ text "Title"
        el "th" $ text "Contents"
    el "tbody" $  do
      forM lss $ \(i, t, c) -> do
        (e,_) <- el' "tr" $ do
          el "td" $ text t
          el "td" $ text c
        return (ViewDocument i <$ domEvent Click e)

documentEditor
  :: AppMonad t m
  => Event t (Maybe ReaderDocument)
  -> AppMonadT t m (Event t ReaderDocument)
documentEditor editEv = divClass "" $ do
  let
    tiAttr = constDyn $ (("style" =: "width: 100%;")
                        <> ("placeholder" =: "Title"))
    taAttr = constDyn $ (("style" =: "width: 100%;")
                        <> ("rows" =: "10")
                        <> ("placeholder" =: "Contents"))

    titleSetEv = (maybe "" _readerDocTitle) <$> editEv
    contentSetEv = (maybe "" getContentText) <$> editEv
    getContentText (ReaderDocument _ _ c)
      = T.unlines $ map toT (V.toList c)
      where
        toT ap = (mconcat $ map getT ap)
        getT (Left t) = t
        getT (Right (v,_,_)) = vocabToText v

  rdDyn <- holdDyn Nothing editEv

  ti <- textInput $ def
    & textInputConfig_attributes .~ tiAttr
    & textInputConfig_setValue .~ titleSetEv
  ta <- textArea $ def
    & textAreaConfig_attributes .~ taAttr
    & textAreaConfig_setValue .~ contentSetEv

  saveEv <- button "Save"
  cancelEv <- button "Cancel"

  let evDyn = AddOrEditDocument
        <$> (fmap _readerDocId <$> rdDyn)
        <*> (value ti)
        <*> (value ta)
  annTextEv <- getWebSocketResponse
    $ tagDyn evDyn saveEv
  delEv <- delay 0.1 (fmapMaybe identity $ tagDyn rdDyn cancelEv)
  return $ leftmost [fmapMaybe identity annTextEv
    , delEv]

vocabRuby :: (_) => Dynamic t Int -> Dynamic t Bool -> Vocab -> m (_)
vocabRuby fontSizePctDyn visDyn (Vocab ks) = do
  let
    rubyAttr = (\s -> "style" =: ("font-size: " <> tshow s <>"%;")) <$> fontSizePctDyn
    g r True = r
    g _ _ = ""
    f (Kana k) = text k
    f (KanjiWithReading (Kanji k) r)
      = elDynAttr "ruby" rubyAttr $ do
          text k
          el "rt" $ dynText (g r <$> visDyn)
  (e,_) <- el' "span" $ mapM f ks
  return $ (domEvent Click e, domEvent Mouseenter e, domEvent Mouseleave e)

lineHeightOptions = Map.fromList $ (\x -> (x, (tshow x) <> "%"))
  <$> ([100,150..400]  :: [Int])

fontSizeOptions = Map.fromList $ (\x -> (x, (tshow x) <> "%"))
  <$> ([80,85..200]  :: [Int])

readingPane :: AppMonad t m
  => Event t ReaderDocument
  -> AppMonadT t m (Event t (), Event t ReaderDocument)
readingPane docEv = do
  closeEv <- button "Close"
  editEv <- button "Edit"
  rdDyn <- holdDyn Nothing (Just <$> docEv)
  widgetHold (return ())
    (readingPaneView <$> docEv)
  return (closeEv
         , fmapMaybe identity $ tagDyn rdDyn editEv)

readingPaneView :: AppMonad t m
  => ReaderDocument
  -> AppMonadT t m ()
readingPaneView (ReaderDocument _ title annText) = do
  let showAllFurigana = constDyn True
  fontSizeDD <- dropdown 100 (constDyn fontSizeOptions) def
  rubySizeDD <- dropdown 120 (constDyn fontSizeOptions) def
  lineHeightDD <- dropdown 150 (constDyn lineHeightOptions) def
  let divAttr = (\s l -> "style" =: ("font-size: " <> tshow s <>"%;"
        <> "line-height: " <> tshow l <> "%;"))
        <$> (value fontSizeDD) <*> (value lineHeightDD)

  vIdEv <- elDynAttr "div" divAttr $ do
    el "h3" $ text title
    forM (V.toList annText) $ \annTextPara ->
      el "p" $ do
        let f (Left t) = never <$ text t
            f (Right (v, vId, vis)) = do
              rec
                let evVis = leftmost [True <$ eme, tagDyn showAllFurigana eml]
                visDyn <- holdDyn vis evVis
                (ek, eme, eml) <- vocabRuby (value rubySizeDD) visDyn v
              return $ (vId, vocabToText v) <$ ek
            onlyKana (Vocab ks) = (flip all) ks $ \case
              (Kana _) -> True
              _ -> False
            addSpace [] = []
            addSpace (l@(Left _):r@(Right _):rs) =
              l : (Left "　") : (addSpace (r:rs))
            addSpace (r1@(Right (v1,_,_)):r2@(Right _):rs)
              | onlyKana v1 = r1 : (Left "　") : (addSpace (r2:rs))
              | otherwise = r1:(addSpace (r2:rs))
            addSpace (r:rs) = r : (addSpace rs)

        leftmost <$> mapM f (annTextPara)

  divClass "" $ do
    let ev = leftmost vIdEv
    detailsEv <- getWebSocketResponse $ GetVocabDetails
      <$> fmap fst ev
    surfDyn <- holdDyn "" (fmap snd ev)
    showVocabDetailsWidget (attachDyn surfDyn detailsEv)
  return ()

showVocabDetailsWidget :: (AppMonad t m)
  => Event t (Text, [(Entry, Maybe SrsEntryId)])
  -> AppMonadT t m ()
showVocabDetailsWidget detailsEv = do
  let

    attrBack = ("class" =: "modal")
          <> ("style" =: "display: block;\
              \opacity: 0%; z-index: 1050;")
    attrFront = ("class" =: "nav navbar-fixed-bottom")
          <> ("style" =: "z-index: 1060;")

    wrapper :: (_) => m a -> m (Event t ())
    wrapper m = do
      (e1,_) <- elAttr' "div" attrBack $ return ()
      elAttr "div" attrFront $
        divClass "container-fluid" $
          elAttr "div" (("class" =: "panel panel-default")
            <> ("style" =: "max-height: 200px;\
                           \overflow-y: auto;")) $ do
            (e,_) <- elClass' "button" "close" $ text "Close"
            m
            return $ leftmost
              [domEvent Click e
              , domEvent Click e1]

    wd :: AppMonad t m
      => Maybe _
      -> AppMonadT t m (Event t ())
    wd (Just (s,es)) = wrapper
      (mapM_ (showEntry s) (orderEntries (fst) es))
    wd Nothing = return never

  rec
    let ev = leftmost [Just <$> detailsEv
             , Nothing <$ (switchPromptlyDyn closeEv)]
    closeEv <- widgetHold (return never)
      (wd <$> ev)

  return ()

showEntry surface (e, sId) = do
  divClass "" $ do
    elClass "span" "" $ do
      entryKanjiAndReading e
    addEditSrsEntryWidget (Right $ e ^. entryUniqueId) (Just surface) sId

  let
    showGlosses ms = mapM_ text $ intersperse ", " $
      map (\m -> T.unwords $ T.words m & _head  %~ capitalize)
      ms
    showInfo [] = return ()
    showInfo is = do
      mapM_ text $ ["("] ++ (intersperse ", " is) ++ [")"]
    showSense s = divClass "" $ do
      showPos $ s ^.. sensePartOfSpeech . traverse
      showInfo $ s ^.. senseInfo . traverse
      showGlosses $ take 5 $ s ^.. senseGlosses . traverse . glossDefinition

  divClass "" $ do
    mapM showSense $ take 3 $ e ^.. entrySenses . traverse

capitalize t
  | T.head t == ('-') = t
  | elem t ignoreList = t
  | otherwise = T.toTitle t
  where ignoreList = ["to"]

showPos ps = do
  elClass "span" "small" $ do
    mapM_ text $ p $ (intersperse ", ") psDesc
  where
    p [] = []
    p c = ["("] ++ c ++ [") "]
    psDesc = catMaybes $ map f ps
    f PosNoun = Just $ "Noun"
    f PosPronoun = Just $ "Pronoun"
    f (PosVerb _ _) = Just $ "Verb"
    f (PosAdverb _) = Just $ "Adv."
    f (PosAdjective _) = Just $ "Adj."
    f PosSuffix = Just $ "Suffix"
    f PosPrefix = Just $ "Prefix"
    f _ = Nothing

entryKanjiAndReading :: (_) => Entry -> m ()
entryKanjiAndReading e = do
  sequenceA_ (intersperse sep els)
  where
  els = map (renderElement (restrictedKanjiPhrases e)
    (e ^. entryReadingElements . to (NE.head) . readingPhrase))
    (orderElements e)
  sep = text ", "

restrictedKanjiPhrases :: Entry
  -> Map KanjiPhrase ReadingElement
restrictedKanjiPhrases e = Map.fromList $ concat $
  e ^.. entryReadingElements . traverse
    . to (\re -> re ^.. readingRestrictKanji . traverse
           . to (\kp -> (kp, re)))

-- Priority of entries
-- Entry with priority elements
-- Entry normal
-- Entry with Info elements
orderEntries :: (a -> Entry) -> [a] -> [a]
orderEntries g es = sortBy (comparing (f . g)) es
  where
    f e
      | any (not . null) $
        (ke ^.. traverse . kanjiPriority) ++
        (re ^.. traverse . readingPriority)
        = 1
      | any (not . null)
        (ke ^.. traverse . kanjiInfo) ||
        any (not . null)
        (re ^.. traverse . readingInfo)
        = 3
      | otherwise = 2
      where
        ke = e ^. entryKanjiElements
        re = e ^. entryReadingElements

-- Priority of elements
-- Kanji with priority
-- Reading with priority
-- Kanji with reading
-- Kanji With restricted reading
-- Reading
-- Kanji with Info
-- Reading with Info
orderElements
  :: Entry
  -> [(Either KanjiElement ReadingElement)]
orderElements e = sortBy (comparing f)
  ((e ^.. entryKanjiElements . traverse . to (Left)) ++
  readingWithoutRes)

  where
    f (Left ke)
      | (ke ^. kanjiPriority . to (not . null)) = 1
      | (ke ^. kanjiInfo . to (not . null)) = 6
      | Map.member (ke ^. kanjiPhrase)
        (restrictedKanjiPhrases e) = 4
      | otherwise = 3

    f (Right re)
      | (re ^. readingPriority . to (not . null)) = 2
      | (re ^. readingInfo . to (not . null)) = 7
      | otherwise = 5

    readingWithoutRes = map Right $
      filter (view $ readingRestrictKanji . to (null)) $
      (e ^.. entryReadingElements . traverse)

renderElement :: (_)
  => Map KanjiPhrase ReadingElement
  -> ReadingPhrase
  -> (Either KanjiElement ReadingElement)
  -> m ()
renderElement restMap defR (Left ke) = case v of
  (Right v) -> displayVocabT v
  (Left _) -> text $ unKanjiPhrase $ ke ^. kanjiPhrase
  where
    kp = (ke ^. kanjiPhrase)
    v = case Map.lookup kp restMap of
          (Just r) -> makeFurigana kp (r ^. readingPhrase)
          Nothing -> makeFurigana kp defR

renderElement _ _ (Right re) =
  text $ unReadingPhrase $ re ^. readingPhrase
