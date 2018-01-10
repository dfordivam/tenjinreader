{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module KanjiBrowser where

import FrontendCommon

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Set as Set

data VisibleWidget = KanjiFilterVis | KanjiDetailPageVis
  deriving (Eq)

kanjiBrowseWidget
  :: AppMonad t m
  => AppMonadT t m ()
kanjiBrowseWidget = divClass "row" $ do

  ev <- getPostBuild

  rec
    let
      filterEvWithPostBuild = leftmost [def <$ ev,
                                        filterEv]

    searchEv <- debounce 1 filterEvWithPostBuild
    filterResultEv <- getWebSocketResponse searchEv

    let
      (kanjiListEv, validRadicalsEv)
        = splitE $ (\(KanjiFilterResult a b) -> (a,b)) <$> filterResultEv

      -- For empty Filter all radicals are valid
      f (KanjiFilter "" (AdditionalFilter "" _ "") []) _ = Map.keys radicalTable
      f _ r = r

      validRadicals = attachPromptlyDynWith f filterDyn validRadicalsEv

    filterDyn <- holdDyn def filterEv

    -- NW 1.1
    filterEv <- divClass "col-sm-8" $ do
      rec
        let visEv = leftmost [KanjiFilterVis <$ closeEv
                             , KanjiDetailPageVis <$ kanjiDetailsEv]
        vis <- holdDyn KanjiFilterVis visEv

        -- Show either the filter options or the kanji details page
        -- NW 1.1.1
        filterEv <- handleVisibility KanjiFilterVis vis $
          kanjiFilterWidget validRadicals

        maybeKanjiDetailsEv <- getWebSocketResponse
          ((flip GetKanjiDetails) def <$> kanjiSelectionEv)

        showWSProcessing kanjiSelectionEv maybeKanjiDetailsEv

        -- NW 1.1.2
        closeEv <- handleVisibility KanjiDetailPageVis vis $ do
          l <- linkClass "Close Details Page" ""
          return (_link_clicked l)

        let kanjiDetailsEv = fmapMaybe identity maybeKanjiDetailsEv
        -- NW 1.1.3
        handleVisibility KanjiDetailPageVis vis $
          kanjiDetailsWidget kanjiDetailsEv

      return filterEv

    -- NW 1.2

    let listAttr = ("class" =: "col-sm-4")
          <> ("style" =: "height: 1000px;\
                         \overflow-y: auto;")
    kanjiSelectionEv <-
      elAttr "div" listAttr $ do
        showWSProcessing filterEv kanjiListEv
        kanjiListWidget kanjiListEv

  return ()
-- Widget to show kanjifilter

kanjiFilterWidget
  :: AppMonad t m
  => Event t [RadicalId]
  -> AppMonadT t m (Event t KanjiFilter)
kanjiFilterWidget validRadicalsEv = do

  -- NW 1
  let
    taAttr = constDyn $ (("style" =: "width: 100%;")
                        <> ("rows" =: "4")
                        <> ("class" =: "form-control")
                        <> ("placeholder" =: "Enter text to search all Kanjis in it"))
  sentenceTextArea <- divClass "" $ textArea $ def
    & textAreaConfig_attributes .~ taAttr

  -- TODO Add IME and kana input separate field
  -- NW 2
  -- (readingTextInput, readingSelectionDropDown, meaningTextInput)
  --   <- divClass "" $ do
  --   t <- textInput def
  --   d <- dropdown
  --     KunYomi
  --     (constDyn ((KunYomi =: "Kunyomi") <> (OnYomi =: "Onyomi ")))
  --     def
  --   m <- textInput def
  --   return (t,d,m)
  let
    tiAttr = constDyn $ (("style" =: "width: 100%;")
                        <> ("class" =: "form-control")
                        <> ("placeholder" =: "Search by meaning or reading"))
  t <- textInput $ def
    & textInputConfig_attributes .~ tiAttr

  let filterDyn = AdditionalFilter <$> (value t)
                    <*> pure KunYomi
                    <*> (pure "")

  -- NW 3
  selectedRadicals <- radicalMatrix validRadicalsEv
  let
    kanjiFilter = KanjiFilter <$> (value sentenceTextArea)
          <*> filterDyn
          <*> selectedRadicals

  return $ updated kanjiFilter

radicalMatrix
  :: forall t m . ( DomBuilder t m
     , MonadIO (Performable m)
     , TriggerEvent t m
     , MonadHold t m
     , PostBuild t m
     , PerformEvent t m
     , MonadFix m
     )
  => Event t [RadicalId] -> m (Dynamic t [RadicalId])
radicalMatrix evValid = do

  --- XXX Fix why delay here?
  evDelayed <- delay 1 evValid
  validRadicals <- holdDyn (Map.keysSet radicalTable) (Set.fromList <$> evDelayed)

  rec
    -- disableAll <- holdDyn False $ leftmost
    --   [True <$ (leftmost ev)
    --   , False <$ updated validRadicals]
    let
      disableAll = constDyn False
      renderMatrix = do
        divClass "row well-lg hidden-xs" $ do
          r <- btn "btn-block btn-default btn-xs" "Reset"
          ev <- mapM showRadical (Map.toList radicalTable)
          return (ev, r)

      showRadical :: (RadicalId, RadicalDetails) -> m (Event t RadicalId)
      showRadical (i,(RadicalDetails r _)) = do
        let valid =
              Set.member i <$> validRadicals
            sel =
              -- pure False
              Set.member i <$> selectedRadicals
            -- (Valid, Selected)
            cl (_,True) = " btn-success "
            cl (True, False) = " btn-info "
            cl (False,False) = " btn-info disabled "
            attr = (\c -> ("class" =: (c <> "btn btn-xs" )))
                     <$> (cl <$> zipDyn valid sel)

            spanAttr = ("class" =: "badge")
              <> ("style" =: "color: black;")

        (e,_) <- elDynAttr' "button" attr $
          elAttr "span" spanAttr $ text r
        let ev = attachDynWithMaybe f
                   (zipDyn valid disableAll)
                   (domEvent Click e)
            f (_,True) _ = Nothing
            f (True,_) _ = Just ()
            f _ _ = Nothing
        return (i <$ ev)

    (ev, resetEv) <- renderMatrix

    let
      h :: RadicalId -> Set RadicalId -> Set RadicalId
      h i s = if Set.member i s then Set.delete i s else Set.insert i s
      foldF = foldDyn h Set.empty (leftmost ev)
    selectedRadicals <- join <$> widgetHold foldF (foldF <$ resetEv)
  return $ Set.toList <$> selectedRadicals


-- kanjiListWidget
--   :: (DomBuilder t m, MonadHold t m)
--   => Event t KanjiList -> m (Event t KanjiId)
kanjiListWidget listEv = do
  let
    listItem itm@(i, k, r, m) = do
          (e, _) <- el' "tr" $ do
            el "td" $ text $ (unKanji k)
            el "td" $ text $ maybe ""
              (\r1 -> show r1) (unRank <$> r)
            el "td" $ text $ T.intercalate "," $ map unMeaning m
          return (i <$ domEvent Click e)

    liWrap i = do
      ev <- dyn $ listItem <$> i
      switchPromptly never ev

    fun (This l) _ = l
    fun (That ln) l = l ++ ln
    fun (These l _) _ = l

  -- NW 1
  rec
    lmEv <- getWebSocketResponse $ LoadMoreKanjiResults <$ ev
    dyn <- foldDyn fun [] (align listEv lmEv)
    d <- elClass "table" "table table-striped" $ do
      el "thead" $ do
        el "tr" $ do
          el "th" $ text "Kanji"
          el "th" $ text "Rank"
          el "th" $ text "Meanings"
      el "tbody" $  do
        -- NW 1.1
        simpleList dyn liWrap
    ev <- do
      showWSProcessing ev lmEv
      btn "btn-block btn-primary" "Load More"
  return $ switchPromptlyDyn $ leftmost <$> d

kanjiDetailsWidget
  :: AppMonad t m
  => Event t KanjiSelectionDetails -> AppMonadT t m ()
kanjiDetailsWidget ev = do
  let f1 (KanjiSelectionDetails k s v) = k
  let f2 (KanjiSelectionDetails k s v) = v
  widgetHold (return()) (kanjiDetailWindow <$> (f1 <$> ev))
  vocabListWindow LoadMoreKanjiVocab (f2 <$> ev)

kanjiDetailWindow :: (DomBuilder t m) => KanjiDetails -> m ()
kanjiDetailWindow k = do
  elClass "table" "table" $ el "tbody" $ do
    el "tr" $ do
      el "td" $ do
        elClass "i" "" $
          text (unKanji $ k ^. kanjiCharacter)
      el "td" $ do
        text "Radicals here"

    el "tr" $ do
      el "td" $ do
        text $ T.intercalate "," $ map unMeaning $
          k ^. kanjiMeanings

      el "td" $ do
        textMay (tshow <$> (unRank <$> k ^. kanjiMostUsedRank))

      -- divClass "" $ do
      --   textMay (unOnYomiT <$> on)
      --   textMay (unKunYomiT <$> ku)

vocabListWindow
  :: (AppMonad t m
     , WebSocketMessage AppRequest req
     , ResponseT AppRequest req ~ [(VocabDetails, Maybe SrsEntryId)])
  => req -> Event t VocabList -> AppMonadT t m ()
vocabListWindow req listEv = do
  let
    listItem (v,s) = divClass "row well-sm" $ do
      divClass "col-md-3" $ do
        displayVocabT $ v ^. vocab

      divClass "col-md-5" $
        text $ T.intercalate ", " $ map unMeaning
          $ v ^. vocabMeanings
      divClass "col-md-2" $
        addEditSrsEntryWidget (Right $ v ^. vocabId)
          Nothing s
      divClass "col-md-2" $ do
        openEv <- btn "btn-xs btn-primary" "Sentences"
        openSentenceWidget (vocabToText $ v ^. vocab
                           , map unMeaning $ v ^. vocabMeanings)
          ((Left $ v ^. vocabId) <$ openEv)

    liWrap i = do
      dyn $ listItem <$> i

    fun (This l) _ = l
    fun (That ln) l = l ++ ln
    fun (These l _) _ = l

  -- NW 1
  rec
    lmEv <- getWebSocketResponse $ req <$ ev
    dyn <- foldDyn fun [] (align listEv lmEv)
    divClass "" $  do
      -- NW 1.1
      simpleList dyn liWrap
      -- NW 1.2
    ev <- do
      showWSProcessing ev lmEv
      btn "btn-block btn-primary" "Load More"
  return ()

textMay (Just v) = text v
textMay Nothing = text ""

vocabSearchWidget
  :: AppMonad t m
  => AppMonadT t m ()
vocabSearchWidget = divClass "" $ divClass "panel panel-default" $ do

  (vocabResEv,searchEv) <- divClass "panel-heading clearfix" $ divClass "" $ do
    let
      tiAttr = constDyn $ (("style" =: "")
                          <> ("class" =: "col-sm-10")
                          <> ("placeholder" =: "Search by meaning or reading"))
    ti <- textInput $ def
      & textInputConfig_attributes .~ tiAttr

    filt <- divClass "col-sm-2" $ do

      dropdown Nothing
        (constDyn ((Nothing =: "All")
        <> ((Just PosNoun) =: "Noun Only")
        <> ((Just (PosVerb (Regular Ichidan) NotSpecified))
            =: "Verb Only")
        <> ((Just (PosAdjective NaAdjective)) =: "Adj or Adv only")))
        $ def
          & dropdownConfig_attributes .~ (constDyn ("class" =: "form-control input-sm"))

    let vsDyn = VocabSearch <$> (value ti)
                  <*> (value filt)
    searchEv <- debounce 1 (updated vsDyn)
    res <- getWebSocketResponse searchEv
    return (res,searchEv)

  divClass "panel-body" $ do
    showWSProcessing searchEv vocabResEv
    vocabListWindow LoadMoreVocabSearchResult vocabResEv
