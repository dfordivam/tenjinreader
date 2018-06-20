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
kanjiBrowseWidget = divClass "" $ do

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

      validRadicals = attachWith f (current filterDyn) validRadicalsEv

    filterDyn <- holdDyn def filterEv

    -- NW 1.1
    filterEv <- divClass "section" $ do
      rec
        let visEv = leftmost [KanjiFilterVis <$ closeEv
                             , KanjiDetailPageVis <$ kanjiDetailsEv]
        vis <- holdDyn KanjiFilterVis visEv

        -- Show either the filter options or the kanji details page
        -- NW 1.1.1
        (filterEv, kanjiSelectionEv) <- handleVisibility KanjiFilterVis vis $ do
          e1 <- kanjiFilterWidget validRadicals
          e2 <- kanjiListWidget kanjiListEv
          return (e1,e2)

        maybeKanjiDetailsEv <- getWebSocketResponse
          ((flip GetKanjiDetails) def <$> kanjiSelectionEv)

        -- NW 1.1.2
        closeEv <- handleVisibility KanjiDetailPageVis vis $ do
          l <- btn "" "Close Details Page"
          return (l)

        let kanjiDetailsEv = fmapMaybe identity maybeKanjiDetailsEv
        -- NW 1.1.3
        handleVisibility KanjiDetailPageVis vis $
          kanjiDetailsWidget kanjiDetailsEv

      return filterEv

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
                        <> ("class" =: "textarea")
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
                        <> ("class" =: "input")
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
  evDelayed <- delay 0.1 evValid
  validRadicals <- holdDyn (Map.keysSet radicalTable) (Set.fromList <$> evDelayed)

  rec
    -- disableAll <- holdDyn False $ leftmost
    --   [True <$ (leftmost ev)
    --   , False <$ updated validRadicals]
    let
      disableAll = constDyn False
      renderMatrix = do
        divClass "" $ do
          (r,d) <- divClass "field is-grouped" $ do
            r <- btn "" "Reset"
            d <- toggle False
              =<< (divClass "" $ btn "" "Show/Hide Radicals")
            return (r,d)
          ev1 <- handleVisibility True d $
            mapM showRadical (Map.toList radicalTable)
          return (ev1, r)

      showRadical :: (RadicalId, RadicalDetails) -> m (Event t RadicalId)
      showRadical (i,(RadicalDetails r _)) = do
        let valid =
              Set.member i <$> validRadicals
            sel =
              -- pure False
              Set.member i <$> selectedRadicals
            -- (Valid, Selected)
            cl (_,True) = (" is-success ", True)
            cl (True, False) = ("", True)
            cl (False,False) = (" is-warning ", False)
            attr = (\(c,b) -> ("class" =: (c <> "button is-small" )) <> (if b then (Map.empty) else ("disabled" =: "")))
                     <$> (cl <$> zipDyn valid sel)

            spanAttr = ("class" =: "is-size-6")
              -- <> ("style" =: "color: black;")

        (e,_) <- elDynAttr' "button" attr $
          elAttr "span" spanAttr $ text r
        let ev1 = attachWithMaybe f
                   (current $ zipDyn valid disableAll)
                   (domEvent Click e)
            f (_,True) _ = Nothing
            f (True,_) _ = Just ()
            f _ _ = Nothing
        return (i <$ ev1)

    (ev, resetEv) <- renderMatrix

    let
      h :: RadicalId -> Set RadicalId -> Set RadicalId
      h i s = if Set.member i s then Set.delete i s else Set.insert i s
      foldF = foldDyn h Set.empty (leftmost ev)
    selectedRadicals <- join <$> widgetHold foldF (foldF <$ resetEv)
  return $ Set.toList <$> selectedRadicals


kanjiListWidget
  :: (AppMonad t m)
  => Event t [(KanjiId, Kanji, Maybe Rank, [Meaning])]
  -> AppMonadT t m (Event t KanjiId)
kanjiListWidget listEv = do
  let
    listItem (i, k, r, m) = do
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
    dynV <- foldDyn fun [] (align listEv lmEv)
    d <- elClass "table" "table table-striped" $ do
      el "thead" $ do
        el "tr" $ do
          el "th" $ text "Kanji"
          el "th" $ text "Rank"
          el "th" $ text "Meanings"
      el "tbody" $  do
        -- NW 1.1
        simpleList dynV liWrap
    ev <- do
      btnLoading "btn-block btn-primary" "Load More" lmEv
  return $ switch . current $ leftmost <$> d

kanjiDetailsWidget
  :: AppMonad t m
  => Event t KanjiSelectionDetails -> AppMonadT t m ()
kanjiDetailsWidget ev = do
  let f1 (KanjiSelectionDetails k _) = k
  let f2 (KanjiSelectionDetails _ v) = v
  _ <- widgetHold (return()) (kanjiDetailWindow <$> (f1 <$> ev))
  vocabListWindow LoadMoreKanjiVocab (f2 <$> ev)

kanjiDetailWindow :: AppMonad t m
  => (KanjiDetails, VocabSrsState, [Text])
  -> AppMonadT t m ()
kanjiDetailWindow (k,vSt,rads) = divClass "box" $ do
  let
    maybeLabel _ Nothing = return ()
    maybeLabel l (Just v) = elClass "span" "label label-default" $
      text l >> text ": " >> text (tshow v)

  divClass "tile is-ancestor" $ do
    divClass "tile level is-mobile" $ do
      divClass "level-left" $ divClass "level-item" $
        elClass "span" "tag is-size-3 is-primary" $
          text (unKanji $ k ^. kanjiCharacter)

      divClass "level-right" $ divClass "level-item" $ elClass "span" "" $ do
        text "Radicals: "
        forM rads $ \t -> elClass "span" "tag is-medium is-info" $ text t

    divClass "tile" $ do
      divClass "tile is-size-5" $
        text $ T.intercalate ", " $
          map capitalize (map unMeaning $ k ^. kanjiMeanings)

      divClass "tile" $ do
        maybeLabel "Rank" (unRank <$> k ^. kanjiMostUsedRank)
        maybeLabel "JLPT" (unJlptLevel <$> k ^. kanjiJlptLevel)
        maybeLabel "WaniKani Lvl" (unWkLevel <$> k ^. kanjiWkLevel)

      addEditSrsEntryWidget (Left $ _kanjiId k) Nothing vSt

vocabListWindow
  :: (AppMonad t m
     , WebSocketMessage AppRequest req
     , ResponseT AppRequest req ~ [(Entry, VocabSrsState)])
  => req -> Event t VocabList -> AppMonadT t m ()
vocabListWindow req listEv = do
  let
    fun (This l) _ = l
    fun (That ln) l = l ++ ln
    fun (These l _) _ = l

  -- NW 1
  rec
    lmEv <- getWebSocketResponse $ req <$ ev
    lsDyn <- foldDyn fun [] (align listEv lmEv)
    _ <- divClass "" $  do
      -- NW 1.1
      dyn $ (mapM (showEntry Nothing)) <$> lsDyn
      -- NW 1.2
    ev <- do
      btnLoading "btn-block btn-primary" "Load More" lmEv
  return ()

vocabSearchWidget
  :: AppMonad t m
  => AppMonadT t m ()
vocabSearchWidget = divClass "" $ divClass "" $ do

  vocabResEv <- divClass "" $ divClass "field has-addons" $ do
    let
      tiAttr = constDyn $ (("style" =: "")
                          <> ("class" =: "input")
                          <> ("placeholder" =: "Search by meaning or reading"))
    ti <- elClass "p" "control" $ textInput $ def
      & textInputConfig_attributes .~ tiAttr

    rec
      searchEv <- elClass "p" "control" $
        btnLoading "" "Search" res

      filt <- elClass "p" "control" $ elClass "span" "select" $ do

        dropdown Nothing
          (constDyn ((Nothing =: "All")
          <> ((Just PosNoun) =: "Noun Only")
          <> ((Just (PosVerb (Regular Ichidan) NotSpecified))
              =: "Verb Only")
          <> ((Just (PosAdjective NaAdjective)) =: "Adj or Adv only")))
          $ def
            & dropdownConfig_attributes .~ (constDyn ("class" =: ""))

      let vsDyn = VocabSearch <$> (value ti)
                    <*> (value filt)
      res <- getWebSocketResponse (tagDyn vsDyn searchEv)
    return res

  divClass "panel-body" $ do
    vocabListWindow LoadMoreVocabSearchResult vocabResEv
