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

    filterResultEv <- getWebSocketResponse filterEvWithPostBuild

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
    kanjiSelectionEv <-
      divClass "col-sm-4" $ do
        kanjiListWidget kanjiListEv

  return ()
-- Widget to show kanjifilter

kanjiFilterWidget
  :: AppMonad t m
  => Event t [RadicalId]
  -> AppMonadT t m (Event t KanjiFilter)
kanjiFilterWidget validRadicalsEv = do

  -- NW 1
  sentenceTextArea <- divClass "" $ textArea def

  -- NW 2
  (readingTextInput, readingSelectionDropDown, meaningTextInput)
    <- divClass "" $ do
    t <- textInput def
    d <- dropdown
      KunYomi
      (constDyn ((KunYomi =: "Kunyomi") <> (OnYomi =: "Onyomi ")))
      def
    m <- textInput def
    return (t,d,m)

  let filterDyn = AdditionalFilter <$> (value readingTextInput)
                    <*> (value readingSelectionDropDown)
                    <*> (value meaningTextInput)

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
    let
      renderMatrix = do
        divClass "row" $ mapM showRadical (Map.toList radicalTable)

      showRadical :: (RadicalId, RadicalDetails) -> m (Event t RadicalId)
      showRadical (i,(RadicalDetails r _)) = do
        let valid =
              Set.member i <$> validRadicals
            sel =
              -- pure False
              Set.member i <$> selectedRadicals
            -- (Valid, Selected)
            cl (_,True) = "green"
            cl (True, False) = ""
            cl (False,False) = ""
            attr = (\c -> ("class" =: c)) <$>
                     (cl <$> zipDyn valid sel)

        (e,_) <- elAttr' "div" ("class" =: "col-sm-1") $
          elDynAttr "button" attr $ text r
        let ev = attachDynWithMaybe f valid
                   (domEvent Click e)
            f True _ = Just ()
            f _ _ = Nothing
        return (i <$ ev)

    ev <- renderMatrix

    let
      h :: RadicalId -> Set RadicalId -> Set RadicalId
      h i s = if Set.member i s then Set.delete i s else Set.insert i s
    selectedRadicals <- foldDyn h Set.empty (leftmost ev)

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
              (\r1 -> "Rank: " <> show r1) (unRank <$> r)
            el "td" $ text $ T.intercalate "," $ map unMeaning m
          return (i <$ domEvent Click e)

    liWrap i = do
      ev <- dyn $ listItem <$> i
      switchPromptly never ev

    fun (This l) _ = l
    fun (That ln) l = l ++ ln
    fun (These l _) _ = l

  -- NW 1
  elClass "table" "table" $ el "tbody" $  do
    rec
      lmEv <- getWebSocketResponse $ LoadMoreKanjiResults <$ ev
      dyn <- foldDyn fun [] (align listEv lmEv)
      -- NW 1.1
      d <- simpleList dyn liWrap
      -- NW 1.2
      ev <- el "td" $ button "Load More"
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
    listItem (v,s) = el "tr" $ do
      el "td" $ do
        displayVocabT $ v ^. vocab

      el "td" $ elClass "table" "table" $ el "tbody" $ do
        el "tr" $ do
          el "td" $ text $ T.intercalate "," $ map unMeaning
            $ v ^. vocabMeanings
          el "td" $ srsEntryWidget (Right $ v ^. vocabId) s

        el "tr" $ do
          el "td" $ textMay (tshow <$> (unRank <$> v ^. vocabFreqRank))

    liWrap i = do
      dyn $ listItem <$> i

    fun (This l) _ = l
    fun (That ln) l = l ++ ln
    fun (These l _) _ = l

  -- NW 1
  elClass "table" "table" $ el "tbody" $  do
    rec
      lmEv <- getWebSocketResponse $ req <$ ev
      dyn <- foldDyn fun [] (align listEv lmEv)
      -- NW 1.1
      simpleList dyn liWrap
      -- NW 1.2
      ev <- el "td" $ button "Load More"
    return ()

-- Controls to add/edit related srs items
srsEntryWidget :: AppMonad t m
  => (Either KanjiId VocabId)
  -> Maybe SrsEntryId
  -> AppMonadT t m ()
srsEntryWidget i s = do
  let
    widget s = case s of
      (Just sId) -> do
        ev <- button "Edit Srs Item"
        return never

      (Nothing) -> do
        ev <- button "Add to Srs"
        getWebSocketResponse $ QuickAddSrsItem i <$ ev
  rec
    sDyn <- holdDyn s resp
    resp <- switchPromptly never
      =<< (dyn $ widget <$> sDyn)

  return ()

displayVocabT :: DomBuilder t m => Vocab -> m ()
displayVocabT (Vocab ks) = do
  let
    f (Kana k) = text k
    f (KanjiWithReading (Kanji k) r)
      = el "ruby" $ do
          text k
          el "rt" $ text r
  mapM_ f ks

textMay (Just v) = text v
textMay Nothing = text ""

vocabSearchWidget
  :: AppMonad t m
  => AppMonadT t m ()
vocabSearchWidget = divClass "" $ divClass "" $ do

  vocabResEv <- divClass "" $ do
    reading <- textInput def
    meaning <- textInput def

    let vsDyn = VocabSearch <$> (AdditionalFilter
                  <$> value reading
                  <*> pure KunYomi
                  <*> value meaning)
    getWebSocketResponse (updated vsDyn)

  vocabListWindow LoadMoreVocabSearchResult vocabResEv
