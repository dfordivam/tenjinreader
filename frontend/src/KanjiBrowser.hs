{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
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
kanjiBrowseWidget = divClass "" $ divClass "" $ do

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

    filterEv <- divClass "" $ do
      rec
        let visEv = leftmost [KanjiFilterVis <$ closeEv
                             , KanjiDetailPageVis <$ kanjiDetailsEv]
        vis <- holdDyn KanjiFilterVis visEv

        -- Show either the filter options or the kanji details page
        filterEv <- handleVisibility KanjiFilterVis vis $
          kanjiFilterWidget validRadicals

        maybeKanjiDetailsEv <- getWebSocketResponse
          ((flip GetKanjiDetails) def <$> kanjiSelectionEv)

        closeEv <- handleVisibility KanjiDetailPageVis vis $ do
          l <- linkClass "Close Details Page" ""
          return (_link_clicked l)

        let kanjiDetailsEv = fmapMaybe identity maybeKanjiDetailsEv
        handleVisibility KanjiDetailPageVis vis $
          kanjiDetailsWidget kanjiDetailsEv

      return filterEv

    kanjiSelectionEv <-
      divClass "" $ do
        kanjiListWidget kanjiListEv

  return ()
-- Widget to show kanjifilter

kanjiFilterWidget
  :: AppMonad t m
  => Event t [RadicalId]
  -> AppMonadT t m (Event t KanjiFilter)
kanjiFilterWidget validRadicalsEv = do
  sentenceTextArea <- divClass "" $ textArea def

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
        divClass "" $ mapM showRadical (Map.toList radicalTable)

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

        (e,_) <- elAttr' "div" ("class" =: "") $
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


kanjiListWidget
  :: (DomBuilder t m, MonadHold t m)
  => Event t KanjiList -> m (Event t KanjiId)
kanjiListWidget listEv = do
  let kanjiTable itms = do
        el "table" $ do
          let listItem itm@(i, k, r, m) = do
                (e, _) <- el' "tr" $ do
                  el "td" $ text $ (unKanji k)
                  el "td" $ text $ maybe ""
                    (\r1 -> "Rank: " <> show r1) (unRank <$> r)
                  el "td" $ text $ T.intercalate "," $ map unMeaning m
                return (i <$ domEvent Click e)
          evs <- mapM listItem itms
          return $ leftmost evs
  d <- widgetHold (return never) (kanjiTable <$> listEv)
  let e = switchPromptlyDyn d
  return e
  -- show list

kanjiDetailsWidget
  :: (DomBuilder t m, MonadHold t m)
  => Event t KanjiSelectionDetails -> m ()
kanjiDetailsWidget ev = do
  let f (KanjiSelectionDetails k v) = do
        kanjiDetailWindow k
        vocabListWindow v
        return ()
  void $ widgetHold (return ()) (f <$> ev)

kanjiDetailWindow :: (DomBuilder t m) => KanjiDetails -> m ()
kanjiDetailWindow k = do
  divClass "" $ do
    divClass "" $ do
      divClass "" $ do
        elClass "i" "" $
          text (unKanji $ k ^. kanjiCharacter)
      divClass "" $ do
        text "Radicals here"

    divClass "" $ do
      divClass "" $ do
        text $ T.intercalate "," $ map unMeaning $
          k ^. kanjiMeanings

      divClass "" $ do
        textMay (tshow <$> (unRank <$> k ^. kanjiMostUsedRank))

      -- divClass "" $ do
      --   textMay (unOnYomiT <$> on)
      --   textMay (unKunYomiT <$> ku)

vocabListWindow :: DomBuilder t m => [VocabDetails] -> m ()
vocabListWindow vs = do
  let
    dispVocab v = divClass "row" $ do
      divClass "" $ do
        displayVocabT $ v ^. vocab

      divClass "" $ do
        divClass "" $ do
          text $ T.intercalate "," $ map unMeaning
            $ v ^. vocabMeanings

        divClass "" $ do
          textMay (tshow <$> (unRank <$> v ^. vocabFreqRank))

        divClass "" $ do
          textMay (tshow <$> (unWikiRank <$> v ^. vocabWikiRank))
          textMay (tshow <$> (unWkLevel <$> v ^. vocabWkLevel))

  divClass "" $ do
    forM_ vs dispVocab

displayVocabT :: DomBuilder t m => Vocab -> m ()
displayVocabT (Vocab ks) = do
  let
    f k = case k of
      (KanjiWithReading k f) -> divClass "" $ do
        divClass "" $ text f
        divClass "" $ text $ unKanji k
      (Kana t) -> divClass "" $ do
        divClass "" $ text ""
        divClass "" $ text t
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

  void $ widgetHold (return ()) (vocabListWindow <$> vocabResEv)
