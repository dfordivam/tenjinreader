{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
module SrsWidget where

import FrontendCommon
import SpeechRecog
import ReviewState

import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Data.Map as Map
import NLP.Romkan (toHiragana)
import Data.List.NonEmpty (NonEmpty)

data SrsWidgetView =
  ShowStatsWindow | ShowReviewWindow | ShowBrowseSrsItemsWindow
  deriving (Eq)

srsWidget
  :: AppMonad t m
  => AppMonadT t m ()
srsWidget = divClass "" $ do
  let

  rec
    let
      visEv = leftmost [ev1,ev2,ev3]
    vis <- holdDyn ShowStatsWindow visEv

    ev1 <- handleVisibility ShowStatsWindow vis $
      showStats

    ev2 <- handleVisibility ShowBrowseSrsItemsWindow vis $
      browseSrsItemsWidget

    ev3 <- handleVisibility ShowReviewWindow vis $
      reviewWidget (Proxy :: Proxy RecogReview)
  return ()

showStats
  :: AppMonad t m
  => AppMonadT t m (Event t SrsWidgetView)
showStats = do
  ev <- getPostBuild
  s <- getWebSocketResponse (GetSrsStats () <$ ev)
  retEvDyn <- widgetHold (return never) (showStatsWidget <$> s)
  return $ switchPromptlyDyn $ retEvDyn

showStatsWidget
  :: (MonadWidget t m)
  => SrsStats -> m (Event t SrsWidgetView)
showStatsWidget s = do
  startReviewEv <- divClass "" $ do
    divClass "" $ do
      divClass "" $
        divClass "" $
          text $ tshow s
      divClass "" $ do
        divClass "" $
          divClass "" $
            text ""

        divClass "" $
          button "Start reviewing"

  --   statsCard "Reviews Today" (reviewsToday s)
  --   statsCard "Total Items" (totalItems s)
  --   statsCard "Total Reviews" (totalReviews s)
  --   statsCard "Average Success" (averageSuccess s)
  --   return ev

  -- divClass "" $ do
  --   progressStatsCard "Discovering" "D1" "D2"
  --     (discoveringCount s)
  --   progressStatsCard "Committing" "C1" "C2"
  --     (committingCount s)
  --   progressStatsCard "Bolstering" "B1" "B2"
  --     (bolsteringCount s)
  --   progressStatsCard "Assimilating" "A1" "A2"
  --     (assimilatingCount s)
  --   divClass "" $ do
  --     divClass "" $
  --       divClass "" $
  --         text $ tshow (setInStone s)
  --     divClass "" $
  --       divClass "" $
  --         text "Set in Stone"

  browseEv <- button "Browse Srs Items"
  return $ leftmost [ShowReviewWindow <$ startReviewEv
                    , ShowBrowseSrsItemsWindow <$ browseEv]

statsCard t val = divClass "" $ do
  divClass "" $
    divClass "" $
      text $ tshow val
  divClass "" $
    divClass "" $
      text t

progressStatsCard l l1 l2 (v1,v2) =
  divClass "" $ do
    divClass "" $
      divClass "" $
        text $ tshow (v1 + v2)
    divClass "" $
      divClass "" $
        text l
    divClass "" $ do
      divClass "" $ do
        divClass "" $
          divClass "" $ text l1
        divClass "" $
          divClass "" $ text $ tshow v1

      divClass "" $ do
        divClass "" $
          divClass "" $ text l2
        divClass "" $
          divClass "" $ text $ tshow v2

srsLevels = Map.fromList
  [ (LearningLvl, "Learning" :: Text)
  , (IntermediateLvl , "Intermediate")
  , (MatureLvl, "Mature")]

data BrowseSrsItemsOptions
  = BrowseSrsItemsDue
  | BrowseSrsItemsNew
  | BrowseSrsItemsSusp
  | BrowseSrsItemsOther
  deriving (Eq, Ord, Show)

browseOptions = Map.fromList
  [ (BrowseSrsItemsDue, "Due" :: Text)
  ,  (BrowseSrsItemsNew, "New")
  ,  (BrowseSrsItemsSusp, "Suspended")
  ,  (BrowseSrsItemsOther, "Others")]

revTypeSel = Map.fromList
  [ (ReviewTypeRecogReview, "Recognition" :: Text)
  , (ReviewTypeProdReview, "Production")]

getBrowseSrsItemsEv ::
     (MonadFix m, MonadHold t m, Reflex t)
  => Dropdown t BrowseSrsItemsOptions
  -> Dropdown t SrsItemLevel
  -> m (Dynamic t BrowseSrsItemsFilter)
getBrowseSrsItemsEv filt levels = do
  let f (This BrowseSrsItemsNew) _ = BrowseNewItems
      f (This b) BrowseNewItems = g b LearningLvl
      f (This b) (BrowseDueItems l)   = g b l
      f (This b) (BrowseSuspItems l)  = g b l
      f (This b) (BrowseOtherItems l) = g b l

      f (That _) BrowseNewItems = BrowseNewItems
      f (That l) (BrowseDueItems _) = BrowseDueItems l
      f (That l) (BrowseSuspItems _) = BrowseSuspItems l
      f (That l) (BrowseOtherItems _) = BrowseOtherItems l

      f (These b l) _ = g b l

      g BrowseSrsItemsDue l = BrowseDueItems l
      g BrowseSrsItemsNew _ = BrowseNewItems
      g BrowseSrsItemsSusp l = BrowseSuspItems l
      g BrowseSrsItemsOther l = BrowseOtherItems l

  foldDyn f (BrowseDueItems LearningLvl)
    (align (filt ^. dropdown_change) (levels ^. dropdown_change))
-- Fetch all srs items then apply the filter client side
-- fetch srs items for every change in filter
--
browseSrsItemsWidget
  :: forall t m . AppMonad t m
  => AppMonadT t m (Event t SrsWidgetView)
browseSrsItemsWidget = do
  -- Widget declarations
  let

    filterOptionsWidget =
      divClass "" $ do
        -- Selection buttons
        selectAllToggleCheckBox <- divClass "" $ do

          checkbox False def -- & setValue .~ allSelected

        filt <- dropdown (BrowseSrsItemsDue) (constDyn browseOptions) def
        levels <- dropdown (LearningLvl) (constDyn srsLevels) def
        revType <- dropdown (ReviewTypeRecogReview) (constDyn revTypeSel) def

        brwDyn <- getBrowseSrsItemsEv filt levels
        let filtOptsDyn = BrowseSrsItems <$> value revType <*> brwDyn
        return (filtOptsDyn, selectAllToggleCheckBox, value filt, value revType)

    checkBoxList selAllEv es =
      divClass "" $ do
        el "label" $ text "Select Items to do bulk edit"
        evs <- elAttr "div" (("class" =: "")
                             <> ("style" =: "height: 400px; overflow-y: scroll")) $
          forM es $ checkBoxListEl selAllEv

        let f (v, True) s = Set.insert v s
            f (v, False) s = Set.delete v s
        selList <- foldDyn f Set.empty (leftmost evs)

        return $ Set.toList <$> selList

    checkBoxListEl :: Event t Bool -> SrsItem
      -> AppMonadT t m (Event t (SrsEntryId, Bool))
    checkBoxListEl selAllEv (SrsItem i t) = divClass "" $ do
      let
      c1 <- do
        divClass "" $ do
          text $ (tshow t)
          ev <- button "edit"
          openEditSrsItemWidget $ i <$ ev
        divClass "" $
          checkbox False $ def & setValue .~ selAllEv
      return $ (,) i <$> updated (value c1)

  -- UI
  divClass "" $ do
    -- Filter Options
    (browseSrsFilterDyn, selectAllToggleCheckBox, filtOptsDyn, revTypeDyn) <-
      filterOptionsWidget

    rec
      let
        checkBoxSelAllEv = updated $
          value selectAllToggleCheckBox

      itemEv <- getWebSocketResponse $ leftmost
        [updated browseSrsFilterDyn
        , tagDyn browseSrsFilterDyn editDone]

      -- List and selection checkBox
      selList <- divClass "" $ do
        widgetHold (checkBoxList never [])
          (checkBoxList checkBoxSelAllEv <$> itemEv)

      -- Action buttons
      editDone <-
        bulkEditWidgetActionButtons filtOptsDyn revTypeDyn $ join selList
    return ()

  closeEv <- divClass "" $
    button "Close Widget"
  return $ ShowStatsWindow <$ closeEv

bulkEditWidgetActionButtons
  :: AppMonad t m
  => Dynamic t BrowseSrsItemsOptions
  -> Dynamic t ReviewType
  -> Dynamic t [SrsEntryId]
  -> AppMonadT t m (Event t ())
bulkEditWidgetActionButtons filtOptsDyn revTypeDyn selList = divClass "" $ do
  today <- liftIO $ utctDay <$> getCurrentTime

  let btn t active = do
        let attr True = ("type" =: "button") <> ("class" =: "btn btn-primary active")
            attr False = ("type" =: "button") <> ("class" =: "btn btn-primary disabled")
        (e, _) <- elDynAttr' "button" (attr <$> active) $ text t
        return $ domEvent Click e
      felem = flip elem

  el "table" $ el "tbody" $ do
    suspendEv <-
      el "td" $
      btn "Suspend" $ (felem [BrowseSrsItemsDue, BrowseSrsItemsOther]) <$> filtOptsDyn

    markDueEv <- el "td" $
      btn "Mark Due" $ (felem [BrowseSrsItemsSusp, BrowseSrsItemsOther]) <$> filtOptsDyn

    deleteEv <- el "td" $
      btn "Delete" (constDyn True)

    reviewDateChange <- el "td" $
      btn "Change Review Date" $ (felem [BrowseSrsItemsDue,
         BrowseSrsItemsSusp, BrowseSrsItemsOther]) <$> filtOptsDyn

    dateDyn <- el "td" $ datePicker today
    let bEditOp = leftmost
          [DeleteSrsItems <$ deleteEv
          , MarkDueSrsItems <$ markDueEv
          , SuspendSrsItems <$ suspendEv
          , ChangeSrsReviewData <$> tagPromptlyDyn dateDyn reviewDateChange]
    doUpdate <- getWebSocketResponse $
      (attachDynWith ($) (BulkEditSrsItems <$> revTypeDyn <*> selList) bEditOp)
    return $ fmapMaybe identity doUpdate

datePicker
  :: (MonadWidget t m)
  => Day -> m (Dynamic t Day)
datePicker today = divClass "" $ do
  let dayList = makeList [1..31]
      monthList = makeList [1..12]
      yearList = makeList [2000..2030]
      makeList x = constDyn $ Map.fromList $ (\x -> (x, tshow x)) <$> x
      (currentYear, currentMonth, currentDay)
        = toGregorian today
      mycol = divClass ""
        --elAttr "div" (("class" =: "column") <> ("style" =: "min-width: 2em;"))
  day <- mycol $ dropdown currentDay dayList $ def
  month <- mycol $ dropdown currentMonth monthList $ def
  year <- mycol $ dropdown currentYear yearList $ def
  return $ fromGregorian <$> value year <*> value month <*> value day

openEditSrsItemWidget
  :: (AppMonad t m)
  => Event t (SrsEntryId)
  -> AppMonadT t m ()
openEditSrsItemWidget ev = do
  srsItEv <- getWebSocketResponse $ GetSrsItem <$> ev

  let
      modalWidget :: (AppMonad t m) => Maybe SrsItemFull -> AppMonadT t m ()
      modalWidget (Just s) = do
        editWidget s
      modalWidget Nothing = do
        text $ "Some Error"


      f (Left (Vocab ((Kana k):_))) = k
      f (Right (Kanji k)) = k

      editWidget :: AppMonad t m => SrsItemFull -> AppMonadT t m ()
      editWidget s = do
        rec
          (sNew, saveEv) <- editWidgetView s ev
          ev <- getWebSocketResponse $ EditSrsItem <$> tagDyn sNew saveEv
        return ()

      editWidgetView
        :: MonadWidget t m
        => SrsItemFull
        -> Event t ()
        -> m (Dynamic t SrsItemFull, Event t ())
      editWidgetView s savedEv = divClass "" $ do
        elClass "h3" "" $ do
          text $ "Edit " <> (f $ srsItemFullVocabOrKanji s)

        reviewDateDyn <- divClass "" $ do
          reviewDataPicker (srsReviewDate s)

        (m,r) <- divClass "" $ do
          meaningTxtInp <- divClass "" $ divClass "" $ do
            divClass "" $ text "Meaning"
            textInput $ def &
              textInputConfig_initialValue .~ (srsMeanings s)

          readingTxtInp <- divClass "" $ divClass "" $ do
            divClass "" $ text "Reading"
            textInput $ def &
              textInputConfig_initialValue .~ (srsReadings s)

          return (meaningTxtInp, readingTxtInp)

        (mn,rn) <- divClass "" $ do
          meaningNotesTxtInp <- divClass "" $ do
            divClass "" $ text "Meaning Notes"
            divClass "" $ divClass "" $ do
              textArea $ def &
                textAreaConfig_initialValue .~
                  (maybe "" identity (srsMeaningNote s))

          readingNotesTxtInp <- divClass "" $ do
            divClass "" $ text "Reading Notes"
            divClass "" $ divClass "" $ do
              textArea $ def &
                textAreaConfig_initialValue .~
                  (maybe "" identity (srsReadingNote s))

          return (meaningNotesTxtInp, readingNotesTxtInp)

        tagsTxtInp <- divClass "" $ do
          divClass "" $ divClass "" $ do
            divClass "" $ text "Tags"
            textInput $ def &
              textInputConfig_initialValue .~
                (maybe "" identity (srsTags s))

        saveEv <- divClass "" $ do
          let savedIcon = elClass "i" "" $ return ()
          ev <- button "Save"
          widgetHold (return ()) (savedIcon <$ savedEv)
          return ev

        let ret = SrsItemFull (srsItemFullId s) (srsItemFullVocabOrKanji s)
                    <$> reviewDateDyn <*> (value m) <*> (value r)
                    <*> pure (srsCurrentGrade s) <*> g mn <*> g rn
                    <*> g tagsTxtInp
            g v = gg <$> value v
            gg t
              | T.null t = Nothing
              | otherwise = Just t

        return (ret, saveEv)

      reviewDataPicker :: (MonadWidget t m) =>
        Maybe Day -> m (Dynamic t (Maybe Day))
      reviewDataPicker inp = do
        today <- liftIO $ utctDay <$> getCurrentTime

        let
          addDateW = do
            button "Add Next Review Date"

          selectDateW = do
            divClass "" $ do
              newDateDyn <- divClass "" $ datePicker defDate
              removeDate <- divClass "" $
                button "Remove Review Date"
              return (removeDate, newDateDyn)

          defDate = maybe today identity inp

        rec
          vDyn <- holdDyn (isJust inp) (leftmost [False <$ r, True <$ a])
          a <- handleVisibility False vDyn addDateW
          (r,d) <- handleVisibility True vDyn selectDateW
        let
            f :: Reflex t => (Dynamic t a) -> Bool -> Dynamic t (Maybe a)
            f d True = Just <$> d
            f _ _ = pure Nothing
        return $ join $ f d <$> vDyn

  void $ widgetHold (return ()) (modalWidget <$> srsItEv)

reviewWidget
  :: forall t m rt proxy . (AppMonad t m, SrsReviewType rt)
  => proxy rt
  -> AppMonadT t m (Event t SrsWidgetView)
reviewWidget p = do
  let
    rt = reviewType p

  let attr = ("class" =: "")
             <> ("style" =: "height: 50rem;")

  ev <- getPostBuild
  initEv <- getWebSocketResponse $ GetNextReviewItems rt [] <$ ev

  rec
    -- Input Events
    -- 1. Initial review items
    -- 2. Review result
    -- 3. Fetch new items from server
    -- 4. Undo event
    -- 5. refresh (if initEv was Nothing)

    let
      addItemEv = AddItemsEv <$> leftmost [initEv, fetchMoreReviewsResp] :: Event t (ReviewStateEvent rt)

    -- Output Events
    -- 1. Show review item
    -- 2. Fetch new reviews
    -- 3. Send the result back
    widgetStateDyn <- foldDyn (flip (foldl widgetStateFun))
      (SrsWidgetState Map.empty Nothing def)
      (mergeList [addItemEv, reviewResultEv])

    let sendResultEv = fmapMaybe sendResultEvFun (updated sendResultDyn)
        addResEv = fmapMaybe (_resultQueue) (updated widgetStateDyn)
        fetchMoreReviews = GetNextReviewItems rt <$> ffilter ((< 5) . length)
          ((Map.keys . _reviewQueue) <$> (updated widgetStateDyn))

    dEv <- debounce 120 fetchMoreReviews
    fetchMoreReviewsResp <- getWebSocketResponse dEv
    sendResultDyn <- foldDyn (flip handlerSendResultEv) ReadyToSend
      (align addResEv (() <$ sendResResp))
    sendResResp <- getWebSocketResponse sendResultEv

  -- toss <- liftIO $ randomIO
  --   rt = if toss then RecogReadingReview else RecogMeaningReview
    (closeEv, reviewResultEv) <- elAttr "div" attr $ divClass "" $ do
      closeEv <- divClass "" $
        button "Close Review"

      -- Show refresh button if no review available
      reviewResultEv <- reviewWidgetView
        (_reviewStats <$> widgetStateDyn)
        =<< getRevItemDyn widgetStateDyn never

      return (closeEv, reviewResultEv)

  return $ ShowStatsWindow <$ closeEv

-- Start initEv (show review if available)
-- review done ev, fetch new event after update of dyn
getRevItemDyn :: _
  => Dynamic t (SrsWidgetState rt)
  -> Event t ()
  -> m (Dynamic t (Maybe (ReviewItem, ReviewResult rt)))
getRevItemDyn widgetStateDyn ev = do
  return $ constDyn Nothing

reviewWidgetView
  :: (AppMonad t m, SrsReviewType rt)
  => Dynamic t SrsReviewStats
  -> Dynamic t (Maybe (ReviewItem, ReviewResult rt))
  -> AppMonadT t m (Event t (ReviewStateEvent rt))
reviewWidgetView statsDyn dyn2 = do
  let
    statsRowAttr = ("class" =: "")
              <> ("style" =: "height: 15rem;")
    statsTextAttr = ("style" =: "font-size: large;")

    showStats = do
      let colour c = ("style" =: ("color: " <> c <>";" ))
      elAttr "span" (colour "black") $
        dynText $ (tshow . _srsReviewStats_pendingCount) <$> statsDyn
      elAttr "span" (colour "green") $
        dynText $ (tshow . _srsReviewStats_correctCount) <$> statsDyn
      elAttr "span" (colour "red") $
        dynText $ (tshow . _srsReviewStats_incorrectCount) <$> statsDyn

  divClass "" $ elAttr "div" statsRowAttr $ do
    elAttr "span" statsTextAttr $
      showStats

  let kanjiRowAttr = ("class" =: "")
         <> ("style" =: "height: 10rem;")
      kanjiTextAttr = ("style" =: "font-size: 5rem;")

  elAttr "div" kanjiRowAttr $
    elAttr "span" kanjiTextAttr $ do
      let
      dynText $ showReviewItemField <$> (preview (_Just . _1 . reviewItemField) <$> dyn2)

  dr <- dyn $ ffor dyn2 $ \case
    (Nothing) -> return never
    (Just v) -> inputFieldWidget v

  -- drSpeech <- case rt of
  --   RecogReadingReview ->
  --     ((\b -> DoReviewEv (i, RecogReadingReview, b)) <$>) <$>
  --       speechRecogWidget (fst r)
  --   _ -> return never

  -- FIXME Show notes only after answering
  --       <> ("style" =: "height: 10rem;")
  -- let notesRowAttr = ("class" =: "")
  --     notesTextAttr = ("style" =: "font-size: large;")
  --     notes = case rt of
  --       (Left (_, MeaningNotes mn)) -> mn
  --       (Right (_, ReadingNotes rn)) -> rn

  -- divClass "" $ elAttr "div" notesRowAttr $ do
  --   elClass "h3" "" $ text "Notes:"
  --   elAttr "p" notesTextAttr $ text notes

  -- Footer
  -- evB <- divClass "" $ divClass "" $ do
  --   ev1 <- divClass "" $
  --     button "Undo"
  --   ev2 <- divClass "" $
  --     button "Add Meaning"
  --   ev3 <- divClass "" $
  --     button "Edit"
  --   openEditSrsItemWidget (i <$ ev3)
  --   return $ leftmost
  --     [UndoReview <$ ev1]
    -- TODO AddAnswer support
      -- , AddAnswer i rt <$> tagDyn inpTextValue ev2]
  evReview <- switchPromptly never dr
  return evReview
    --leftmost [evB, dr, drSpeech]

showReviewItemField t = "review item here"

inputFieldWidget
  :: _
  => (ReviewItem, ReviewResult rt)
  -> m (Event t (ReviewStateEvent rt))
inputFieldWidget (ri, rt) = do
  let
    style = "text-align: center;" -- <> color
    -- color = if rt == RecogMeaningReview
    --   then "background-color: palegreen;"
    --   else "background-color: aliceblue;"
    inputField ev = do
      let tiAttr = def
            & textInputConfig_setValue .~ ev
            & textInputConfig_attributes
            .~ constDyn ("style" =: style)
      divClass "" $
        divClass "" $ do
          textInput tiAttr

    showResult res = do
      divClass "" $ text $ "Result: " <> res

  rec
    inpField <- inputField inpTxtEv
    (dr, inpTxtEv, resEv) <-
      reviewInputFieldHandler inpField rt ri
  widgetHold (return ()) (showResult <$> resEv)
  return (dr)

reviewInputFieldHandler
 :: (MonadFix m,
     MonadHold t m,
     Reflex t,
     SrsReviewType rt)
 => TextInput t
 -> ReviewResult rt
 -> ReviewItem
 -> m (Event t (ReviewStateEvent rt), Event t Text, Event t Text)
reviewInputFieldHandler ti rt ri@(ReviewItem i k m r) = do
  let enterPress = ffilter (==13) (ti ^. textInput_keypress) -- 13 -> Enter
      correct = checkAnswer n <$> value ti
      n = getAnswer ri rt
      h _ NewReview = ShowAnswer
      h _ ShowAnswer = NextReview
      h _ _ = NewReview
  dyn <- foldDyn h NewReview enterPress
  let
    sendResult = ffilter (== NextReview) (tagDyn dyn enterPress)
    dr = (\b -> DoReviewEv (i, rt, b)) <$> tagDyn correct sendResult

    hiragana = never
    -- case rt of
    --   RecogMeaningReview -> never
    --   -- TODO Implement proper kana writing support
    --   RecogReadingReview -> toHiragana <$> (ti ^. textInput_input)
    correctEv = tagDyn correct enterPress
  -- the dr event will fire after the correctEv (on second enter press)
  let resEv b = (if b
        then "Correct : "
        else "Incorrect : ") <> ans
      ans = show n
  return (dr, hiragana, resEv <$> correctEv)

-- TODO For meaning reviews allow minor mistakes
checkAnswer :: (Either (NonEmpty Meaning) (NonEmpty Reading))
            -> Text
            -> Bool
checkAnswer (Left m) t = elem t answers
  where answers = map unMeaning m
checkAnswer (Right r) t = elem t answers
  where answers = map unReading r

data AnswerBoxState = NewReview | ShowAnswer | NextReview
  deriving (Eq)
