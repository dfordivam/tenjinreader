{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
module SrsWidget where

import FrontendCommon
import SpeechRecog

import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Data.Map as Map
import System.Random
import NLP.Romkan (toHiragana)

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
      reviewWidget
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
    ev <- divClass "" $ do
      divClass "" $
        divClass "" $
          text $ tshow (pendingReviewCount s)
      divClass "" $ do
        divClass "" $
          divClass "" $
            text ""

        divClass "" $
          button "Start reviewing"

    statsCard "Reviews Today" (reviewsToday s)
    statsCard "Total Items" (totalItems s)
    statsCard "Total Reviews" (totalReviews s)
    statsCard "Average Success" (averageSuccess s)
    return ev

  divClass "" $ do
    progressStatsCard "Discovering" "D1" "D2"
      (discoveringCount s)
    progressStatsCard "Committing" "C1" "C2"
      (committingCount s)
    progressStatsCard "Bolstering" "B1" "B2"
      (bolsteringCount s)
    progressStatsCard "Assimilating" "A1" "A2"
      (assimilatingCount s)
    divClass "" $ do
      divClass "" $
        divClass "" $
          text $ tshow (setInStone s)
      divClass "" $
        divClass "" $
          text "Set in Stone"

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

-- TODO Fix this srsLevels
srsLevels :: Map SrsLevel Text
srsLevels = Map.fromList $ map (\g -> (SrsLevel g, (tshow g))) [0..8]

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

        levels
          <- divClass "" $
             divClass "" $ el "label" $
               dropdown (SrsLevel 0) (constDyn srsLevels) def

         -- Kanji/Vocab
         -- Pending review

        return (BrowseSrsItems <$> ((:[]) <$> _dropdown_change levels)
               , selectAllToggleCheckBox)

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
      -> AppMonadT t m (Event t (SrsItemId, Bool))
    checkBoxListEl selAllEv (SrsItem i v sus pend) = divClass "" $ do
      let
        f (Left (Vocab ((Kana k):_))) = k
        f (Right (Kanji k)) = k
        -- c = if sus
        --   then divClass "grey"
        --   else if pend
        --     then divClass "violet"
        --     else divClass "black"
      c1 <- do
        divClass "" $ do
          ev <- button "edit"
          openEditSrsItemWidget $ i <$ ev
        divClass "" $
          checkbox False $ def & setValue .~ selAllEv
      return $ (,) i <$> updated (value c1)

  -- UI
  divClass "" $ do
    -- Filter Options
    (browseSrsFilterEv, selectAllToggleCheckBox) <-
      filterOptionsWidget

    filteredList <- getWebSocketResponse browseSrsFilterEv
    browseSrsFilterDyn <- holdDyn (BrowseSrsItems []) browseSrsFilterEv
    rec
      let
        itemEv = leftmost [filteredList, afterEditList]

        checkBoxSelAllEv = updated $
          value selectAllToggleCheckBox

      -- List and selection checkBox
      selList <- divClass "" $ do
        widgetHold (checkBoxList never [])
          (checkBoxList checkBoxSelAllEv <$> itemEv)

      -- Action buttons
      afterEditList <-
        bulkEditWidgetActionButtons browseSrsFilterDyn $ join selList
    return ()

  closeEv <- divClass "" $
    button "Close Widget"
  return $ ShowStatsWindow <$ closeEv

bulkEditWidgetActionButtons
  :: AppMonad t m
  => Dynamic t BrowseSrsItems
  -> Dynamic t [SrsItemId]
  -> AppMonadT t m (Event t [SrsItem])
bulkEditWidgetActionButtons filtOptsDyn selList = divClass "" $ do
  currentTime <- liftIO getCurrentTime

  suspendEv <- divClass "" $
    button "Suspend"

  resumeEv <- divClass "" $
    button "Resume"

  deleteEv <- divClass "" $
    button "Delete"

  changeLvlSel <- divClass "" $
    dropdown (SrsLevel 0) (constDyn srsLevels) $ def
  changeLvlEv <- divClass "" $
    button "Change Level"

  reviewDateChange <- divClass "" $
    button "Change Review Date"

  dateDyn <- divClass "" $ datePicker currentTime
  let bEditOp = leftmost
        [DeleteSrsItems <$ deleteEv
        , SuspendSrsItems <$ suspendEv
        , ResumeSrsItems <$ resumeEv
        , ChangeSrsLevel <$> tagPromptlyDyn (value changeLvlSel) changeLvlEv
        , ChangeSrsReviewData <$> tagPromptlyDyn dateDyn reviewDateChange]
  getWebSocketResponse $ (\((s,b),e) -> BulkEditSrsItems s e b) <$>
    (attachDyn ((,) <$> selList <*> filtOptsDyn) bEditOp)

datePicker
  :: (MonadWidget t m)
  => UTCTime -> m (Dynamic t UTCTime)
datePicker defTime = divClass "" $ do
  let dayList = makeList [1..31]
      monthList = makeList [1..12]
      yearList = makeList [2000..2030]
      makeList x = constDyn $ Map.fromList $ (\x -> (x, tshow x)) <$> x
      (currentYear, currentMonth, currentDay)
        = (\(UTCTime d _) -> toGregorian d) defTime
      mycol = divClass ""
        --elAttr "div" (("class" =: "column") <> ("style" =: "min-width: 2em;"))
  day <- mycol $ dropdown currentDay dayList $ def
  month <- mycol $ dropdown currentMonth monthList $ def
  year <- mycol $ dropdown currentYear yearList $ def
  return $ UTCTime <$> (fromGregorian <$> value year <*> value month <*> value day) <*> pure 1

openEditSrsItemWidget
  :: (AppMonad t m)
  => Event t (SrsItemId)
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
        Maybe UTCTime -> m (Dynamic t (Maybe UTCTime))
      reviewDataPicker inp = do
        currentTime <- liftIO getCurrentTime

        let
          addDateW = do
            button "Add Next Review Date"

          selectDateW = do
            divClass "" $ do
              newDateDyn <- divClass "" $ datePicker defDate
              removeDate <- divClass "" $
                button "Remove Review Date"
              return (removeDate, newDateDyn)

          defDate = maybe currentTime identity inp

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

data ReviewStatus =
  NotAnswered | AnsweredWrong
  | AnsweredWithMistake | AnsweredWithoutMistake
  deriving (Eq)

type ReviewState = Map ReviewType ReviewStatus
type Result = (SrsItemId,Bool)

data SrsWidgetState = SrsWidgetState
  { reviewQueue :: Map SrsItemId (ReviewItem, ReviewState)
  , resultQueue :: Maybe Result
  , reviewStats :: SrsReviewStats
  }

data ReviewStateEvent
  = DoReviewEv DoReview
  | AddItemsEv [ReviewItem]
  | UndoReview

type DoReview = (SrsItemId, ReviewType, Bool)

widgetStateFun :: SrsWidgetState -> ReviewStateEvent -> SrsWidgetState
widgetStateFun st (AddItemsEv ri) = st -- Map.merge const reviewQueue (Map.fromList ri)
widgetStateFun st (DoReviewEv (i,rt,b)) = if b
  then st
  else st
widgetStateFun st (UndoReview) = st

getReviewFun :: SrsWidgetState -> () -> Maybe (ReviewItem, SrsReviewStats)
getReviewFun st _ = (,) <$> Map.minView (reviewQueue st) ^? _Just . _1 . _1
  <*> pure (reviewStats st)

-- Result Synchronise with server
-- TODO implement feature to re-send result if no response recieved
data ResultsSyncState =
  ReadyToSend
  | WaitingForResp [Result] [Result]

sendResultEvFun (ReadyToSend) = Nothing
sendResultEvFun (WaitingForResp r _) = Just (AlsoDoReview r)

handlerSendResultEv :: ResultsSyncState -> (These Result ()) -> ResultsSyncState
handlerSendResultEv ReadyToSend (This r) = WaitingForResp [r] []
handlerSendResultEv ReadyToSend (That _) = error "Got result resp, when not expecting"
handlerSendResultEv ReadyToSend (These r _) = error "Got result resp, when not expecting"
handlerSendResultEv (WaitingForResp r rs) (This rn) = WaitingForResp r (rs ++ [rn])
handlerSendResultEv (WaitingForResp _ rs) (That _) = WaitingForResp rs []
handlerSendResultEv (WaitingForResp _ []) (That _) = ReadyToSend
handlerSendResultEv (WaitingForResp _ (rs)) (These rn _) = WaitingForResp (rs ++ [rn]) []
handlerSendResultEv (WaitingForResp _ []) (These rn _) = WaitingForResp [rn] []


reviewWidget
  :: (AppMonad t m)
  => AppMonadT t m (Event t SrsWidgetView)
reviewWidget = do
  let

  let attr = ("class" =: "")
             <> ("style" =: "height: 50rem;")

  ev <- getPostBuild
  initEv <- getWebSocketResponse $ GetNextReviewItems <$ ev

  rec
    -- Input Events
    -- 1. Initial review items
    -- 2. Review result
    -- 3. Fetch new items from server
    -- 4. Undo event
    -- 5. refresh (if initEv was Nothing)
    let itemsEv = leftmost [initEv, sendResResp]
        addItemEv = fmapMaybe (fmap AddItemsEv) itemsEv

    -- Output Events
    -- 1. Show review item
    -- 2. Fetch new reviews
    -- 3. Send the result back
    widgetStateDyn <- foldDyn (flip (foldl widgetStateFun))
      (SrsWidgetState Map.empty Nothing def)
      (mergeList [addItemEv, reviewResultEv])

    let sendResultEv = fmapMaybe sendResultEvFun (updated sendResultDyn)
        addResEv = fmapMaybe (resultQueue) (updated widgetStateDyn)
    sendResultDyn <- foldDyn (flip handlerSendResultEv) ReadyToSend
      (align addResEv (() <$ sendResResp))
    sendResResp <- getWebSocketResponse sendResultEv

    (closeEv, reviewResultEv) <- elAttr "div" attr $ divClass "" $ do
      closeEv <- divClass "" $
        button "Close Review"

      -- Start initEv (show review if available)
      -- review done ev, fetch new event after update of dyn
      let reviewItemEv = attachDynWithMaybe getReviewFun
            widgetStateDyn $ leftmost
              [void reviewResultEv, void initEv]

      -- Show refresh button if no review available
      drDyn <- widgetHold (return never) $
        reviewWidgetView <$> reviewItemEv

      let reviewResultEv = switchPromptlyDyn drDyn

      return (closeEv, reviewResultEv)

  return $ ShowStatsWindow <$ closeEv

reviewWidgetView
  :: AppMonad t m
  => (ReviewItem, SrsReviewStats)
  -> AppMonadT t m (Event t ReviewStateEvent)
reviewWidgetView (ri@(ReviewItem i k m r), s) = do
  toss <- liftIO $ randomIO
  let
    rt = if toss then ReadingReview else MeaningReview
    statsRowAttr = ("class" =: "")
              <> ("style" =: "height: 15rem;")
    statsTextAttr = ("style" =: "font-size: large;")

    showStats s = do
      let colour c = ("style" =: ("color: " <> c <>";" ))
      elAttr "span" (colour "black") $
        text $ tshow (_srsReviewStats_pendingCount s)  <> " "
      elAttr "span" (colour "green") $
        text $ tshow (_srsReviewStats_correctCount s) <> " "
      elAttr "span" (colour "red") $
        text $ tshow (_srsReviewStats_incorrectCount s)

  divClass "" $ elAttr "div" statsRowAttr $ do
    elAttr "span" statsTextAttr $
      showStats s

  let kanjiRowAttr = ("class" =: "")
         <> ("style" =: "height: 10rem;")
      kanjiTextAttr = ("style" =: "font-size: 5rem;")

  elAttr "div" kanjiRowAttr $
    elAttr "span" kanjiTextAttr $ do
      let
        f (Left (Vocab ((Kana k):_))) = k
        f (Right (Kanji k)) = k
      text $ f k

  (dr,inpTextValue) <- inputFieldWidget ri rt

  drSpeech <- case rt of
    ReadingReview ->
      ((\b -> DoReviewEv (i, ReadingReview, b)) <$>) <$>
        speechRecogWidget (fst r)
    _ -> return never

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

  evB <- divClass "" $ divClass "" $ do
    ev1 <- divClass "" $
      button "Undo"
    ev2 <- divClass "" $
      button "Add Meaning"
    ev3 <- divClass "" $
      button "Edit"
    openEditSrsItemWidget (i <$ ev3)
    return $ leftmost
      [UndoReview <$ ev1]
      -- , AddAnswer i rt <$> tagDyn inpTextValue ev2]
  return $ leftmost [evB,DoReviewEv <$> dr, drSpeech]

inputFieldWidget
  :: _
  => ReviewItem
  -> ReviewType
  -> m (Event t DoReview, Dynamic t Text)
inputFieldWidget ri rt = do
  let
    style = "text-align: center;" <> color
    color = if rt == MeaningReview
      then "background-color: palegreen;"
      else "background-color: aliceblue;"
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
  return (dr, value inpField)

reviewInputFieldHandler
 :: (MonadFix m,
     MonadHold t m,
     Reflex t)
 => TextInput t
 -> ReviewType
 -> ReviewItem
 -> m (Event t DoReview, Event t Text, Event t Text)
reviewInputFieldHandler ti rt (ReviewItem i k m r) = do
  let enterPress = ffilter (==13) (ti ^. textInput_keypress) -- 13 -> Enter
      correct = checkAnswer n <$> value ti
      n = case rt of
        MeaningReview -> Left m
        ReadingReview -> Right r
      h _ NewReview = ShowAnswer
      h _ ShowAnswer = NextReview
      h _ _ = NewReview
  dyn <- foldDyn h NewReview enterPress
  let
    sendResult = ffilter (== NextReview) (tagDyn dyn enterPress)
    dr = (\b -> (i, rt, b)) <$> tagDyn correct sendResult

    hiragana = case rt of
      MeaningReview -> never
      ReadingReview -> toHiragana <$> (ti ^. textInput_input)
    correctEv = tagDyn correct enterPress
  -- the dr event will fire after the correctEv (on second enter press)
  let resEv b = (if b
        then "Correct : "
        else "Incorrect : ") <> ans
      ans = case rt of
        MeaningReview -> unMeaning $ fst m
        ReadingReview -> unReading $ fst r
  return (dr, hiragana, resEv <$> correctEv)

-- TODO
-- For meaning reviews allow minor mistakes
checkAnswer :: (Either (Meaning, MeaningNotes) (Reading, ReadingNotes))
            -> Text
            -> Bool
checkAnswer (Left (Meaning m,_)) t = elem t answers
  where answers = T.splitOn "," m
checkAnswer (Right (Reading r,_)) t = elem t answers
  where answers = T.splitOn "," r

data AnswerBoxState = NewReview | ShowAnswer | NextReview
  deriving (Eq)
