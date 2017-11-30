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

getBrowseSrsItemsEv ::
     (MonadFix m, MonadHold t m, Reflex t)
  => Dropdown t BrowseSrsItemsOptions
  -> Dropdown t SrsItemLevel
  -> m (Dynamic t BrowseSrsItems)
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

        brwDyn <- getBrowseSrsItemsEv filt levels
        return (brwDyn, selectAllToggleCheckBox, value filt)

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
          text t
          ev <- button "edit"
          openEditSrsItemWidget $ i <$ ev
        divClass "" $
          checkbox False $ def & setValue .~ selAllEv
      return $ (,) i <$> updated (value c1)

  -- UI
  divClass "" $ do
    -- Filter Options
    (browseSrsFilterDyn, selectAllToggleCheckBox, filtOptsDyn) <-
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
        bulkEditWidgetActionButtons filtOptsDyn $ join selList
    return ()

  closeEv <- divClass "" $
    button "Close Widget"
  return $ ShowStatsWindow <$ closeEv

bulkEditWidgetActionButtons
  :: AppMonad t m
  => Dynamic t BrowseSrsItemsOptions
  -> Dynamic t [SrsEntryId]
  -> AppMonadT t m (Event t ())
bulkEditWidgetActionButtons filtOptsDyn selList = divClass "" $ do
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
    getWebSocketResponse $ (uncurry BulkEditSrsItems) <$>
      (attachDyn selList bEditOp)

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

data ReviewStatus =
  NotAnswered | AnsweredWrong
  | AnsweredWithMistake | AnsweredWithoutMistake
  deriving (Eq)

type ReviewState = Map ReviewType ReviewStatus
type Result = (SrsEntryId,Bool)

data SrsWidgetState = SrsWidgetState
  { reviewQueue :: Map SrsEntryId (ReviewItem, ReviewState)
  , resultQueue :: Maybe Result
  , reviewStats :: SrsReviewStats
  }

data ReviewStateEvent
  = DoReviewEv (SrsEntryId, ReviewType, Bool)
  | AddItemsEv [ReviewItem]
  | UndoReview

-- Just True -> No Mistake
-- Just False -> Did Mistake
-- Nothing -> Not Complete
isComplete :: ReviewState -> Maybe Bool
isComplete rSt =
  if all done reviews
    then Just $ not (any wrong reviews)
    else Nothing
  where done AnsweredWithMistake = True
        done AnsweredWithoutMistake = True
        done _ = False
        wrong AnsweredWithMistake = True
        wrong _ = False
        reviews = map snd $ Map.toList rSt


widgetStateFun :: SrsWidgetState -> ReviewStateEvent -> SrsWidgetState
widgetStateFun st (AddItemsEv ri) =
  st { reviewQueue = Map.union (reviewQueue st)
         (Map.fromList $ map (\r@(ReviewItem i _ _ _) -> (i,(r, defState))) ri)
      , resultQueue = Nothing
      }
  where defState = Map.fromList [(ReadingReview, NotAnswered), (MeaningReview, NotAnswered)]

widgetStateFun st (DoReviewEv (i,rt,b)) = if b
  then
    let
      stNew = (Map.adjust fun rt) <$> stOld
      done = join $ isComplete <$> stNew
      fun NotAnswered = AnsweredWithoutMistake
      fun AnsweredWrong = AnsweredWithMistake
      fun _ = error "Already answered"
      upFun (r,_) = case done of
        (Just _) -> Nothing
        Nothing -> (,) <$> pure r <*> stNew
    in st
      { reviewQueue = Map.update upFun i (reviewQueue st)
      , resultQueue = (,) <$> pure i <*> done
      }

  else
    let
      stNew = (Map.adjust fun rt) <$> stOld
      fun NotAnswered = AnsweredWrong
      fun AnsweredWrong = AnsweredWrong
      fun _ = error "Already answered"
    in st
      { reviewQueue = Map.update (\(r,_) -> (,) <$> pure r <*> stNew) i (reviewQueue st)
      , resultQueue = Nothing
      }

  where
    stOld = snd <$> Map.lookup i (reviewQueue st)

widgetStateFun st (UndoReview) = st


getReviewFun :: SrsWidgetState -> () -> Maybe (ReviewItem, SrsReviewStats, ReviewType)
getReviewFun st _ = (\a b c -> (a,b,c)) <$> rItem ^? _Just . _1
  <*> pure (reviewStats st) <*> revPending
  where
    rItem = Map.minView (reviewQueue st) ^? _Just . _1
    revPending = fst <$> (join $ headMay <$> ((\m -> filter (f . snd) (Map.assocs m)) <$>
      rItem ^? _Just . _2))
    f NotAnswered = True
    f AnsweredWrong = True
    f _ = False

-- Result Synchronise with server
-- TODO implement feature to re-send result if no response recieved
data ResultsSyncState =
  ReadyToSend
  | WaitingForResp [Result] [Result]

sendResultEvFun (ReadyToSend) = Nothing
sendResultEvFun (WaitingForResp r _) = Just (DoReview r)

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
  initEv <- getWebSocketResponse $ GetNextReviewItems [] <$ ev

  rec
    -- Input Events
    -- 1. Initial review items
    -- 2. Review result
    -- 3. Fetch new items from server
    -- 4. Undo event
    -- 5. refresh (if initEv was Nothing)

    let
      addItemEv = AddItemsEv <$> leftmost [initEv, fetchMoreReviewsResp]

    -- Output Events
    -- 1. Show review item
    -- 2. Fetch new reviews
    -- 3. Send the result back
    widgetStateDyn <- foldDyn (flip (foldl widgetStateFun))
      (SrsWidgetState Map.empty Nothing def)
      (mergeList [addItemEv, reviewResultEv])

    let sendResultEv = fmapMaybe sendResultEvFun (updated sendResultDyn)
        addResEv = fmapMaybe (resultQueue) (updated widgetStateDyn)
        fetchMoreReviews = GetNextReviewItems <$> ffilter ((< 5) . length)
          ((Map.keys . reviewQueue) <$> (updated widgetStateDyn))

    fetchMoreReviewsResp <- getWebSocketResponse fetchMoreReviews
    sendResultDyn <- foldDyn (flip handlerSendResultEv) ReadyToSend
      (align addResEv (() <$ sendResResp))
    sendResResp <- getWebSocketResponse sendResultEv

  -- toss <- liftIO $ randomIO
  --   rt = if toss then ReadingReview else MeaningReview
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
  => (ReviewItem, SrsReviewStats, ReviewType)
  -> AppMonadT t m (Event t ReviewStateEvent)
reviewWidgetView (ri@(ReviewItem i k m r), s, rt) = do
  let
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
    -- TODO AddAnswer support
      -- , AddAnswer i rt <$> tagDyn inpTextValue ev2]
  return $ leftmost [evB, dr, drSpeech]

inputFieldWidget
  :: _
  => ReviewItem
  -> ReviewType
  -> m (Event t ReviewStateEvent, Dynamic t Text)
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
 -> m (Event t ReviewStateEvent, Event t Text, Event t Text)
reviewInputFieldHandler ti rt (ReviewItem i k m r) = do
  let enterPress = ffilter (==13) (ti ^. textInput_keypress) -- 13 -> Enter
      correct = checkAnswer n <$> value ti
      n = case rt of
        MeaningReview -> Left $ fst m
        ReadingReview -> Right $ fst r
      h _ NewReview = ShowAnswer
      h _ ShowAnswer = NextReview
      h _ _ = NewReview
  dyn <- foldDyn h NewReview enterPress
  let
    sendResult = ffilter (== NextReview) (tagDyn dyn enterPress)
    dr = (\b -> DoReviewEv (i, rt, b)) <$> tagDyn correct sendResult

    hiragana = case rt of
      MeaningReview -> never
      -- TODO Implement proper kana writing support
      ReadingReview -> toHiragana <$> (ti ^. textInput_input)
    correctEv = tagDyn correct enterPress
  -- the dr event will fire after the correctEv (on second enter press)
  let resEv b = (if b
        then "Correct : "
        else "Incorrect : ") <> ans
      ans = mconcat $ intersperse ", "  $ case rt of
        MeaningReview -> map unMeaning $ fst m
        ReadingReview -> map unReading $ fst r
  return (dr, hiragana, resEv <$> correctEv)

-- TODO For meaning reviews allow minor mistakes
checkAnswer :: (Either [Meaning] [Reading])
            -> Text
            -> Bool
checkAnswer (Left m) t = elem t answers
  where answers = map unMeaning m
checkAnswer (Right r) t = elem t answers
  where answers = map unReading r

data AnswerBoxState = NewReview | ShowAnswer | NextReview
  deriving (Eq)
