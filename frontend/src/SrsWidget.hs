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
import qualified Data.List.NonEmpty as NE
import NLP.Romkan (toHiragana)
import Data.List.NonEmpty (NonEmpty)
import System.Random
import qualified GHCJS.DOM.HTMLElement as DOM

data SrsWidgetView =
  ShowStatsWindow | ShowReviewWindow ReviewType | ShowBrowseSrsItemsWindow
  deriving (Eq)

srsWidget
  :: AppMonad t m
  => AppMonadT t m ()
srsWidget = divClass "" $ do
  ev <- getPostBuild

  let widgetReDraw w redraw = do
        evDyn <- widgetHold (w)
          (w <$ redraw)
        return $ switchPromptlyDyn evDyn
  rec
    let
      visEv = leftmost [ev1,ev2,ev3,ev4, ShowStatsWindow <$ ev]
      refreshEv = (() <$ visEv)
    vis <- holdDyn ShowStatsWindow visEv

    ev1 <- handleVisibility ShowStatsWindow vis $
      showStats refreshEv

    (ev2, editDone) <- handleVisibility ShowBrowseSrsItemsWindow vis $
      browseSrsItemsWidget

    ev3 <- handleVisibility (ShowReviewWindow ReviewTypeRecogReview) vis $
      widgetReDraw (reviewWidget (Proxy :: Proxy RecogReview) refreshEv) editDone

    ev4 <- handleVisibility (ShowReviewWindow ReviewTypeProdReview) vis $
      widgetReDraw (reviewWidget (Proxy :: Proxy ProdReview) refreshEv) editDone
  return ()

showStats
  :: AppMonad t m
  => Event t ()
  -> AppMonadT t m (Event t SrsWidgetView)
showStats refreshEv = do
  s <- getWebSocketResponse (GetSrsStats () <$ refreshEv)
  showWSProcessing refreshEv s
  retEvDyn <- widgetHold (return never) (showStatsWidget <$> s)
  return $ switchPromptlyDyn $ retEvDyn

showStatsWidget
  :: (MonadWidget t m)
  => (SrsStats, SrsStats) -> m (Event t SrsWidgetView)
showStatsWidget (recog, prod) = do
  let
    w lbl rs = divClass "panel panel-default" $ do
      ev <- divClass "panel-heading" $ divClass "row" $ do
        elClass "h4" "col-sm-3" $ text lbl
        divClass "col-sm-4" $
          btn "btn-lg btn-success" "Start Review"

      divClass "panel-body" $ divClass "row" $ do
        divClass "col-sm-1" $ text "Pending:"
        divClass "col-sm-2" $ text $ tshow (reviewsToday rs)
        divClass "col-sm-1" $ text "Total Reviews:"
        divClass "col-sm-2" $ text $ tshow (totalReviews rs)
        divClass "col-sm-1" $ text "Average Success"
        divClass "col-sm-2" $ text $ tshow (averageSuccess rs) <> "%"

      return ev

  ev1 <- w "Recognition Review" recog
  ev2 <- w "Production Review" prod
  browseEv <- btn "btn-primary" "Browse Srs Items"
  return $ leftmost
    [ShowReviewWindow ReviewTypeRecogReview <$ ev1
    , ShowReviewWindow ReviewTypeProdReview <$ ev2
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
  => AppMonadT t m (Event t SrsWidgetView, Event t ())
browseSrsItemsWidget = do
  -- Widget declarations
  let
    ddConf :: _
    ddConf = def & dropdownConfig_attributes .~ (constDyn ddAttr)
    ddAttr = ("class" =: "form-control input-sm")

    filterOptionsWidget =
      divClass "panel-heading" $ divClass "form-inline" $ divClass "form-group" $ do
        -- Selection buttons
        selectAllToggleCheckBox <- divClass "col-sm-1" $ do

          checkbox False def -- & setValue .~ allSelected

        filt <- dropdown (BrowseSrsItemsDue) (constDyn browseOptions) ddConf
        levels <- dropdown (LearningLvl) (constDyn srsLevels) ddConf
        revType <- dropdown (ReviewTypeRecogReview) (constDyn revTypeSel) ddConf

        brwDyn <- getBrowseSrsItemsEv filt levels
        let filtOptsDyn = BrowseSrsItems <$> value revType <*> brwDyn
        return (filtOptsDyn, selectAllToggleCheckBox, value filt, value revType)

    checkBoxList selAllEv es =
      divClass "" $ do
        -- el "label" $ text "Select Items to do bulk edit"
        dyns <- elAttr "div" (("class" =: "")
                <> ("style" =: "height: 400px; overflow-y: auto")) $
          elClass "table" "table table-striped" $ el "tbody" $
            forM es $ checkBoxListEl selAllEv

        let f (v, True) = Just v
            f (v, False) = Nothing
            ds = distributeListOverDynPure dyns

        return $ (catMaybes . (map f)) <$> ds

    checkBoxListEl :: Event t Bool -> SrsItem
      -> AppMonadT t m (Dynamic t (SrsEntryId , Bool))
    checkBoxListEl selAllEv (SrsItem i t) = el "tr" $ do
      c1 <- elClass "td" "col-sm-1" $
        checkbox False $ def & setValue .~ selAllEv
      elClass "td" "el-sm-4" $
        text $ fold $ NE.intersperse ", " $ t
      ev <- elClass "td" "el-sm-2" $
        btn "btn-sm btn-primary" "edit"
      openEditSrsItemWidget $ i <$ ev
      return $ (,) i <$> (value c1)

  -- UI
  (closeEv, editDone) <- divClass "panel panel-default" $ do
    (e,_) <- elClass' "button" "close" $ text "Close"

    -- Filter Options
    (browseSrsFilterDyn, selectAllToggleCheckBox, filtOptsDyn, revTypeDyn) <-
      filterOptionsWidget

    evPB <- getPostBuild
    rec
      let
        checkBoxSelAllEv = updated $
          value selectAllToggleCheckBox

        reqEv = leftmost
          [updated browseSrsFilterDyn
          , tagDyn browseSrsFilterDyn editDone
          , tagDyn browseSrsFilterDyn evPB]
      itemEv <- getWebSocketResponse reqEv

      -- List and selection checkBox
      selList <- divClass "panel-body" $ do
        showWSProcessing reqEv itemEv
        widgetHold (checkBoxList never [])
          (checkBoxList checkBoxSelAllEv <$> itemEv)

      -- Action buttons
      editDone <-
        bulkEditWidgetActionButtons filtOptsDyn revTypeDyn $ join selList
    return (domEvent Click e, editDone)

  return $ (ShowStatsWindow <$ closeEv, editDone)

buttonWithDisable t active = do
  let attr True = ("type" =: "button") <> ("class" =: "btn btn-primary active")
      attr False = ("type" =: "button") <> ("class" =: "btn btn-primary disabled")
  (e, _) <- elDynAttr' "button" (attr <$> active) $ text t
  return $ domEvent Click e

bulkEditWidgetActionButtons
  :: AppMonad t m
  => Dynamic t BrowseSrsItemsOptions
  -> Dynamic t ReviewType
  -> Dynamic t [SrsEntryId]
  -> AppMonadT t m (Event t ())
bulkEditWidgetActionButtons filtOptsDyn revTypeDyn selList = divClass "panel-footer" $ do
  today <- liftIO $ utctDay <$> getCurrentTime

  let
      felem = flip elem
      btn = buttonWithDisable

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
    showWSProcessing bEditOp doUpdate
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

reviewWidget
  :: forall t m rt proxy . (AppMonad t m, SrsReviewType rt)
  => proxy rt
  -> Event t ()
  -> AppMonadT t m (Event t SrsWidgetView)
reviewWidget p refreshEv = do
  initWanakaBindFn
  let
    rt = reviewType p

  -- ev <- getPostBuild
  -- initEv <- getWebSocketResponse $ GetNextReviewItems rt [] <$ ev

  rec
    -- Input Events
    -- 1. Initial review items
    -- 2. Review result
    -- 3. Fetch new items from server
    -- 4. Undo event
    -- 5. refresh (if initEv was Nothing)

    let
      addItemEv = uncurry AddItemsEv <$> leftmost [fetchMoreReviewsResp] :: Event t (ReviewStateEvent rt)

    -- Output Events
    -- 1. Show review item
    -- 2. Fetch new reviews
    -- 3. Send the result back
    widgetStateDyn <- foldDyn (flip (foldl widgetStateFun))
      (SrsWidgetState Map.empty Nothing def)
      (mergeList [addItemEv, reviewResultEv])

    let
        fetchMoreReviews = GetNextReviewItems rt <$> ffilter ((< 5) . length)
          ((Map.keys . _reviewQueue) <$> (tagDyn widgetStateDyn newReviewEv))

        newReviewEv = leftmost [() <$ reviewResultEv
                               ,() <$ refreshEv]

    -- There is race between newReviewEv and sendResultEv
    dEv <- debounce 2 fetchMoreReviews
    fetchMoreReviewsResp <- getWebSocketResponse dEv

    let
        addResEv = fmapMaybe (_resultQueue)
          (updated widgetStateDyn)
    syncResultWithServer rt addResEv

    (closeEv, reviewResultEv) <- reviewWidgetView
      (_reviewStats <$> widgetStateDyn)
      =<< getRevItemDyn widgetStateDyn newReviewEv

  return $ ShowStatsWindow <$ closeEv

-- Start initEv (show review if available)
-- review done ev, fetch new event after update of dyn
getRevItemDyn :: _
  => Dynamic t (SrsWidgetState rt)
  -> Event t ()
  -> m (Dynamic t (Maybe (ReviewItem, ActualReviewType rt)))
getRevItemDyn widgetStateDyn ev = do
  v <- performEvent $ ffor (tagDyn widgetStateDyn ev) $ \st -> do
    rss <- liftIO $ getRandomItems (Map.toList (_reviewQueue st)) 1
    toss <- liftIO $ randomIO
    return $ (\(_,(ri,rt)) -> (ri, getRandomRT ri rt toss)) <$> (headMay rss)

  holdDyn Nothing v


getRandomItems :: [a] -> Int -> IO [a]
getRandomItems inp s = do
  let l = length inp
      idMap = Map.fromList $ zip [1..l] inp

      loop set = do
        r <- randomRIO (1,l)
        let setN = Set.insert r set
        if Set.size setN >= s
          then return setN
          else loop setN

  set <- loop Set.empty
  return $ catMaybes $
    fmap (\k -> Map.lookup k idMap) $ Set.toList set

-- Required inputs for working of review widget
-- 1. Field (What to display to user as question)
-- 2. Field Tags (?)
-- 3. Answer
-- 4. Additional notes (Shown after answering question)
reviewWidgetView
  :: (AppMonad t m, SrsReviewType rt)
  => Dynamic t SrsReviewStats
  -> Dynamic t (Maybe (ReviewItem, ActualReviewType rt))
  -> AppMonadT t m (Event t (), Event t (ReviewStateEvent rt))
reviewWidgetView statsDyn dyn2 = divClass "panel panel-default" $ do
  let
    statsTextAttr = ("style" =: "font-size: large;")
      <> ("class" =: "center-block text-center")

    showStats = do
      let colour c = ("style" =: ("color: " <> c <>";" ))
          labelText t = elClass "span" "small text-muted" $ text t
      labelText "Pending "
      el "span" $
        dynText $ (tshow . _srsReviewStats_pendingCount) <$> statsDyn
      text "\t|\t"
      labelText " Correct "
      elAttr "span" (colour "green") $
        dynText $ (tshow . _srsReviewStats_correctCount) <$> statsDyn
      text "\t|\t"
      labelText " Incorrect "
      elAttr "span" (colour "red") $
        dynText $ (tshow . _srsReviewStats_incorrectCount) <$> statsDyn

  closeEv <- divClass "panel-heading" $ do
    (e,_) <- elClass' "button" "close" $ text "Close"

    divClass "" $ do
      elAttr "span" statsTextAttr $
        showStats
    return $ domEvent Click e

  let kanjiRowAttr = ("class" =: "center-block")
         <> ("style" =: "height: 15em;\
             \display: table;")
      kanjiCellAttr = ("style" =: "vertical-align: middle;\
             \display: table-cell;")

  elAttr "div" kanjiRowAttr $ elAttr "div" kanjiCellAttr $ do
    let
      showNE (Just (ne, stl)) = elAttr "span" kanjiTextAttr $ do
          mapM_ text (NE.intersperse ", " ne)
        where kanjiTextAttr = ("style" =: stl)
      showNE Nothing = text "No Reviews!"
    dyn $ showNE <$> (dyn2 & mapped . mapped %~ (uncurry getField))

  doRecog <- lift $ speechRecogSetup

  dr <- dyn $ ffor dyn2 $ \case
    (Nothing) -> return never
    (Just v) -> inputFieldWidget doRecog v

  evReview <- switchPromptly never dr
  return (closeEv, evReview)
    --leftmost [evB, dr, drSpeech]

inputFieldWidget
  :: (AppMonad t m, SrsReviewType rt)
  => (Event t () -> m (Event t Result))
  -> (ReviewItem, ActualReviewType rt)
  -> AppMonadT t m (Event t (ReviewStateEvent rt))
inputFieldWidget doRecog (ri@(ReviewItem i k m r), rt) = do
  let
    tiId = "JP-TextInput-IME-Input"
    style = "text-align: center; width: 100%;" <> color
    color = getInputFieldStyle rt
    ph = getInputFieldPlaceHolder rt
    inputField ev = do
      let tiAttr = def
            & textInputConfig_setValue .~ ev
            & textInputConfig_attributes
            .~ constDyn (("style" =: style)
                        <> ("id" =: tiId)
                        <> ("class" =: "form-control")
                        <> ("placeholder" =: ph)
                        <> ("autocapitalize" =: "none")
                        <> ("autocorrect" =: "none")
                        <> ("autocomplete" =: "off"))
      divClass "" $
        divClass "" $ do
          textInput tiAttr

    showResult b = divClass "" $ do
      let s = if b then "Correct: " else "Incorrect: "
          ans = getAnswer ri rt
      text $ s <> (fold $ case ans of
        (Left m) -> NE.intersperse ", " $ fmap unMeaning m
        (Right r) -> NE.intersperse ", " $ (fmap unReading r) <> (ri ^. reviewItemField))
      divClass "" $ do
        text "Notes:"
        case ans of
          (Left _) -> forMOf_ (reviewItemMeaning . _2 . _Just . to unMeaningNotes) ri
            $ \mn -> text $ "> " <> mn
          (Right _) -> forMOf_ (reviewItemReading . _2 . _Just . to unReadingNotes) ri
            $ \mn -> text $ "> " <> mn

  rec
    inpField <- inputField inpTxtEv
    (dr, inpTxtEv, resEv) <-
      reviewInputFieldHandler inpField rt ri

  -- Need dalay, otherwise focus doesn't work
  ev <- delay 0.1 =<< getPostBuild

  let focusAndBind e = do
        DOM.focus e
        let ans = getAnswer ri rt
        when (isRight ans) bindWanaKana

  widgetHold (return ()) (focusAndBind (_textInput_element inpField) <$ ev)

  let resultDisAttr = ("class" =: "")
          <> ("style" =: "height: 6em;\
              \overflow-y: auto")
  elAttr "div" resultDisAttr $
    widgetHold (return ()) (showResult <$> resEv)

  -- Footer
  (wakaru, addEditEv, recogRes) <- divClass "row" $ do
    addEditEv <- divClass "col-sm-2" $ do
      ev <- btn "btn-primary" "Edit"
      newSrsEntryEv <- openEditSrsItemWidget (i <$ ev)
      return $ (\s -> AddItemsEv [getReviewItem s] 0) <$> newSrsEntryEv
    wEv <- divClass "col-sm-2" $
      btn "btn-primary" "分かる"
    recogRes <- divClass "col-md-8" $
      speechRecogWidget doRecog (ri, rt)
    let wakaru = DoReviewEv (i, rt, True) <$ wEv
    return (wakaru, addEditEv, recogRes)

  return $ leftmost [ wakaru, recogRes
                    , dr, addEditEv]

reviewInputFieldHandler
 :: (MonadFix m,
     MonadHold t m,
     Reflex t,
     SrsReviewType rt)
 => TextInput t
 -> ActualReviewType rt
 -> ReviewItem
 -> m (Event t (ReviewStateEvent rt), Event t Text, Event t Bool)
reviewInputFieldHandler ti rt ri@(ReviewItem i k m r) = do
  let enterPress = ffilter (==13) (ti ^. textInput_keypress) -- 13 -> Enter
      correct = checkAnswer n <$> value ti
      n = getAnswer ri rt
      h _ ReviewStart = ShowAnswer
      h _ ShowAnswer = NextReview
      h _ _ = ReviewStart
  dyn <- foldDyn h ReviewStart enterPress
  let

    hiragana = never
    -- case rt of
    --   RecogMeaningReview -> never
    --   -- TODO Implement proper kana writing support
    -- Wanakana or make reflex IME
    --   RecogReadingReview -> toHiragana <$> (ti ^. textInput_input)

  -- the dr event will fire after the correctEv (on second enter press)
    correctEv = tagDyn correct enterPress
    sendResult = ffilter (== NextReview) (tagDyn dyn enterPress)
    dr = (\b -> DoReviewEv (i, rt, b)) <$> tagDyn correct sendResult
  return (dr, hiragana, correctEv)

-- TODO For meaning reviews allow minor mistakes
checkAnswer :: (Either (NonEmpty Meaning) (NonEmpty Reading))
            -> Text
            -> Bool
checkAnswer (Left m) t = elem (T.toCaseFold $ T.strip t) (answers <> woExpl <> woDots)
  where answers = map (T.toCaseFold . unMeaning) m
        -- as (i.e. in the role of) -> as
        woExpl = map (T.strip . fst . (T.breakOn "(")) answers
        -- apart from... -> apart from
        woDots = map (T.strip . fst . (T.breakOn "...")) answers

checkAnswer (Right r) t = elem t answers
  where answers = map unReading r

checkSpeechRecogResult
  :: (AppMonad t m, SrsReviewType rt)
  => (ReviewItem, ActualReviewType rt)
  -> Event t Result
  -> AppMonadT t m (Event t Bool)
checkSpeechRecogResult (ri,rt) resEv = do
  let
    checkF res = do
      ev <- getPostBuild
      let
        r1 = any ((checkAnswer n) . snd) (concat res)
        n = getAnswer ri rt
        readings = case n of
          (Left m) -> []
          (Right r) -> NE.toList r
      if r1
        then return (True <$ ev)
        else do
          respEv <- getWebSocketResponse $ CheckAnswer readings res <$ ev
          return ((== AnswerCorrect) <$> respEv)

  evDyn <- widgetHold (return never)
    (checkF <$> resEv)
  return $ switchPromptlyDyn evDyn


data AnswerBoxState = ReviewStart | ShowAnswer | NextReview
  deriving (Eq)

data SpeechRecogWidgetState
  = NewReviewStart
  | WaitingForRecogResponse
  | WaitingForServerResponse
  | AnswerSuccessful
  | AnswerWrong

btnText :: SpeechRecogWidgetState
  -> Text
btnText NewReviewStart = "Recog"
btnText WaitingForRecogResponse = "Listening"
btnText WaitingForServerResponse = "Processing"
btnText AnswerSuccessful = "Correct! (Click for next)"
btnText AnswerWrong = "Not quite, Try Again?"

-- Widget Sta
speechRecogWidget :: forall t m rt . (AppMonad t m, SrsReviewType rt)
  => (Event t () -> m (Event t Result))
  -> (ReviewItem, ActualReviewType rt)
  -> AppMonadT t m (Event t (ReviewStateEvent rt))
speechRecogWidget doRecog (ri@(ReviewItem i k m r),rt) = do
  let startRecogEv st ev =
        fforMaybe (tagDyn st ev) $ \case
          NewReviewStart -> Just ()
          AnswerWrong -> Just ()
          _ -> Nothing

      doReviewEv st ev =
        fforMaybe (tagDyn st ev) $ \case
          AnswerSuccessful -> Just ()
          _ -> Nothing

  rec
    let
      ev' = leftmost [ev1, ev2, ev3]
      ev1 = ffor recRes2 $ \case
          True -> AnswerSuccessful
          False -> AnswerWrong
      ev2 = WaitingForRecogResponse <$ stRec
      ev3 = WaitingForServerResponse <$ recRes1
      stRec = (startRecogEv stDyn evClick)

    evEv <- dyn $ (btn "btn-primary")
      <$> (btnText <$> stDyn)
    evClick <- switchPromptly never evEv
    ev <- delay 0.1 ev'

    recRes1 <- lift $ doRecog stRec
    recRes2 <- checkSpeechRecogResult (ri,rt) (traceEvent "Recog ->" recRes1)

    stDyn <- holdDyn NewReviewStart ev

  return $ (DoReviewEv (i,rt,True)) <$ doReviewEv stDyn evClick
