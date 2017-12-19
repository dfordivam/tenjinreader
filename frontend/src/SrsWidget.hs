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
import qualified JSDOM.HTMLElement as DOM

data SrsWidgetView =
  ShowStatsWindow | ShowReviewWindow ReviewType | ShowBrowseSrsItemsWindow
  deriving (Eq)

srsWidget
  :: AppMonad t m
  => AppMonadT t m ()
srsWidget = divClass "" $ do
  let

  rec
    let
      visEv = leftmost [ev1,ev2,ev3,ev4]
    vis <- holdDyn ShowStatsWindow visEv

    ev1 <- handleVisibility ShowStatsWindow vis $
      showStats

    ev2 <- handleVisibility ShowBrowseSrsItemsWindow vis $
      browseSrsItemsWidget

    ev3 <- handleVisibility (ShowReviewWindow ReviewTypeRecogReview) vis $
      reviewWidget (Proxy :: Proxy RecogReview)

    ev4 <- handleVisibility (ShowReviewWindow ReviewTypeProdReview) vis $
      reviewWidget (Proxy :: Proxy ProdReview)
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
  => (SrsStats, SrsStats) -> m (Event t SrsWidgetView)
showStatsWidget (recog, prod) = do
  let
    w lbl rs = divClass "panel panel-default" $ do
      ev <- divClass "panel-heading" $ divClass "row" $ do
        divClass "col-sm-2" $ text lbl
        divClass "col-sm-4" $
          button "Start Review"

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
  browseEv <- button "Browse Srs Items"
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
          el "table" $ el "tbody" $ forM es $ checkBoxListEl selAllEv

        let f (v, True) s = Set.insert v s
            f (v, False) s = Set.delete v s
        selList <- foldDyn f Set.empty (leftmost evs)

        return $ Set.toList <$> selList

    checkBoxListEl :: Event t Bool -> SrsItem
      -> AppMonadT t m (Event t (SrsEntryId, Bool))
    checkBoxListEl selAllEv (SrsItem i t) = el "tr" $ do
      c1 <- el "td" $
        checkbox False $ def & setValue .~ selAllEv
      el "td" $
        text $ fold $ NE.intersperse ", " $ t
      ev <- el "td" $
        button "edit"
      openEditSrsItemWidget $ i <$ ev
      return $ (,) i <$> updated (value c1)

  -- UI
  closeEv <- divClass "" $
    button "Close Widget"
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

  return $ ShowStatsWindow <$ closeEv

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
bulkEditWidgetActionButtons filtOptsDyn revTypeDyn selList = divClass "" $ do
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

    let
        fetchMoreReviews = GetNextReviewItems rt <$> ffilter ((< 5) . length)
          ((Map.keys . _reviewQueue) <$> (updated widgetStateDyn))

        newReviewEv = leftmost [() <$ reviewResultEv
                               , () <$ refreshEv
                               , () <$ initEv]

    dEv <- debounce 120 fetchMoreReviews
    fetchMoreReviewsResp <- getWebSocketResponse dEv

    let
        addResEv = traceEvent "addResEv" $ fmapMaybe (_resultQueue)
          (updated widgetStateDyn)
    syncResultWithServer rt addResEv

    refreshEv <- button "refresh"
    (closeEv, reviewResultEv) <- elAttr "div" attr $ divClass "" $ do
      closeEv <- divClass "" $
        button "Close Review"


      reviewResultEv <- reviewWidgetView
        (_reviewStats <$> widgetStateDyn)
        =<< getRevItemDyn widgetStateDyn newReviewEv

      return (closeEv, reviewResultEv)

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
         <> ("style" =: "height: 15rem; text-align: center;")

  elAttr "div" kanjiRowAttr $ do
    let
      showNE (Just (ne, stl)) = elAttr "span" kanjiTextAttr $ do
          mapM_ text (NE.intersperse ", " ne)
        where kanjiTextAttr = ("style" =: stl)
      showNE Nothing = text "No Reviews!"
    dyn $ showNE <$> (dyn2 & mapped . mapped %~ (uncurry getField))

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

inputFieldWidget
  :: _
  => (ReviewItem, ActualReviewType rt)
  -> m (Event t (ReviewStateEvent rt))
inputFieldWidget (ri@(ReviewItem i k m r), rt) = do
  let
    style = "text-align: center; width: 100%;" <> color
    color = getInputFieldStyle rt
    ph = getInputFieldPlaceHolder rt
    inputField ev = do
      let tiAttr = def
            & textInputConfig_setValue .~ ev
            & textInputConfig_attributes
            .~ constDyn (("style" =: style)
                        <> ("placeholder" =: ph))
      divClass "" $
        divClass "" $ do
          textInput tiAttr

    showResult b = divClass "" $ do
      let s = if b then "Correct: " else "Incorrect: "
          ans = getAnswer ri rt
      text $ s <> (fold $ case ans of
        (Left m) -> NE.intersperse ", " $ fmap unMeaning m
        (Right r) -> NE.intersperse ", " $ fmap unReading r)
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

  ev <- delay 0.1 =<< getPostBuild

  widgetHold (return ()) ( DOM.focus (_textInput_element inpField) <$ ev)

  widgetHold (return ()) (showResult <$> resEv)
  drForced <- button "分かる"
  return $ leftmost [DoReviewEv (i, rt, True) <$ drForced
                    , dr]

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

data AnswerBoxState = ReviewStart | ShowAnswer | NextReview
  deriving (Eq)
