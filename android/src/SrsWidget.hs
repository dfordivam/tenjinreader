{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
module SrsWidget where

import FrontendCommon
import ReviewState

import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)
import System.Random
import qualified GHCJS.DOM.HTMLElement as DOM

import Language.Javascript.JSaddle.Object
import Language.Javascript.JSaddle.Types (liftJSM)

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
        return $ switch (current evDyn)
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
  retEvDyn <- widgetHold (return never) (showStatsWidget <$> s)
  return $ switch (current retEvDyn)

showStatsWidget
  :: (MonadWidget t m)
  => (SrsStats, SrsStats) -> m (Event t SrsWidgetView)
showStatsWidget (recog, prod) = do
  let
    w lbl rs = divClass "panel" $ do
      ev <- divClass "panel-heading" $ divClass "level is-mobile" $ do
        divClass "level-left" $ text lbl
        divClass "level-item" $
          btn "" "Start Review"

      divClass "panel-block" $ divClass "container" $ divClass "level is-mobile" $ do
        divClass "level-item has-text-centered" $ divClass "" $ do
          elClass "p" "heading" $ text "Pending"
          elClass "p" "title" $ text $ tshow (reviewsToday rs)
        divClass "level-item has-text-centered" $ divClass "" $ do
          elClass "p" "heading" $ text "Total"
          elClass "p" "title" $ text $ tshow (totalReviews rs)
        divClass "level-item has-text-centered" $ divClass "" $ do
          elClass "p" "heading" $ text "Correct"
          elClass "p" "title" $ text $ (tshow (averageSuccess rs) <> "%")

      return ev

  ev1 <- w "Recognition Review" recog
  ev2 <- w "Production Review" prod
  browseEv <- btn "" "Browse Srs Items"
  return $ leftmost
    [ShowReviewWindow ReviewTypeRecogReview <$ ev1
    , ShowReviewWindow ReviewTypeProdReview <$ ev2
    , ShowBrowseSrsItemsWindow <$ browseEv]

srsLevels = Map.fromList
  [ (LearningLvl, "< 4 Days" :: Text)
  , (IntermediateLvl , "4 - 60")
  , (MatureLvl, "> 60 Days")]

data BrowseSrsItemsOptions
  = BrowseSrsItemsDue
  | BrowseSrsItemsNew
  | BrowseSrsItemsSusp
  | BrowseSrsItemsOther
  deriving (Eq, Ord, Show)

browseOptions = Map.fromList
  [ (BrowseSrsItemsDue, "Due" :: Text)
  ,  (BrowseSrsItemsNew, "New")
  ,  (BrowseSrsItemsSusp, "Susp")
  ,  (BrowseSrsItemsOther, "Others")]

revTypeSel = Map.fromList
  [ (ReviewTypeRecogReview, "Recog" :: Text)
  , (ReviewTypeProdReview, "Prod")]

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
    panelHead = divClass "panel-heading notification" $ do
      fo <- divClass "field is-grouped" $ divClass "" $ do
        -- Selection buttons
        selectAllToggleCheckBox <- do
          checkbox False def -- & setValue .~ allSelected

        filt <- elClass "span" "select is-small" $
          dropdown (BrowseSrsItemsDue) (constDyn browseOptions) def
        levels <-  elClass "span" "select is-small" $
          dropdown (LearningLvl) (constDyn srsLevels) def
        revType <-  elClass "span" "select is-small" $
          dropdown (ReviewTypeRecogReview) (constDyn revTypeSel) def

        brwDyn <- getBrowseSrsItemsEv filt levels
        let filtOptsDyn = BrowseSrsItems <$> value revType <*> brwDyn
        return (filtOptsDyn, selectAllToggleCheckBox, value filt, value revType)
      clEv <- do
        (e,_) <- elClass' "button" "delete is-medium" $ text "close"
        return (domEvent Click e)
      return (clEv, fo)

    itemList selAllEv es = do
      dyns <- elAttr "div" (("class" =: "")
              <> ("style" =: "height: 60vh; overflow-y: auto")) $
        forM es $ checkBoxListEl selAllEv

      let f (v, True) = Just v
          f (_, False) = Nothing
          ds = distributeListOverDynPure dyns

      return $ (catMaybes . (map f)) <$> ds

    checkBoxListEl :: Event t Bool -> SrsItem
      -> AppMonadT t m (Dynamic t (SrsEntryId , Bool))
    checkBoxListEl selAllEv (SrsItem i t) = divClass "panel-block" $ divClass "control" $ divClass "level is-mobile" $ do
      c1 <- divClass "level-left" $ do
        c <- divClass "level-item" $ checkbox False $ def & setValue .~ selAllEv
        divClass "level-item" $ elClass "p" "" $
          text $ fold $ NE.intersperse ", " $ t
        return c
      divClass "level-right" $ divClass "level-item" $
        editSrsItemWidget i
      return $ (,) i <$> (value c1)

  -- UI
  (closeEv, editDone) <- divClass "panel" $ do
    -- Filter Options
    (clEv,(browseSrsFilterDyn, selectAllToggleCheckBox, filtOptsDyn, revTypeDyn)) <-
      panelHead

    evPB <- getPostBuild
    rec
      let
        checkBoxSelAllEv = updated $
          value selectAllToggleCheckBox

        reqEv = leftmost
          [ updated browseSrsFilterDyn
          , tag (current browseSrsFilterDyn) editDone
          , tag (current browseSrsFilterDyn) evPB]
      itemEv <- getWebSocketResponse reqEv

      -- List and selection checkBox
      selList <- divClass "panel-body" $ do
        widgetHold (itemList never [])
          (itemList checkBoxSelAllEv <$> itemEv)
      -- Action buttons
      editDone <-
        bulkEditWidgetActionButtons filtOptsDyn revTypeDyn $ join selList
    return (clEv, editDone)

  return $ (ShowStatsWindow <$ closeEv, editDone)

btnWithDisable :: (_)
  => Text
  -> Dynamic t1 Bool
  -> m (Event t ())
btnWithDisable t active = do
  let attr b = ("class" =: "button") <> (if b then (Map.empty) else ("disabled" =: ""))
  (e, _) <- elDynAttr' "a" (attr <$> active) $ text t
  return $ domEvent Click e

bulkEditWidgetActionButtons
  :: AppMonad t m
  => Dynamic t BrowseSrsItemsOptions
  -> Dynamic t ReviewType
  -> Dynamic t [SrsEntryId]
  -> AppMonadT t m (Event t ())
bulkEditWidgetActionButtons filtOptsDyn revTypeDyn selList = divClass "field is-grouped is-grouped-centered is-grouped-multiline" $ do
  today <- liftIO $ utctDay <$> getCurrentTime

  let
      felem = flip elem

  suspendEv <- elClass "p" "control" $
    btnWithDisable "Suspend" $
      (felem [BrowseSrsItemsDue, BrowseSrsItemsOther]) <$> filtOptsDyn

  markDueEv <-  elClass "p" "control" $
    btnWithDisable "Mark Due" $ (felem [BrowseSrsItemsSusp, BrowseSrsItemsOther]) <$> filtOptsDyn

  deleteEv <-  elClass "p" "control" $
    btnWithDisable "Delete" (constDyn True)

  reviewDateChange <- elClass "p" "control" $
    btnWithDisable "Change Review Date" $ (felem [BrowseSrsItemsDue,
       BrowseSrsItemsSusp, BrowseSrsItemsOther]) <$> filtOptsDyn

  dateDyn <- elClass "p" "control" $
    datePicker today

  let bEditOp = leftmost
        [DeleteSrsItems <$ deleteEv
        , MarkDueSrsItems <$ markDueEv
        , SuspendSrsItems <$ suspendEv
        , ChangeSrsReviewData <$> tag (current dateDyn) reviewDateChange]
  doUpdate <- getWebSocketResponse $
    (attachWith ($) (current $ BulkEditSrsItems <$> revTypeDyn <*> selList) bEditOp)
  return $ fmapMaybe identity doUpdate

datePicker
  :: (MonadWidget t m)
  => Day -> m (Dynamic t Day)
datePicker today = divClass "" $ do
  let dayList = makeList [1..31]
      monthList = makeList [1..12]
      yearList = makeList [2000..2030]
      makeList x1 = constDyn $ Map.fromList $ (\x -> (x, tshow x)) <$> x1
      (currentYear, currentMonth, currentDay)
        = toGregorian today
      mycol = elClass "span" "select"
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
      f a True = Just <$> a
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
    -- Output Events
    -- 1. Show review item
    -- 2. Fetch new reviews
    -- 3. Send the result back
    widgetStateDyn <- foldDyn (flip (foldl widgetStateFun))
      (SrsWidgetState Map.empty Map.empty Nothing def)
      (mergeList [addItemEv, reviewResultEv])

    let
      addResEv = fmapMaybe (_resultQueue)
          (updated widgetStateDyn)

      newReviewEv = leftmost [() <$ reviewResultEv
                             ,() <$ refreshEv]

    (addItemEv :: Event t (ReviewStateEvent rt))
      <- syncResultWithServer rt refreshEv
        addResEv widgetStateDyn

    (closeEv, reviewResultEv) <- reviewWidgetView
      (_reviewStats <$> widgetStateDyn)
      =<< getRevItemDyn widgetStateDyn newReviewEv

  return $ ShowStatsWindow <$ closeEv

-- Start initEv (show review if available)
-- review done ev, fetch new event after update of dyn
getRevItemDyn
  :: (MonadFix m,
       MonadHold t m,
       SrsReviewType rt,
       MonadIO (Performable m),
       PerformEvent t m)
  => Dynamic t (SrsWidgetState rt)
  -> Event t ()
  -> m (Dynamic t (Maybe (ReviewItem, ActualReviewType rt)))
getRevItemDyn widgetStateDyn ev = do
  rec
    v <- performEvent $ ffor (tagPromptlyDyn ((,) <$> riDyn <*> widgetStateDyn) ev) $
      \(last, st) -> do
        t <- liftIO $ getCurrentTime
        let
          allrs = (Map.toList (_reviewQueue st))
          rs | length allrs > 1 = maybe allrs
               (\(l,_) -> filter (\r -> (_reviewItemId l) /= (fst r)) allrs) last
             | otherwise = allrs

        let
          loop = do
            rss <- liftIO $ getRandomItems rs 1
            let
              riMb = headMay rss
            case (join $ (\(i,_) -> Map.lookup i (_incorrectItems st)) <$> riMb) of
              Nothing -> return riMb
              (Just t1) -> if (diffUTCTime t t1 > 60)
                then return riMb
                else if (length allrs > 1) && (length rs > (Map.size (_incorrectItems st)))
                       then loop
                       else return riMb

        rIdMb <- loop
        toss <- liftIO $ randomIO
        return $ (\(_,(ri,rt)) -> (ri, getRandomRT ri rt toss)) <$> rIdMb

    riDyn <- holdDyn Nothing v
  return riDyn


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
    showStatsW = do
      let colour c = ("style" =: ("color: " <> c <>";" ))
          labelText t = elClass "span" "small text-muted" $ text t
      el "span" $
        dynText $ (tshow . _srsReviewStats_pendingCount) <$> statsDyn
      text "\t|\t"
      elAttr "span" (colour "green") $
        dynText $ (tshow . _srsReviewStats_correctCount) <$> statsDyn
      text "\t|\t"
      elAttr "span" (colour "red") $
        dynText $ (tshow . _srsReviewStats_incorrectCount) <$> statsDyn

  (autoFocus, closeEv) <- divClass "panel-heading notification" $ do
    autoFocus <- do
      cb <- checkbox False $ def
      text "Auto Focus"
      return (value cb)

    divClass "has-text-centered" $ do
      elClass "span" "" $
        showStatsW
    cEv <- do
      (e,_) <- elClass' "button" "delete is-medium" $ text "close"
      return (domEvent Click e)
    return $ (autoFocus, cEv)

  let kanjiRowAttr = ("class" =: "container")
         <> ("style" =: "height: 15em; display: table;")
      kanjiCellAttr = ("style" =: "vertical-align: middle; max-width: 25em; display: table-cell;")

  _ <- elAttr "div" kanjiRowAttr $ elAttr "div" kanjiCellAttr $ do
    let
      showNE (Just (ne, stl)) = elAttr "span" kanjiTextAttr $ do
          mapM_ text (NE.intersperse ", " ne)
        where kanjiTextAttr = ("style" =: stl)
      showNE Nothing = text "No Reviews! (Please close and open again to refresh)"
    dyn $ showNE <$> (dyn2 & mapped . mapped %~ (uncurry getField))

  dr <- dyn $ ffor dyn2 $ \case
    (Nothing) -> return never
    (Just v) -> inputFieldWidget closeEv autoFocus v

  evReview <- switchPromptly never dr
  return (closeEv, evReview)

inputFieldWidget
  :: (AppMonad t m, SrsReviewType rt)
  => Event t ()
  -> Dynamic t Bool
  -> (ReviewItem, ActualReviewType rt)
  -> AppMonadT t m (Event t (ReviewStateEvent rt))
inputFieldWidget closeEv autoFocus (ri@(ReviewItem i k m _), rt) = do
  let
    tiId = getInputFieldId rt
    style = "text-align: center; width: 100%;" <> color
    color = getInputFieldStyle rt
    ph = getInputFieldPlaceHolder rt
    inputField = do
      let tiAttr = def
            & textInputConfig_attributes
            .~ constDyn (("style" =: style)
                        <> ("id" =: tiId)
                        <> ("class" =: "input")
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

  inpField <- inputField
  (dr, resEv) <-
    reviewInputFieldHandler inpField rt ri

  -- Need dalay, otherwise focus doesn't work
  evPB <- delay 0.1 =<< getPostBuild

  let focusAndBind e b = do
        when b $ DOM.focus e
        let ans = getAnswer ri rt
        when (isRight ans) bindWanaKana

  _ <- widgetHold (return ())
    (focusAndBind (_textInput_element inpField) <$> tagDyn autoFocus evPB)

  let resultDisAttr = ("class" =: "")
          <> ("style" =: "height: 6em; overflow-y: auto")
  rec
    _ <- elAttr "div" resultDisAttr $
      widgetHold (return ()) (showResult <$> (leftmost [resEv, shimesuEv]))

    -- Footer
    (addEditEv, susBuryEv) <- divClass "field is-grouped is-grouped-centered is-grouped-multiline" $ do
      divClass "" $ do
        openSentenceWidget (NE.head k, map (unMeaning) $ NE.toList (fst m)) (Right i)

      aeEv <- divClass "" $ do
        newSrsEntryEv <- editSrsItemWidget i
        return $ ((\s -> AddItemsEv [getReviewItem s] Nothing) <$> newSrsEntryEv)

      sbEv <- divClass "" $ do
        ev1 <- btn "" "Bury"
        ev2 <- btn "" "Suspend"
        return (leftmost [SuspendEv i <$ ev2, BuryEv i <$ ev1])

      return (aeEv, sbEv)

    (shiruResEv, shimesuEv) <- divClass "field is-grouped is-grouped-centered is-grouped-multiline" $ do
      shirimasu <- divClass "" $
        btn "is-medium" "知っている"

      (shimesu, shiranai) <- divClass "" $ do
        rec
          ev <- switch . current <$> widgetHold (btn "is-medium" "示す")
            (return never <$ ev)
        ev2 <- widgetHoldWithRemoveAfterEvent ((btn "is-medium" "知らない") <$ ev)
        return (ev,ev2)
      shiruRes <- tagWithTime $ (\b -> (i, rt, b)) <$>
        leftmost [True <$ shirimasu, False <$ shiranai]

      return (shiruRes , False <$ shimesu)

  return $ leftmost [shiruResEv, dr, addEditEv, susBuryEv]

tagWithTime ev = performEvent $ ffor ev $ \e@(i,_,b) -> do
  t <- liftIO $ getCurrentTime
  return $ DoReviewEv e t

reviewInputFieldHandler
 :: (MonadFix m,
     MonadHold t m,
     PerformEvent t m,
     MonadIO (Performable m),
     Reflex t,
     SrsReviewType rt)
 => TextInput t
 -> ActualReviewType rt
 -> ReviewItem
 -> m (Event t (ReviewStateEvent rt), Event t Bool)
reviewInputFieldHandler ti rt ri@(ReviewItem i _ _ _) = do
  let enterPress = ffilter (==13) (ti ^. textInput_keypress) -- 13 -> Enter
      correct = current $ checkAnswer n <$> value ti
      n = getAnswer ri rt
      h _ ReviewStart = ShowAnswer
      h _ ShowAnswer = NextReview
      h _ _ = ReviewStart
  d <- foldDyn h ReviewStart enterPress
  let

  -- the dr event will fire after the correctEv (on second enter press)
    correctEv = tag correct enterPress
    sendResult = ffilter (== NextReview) (tag (current d) enterPress)
  dr <- tagWithTime $ (\b -> (i, rt, b)) <$> tag correct sendResult
  return (dr, correctEv)

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
initWanakaBindFn :: (MonadWidget t m) => m ()
initWanakaBindFn =
  void $ liftJSM $ eval ("globalFunc_wanakanaBind = function () {"
                <> "var input1 = document.getElementById('JP-TextInput-IME-Input1');"
                <> "var input2 = document.getElementById('JP-TextInput-IME-Input2');"
                <> "wanakana.bind(input1); wanakana.bind(input2);}" :: Text)

bindWanaKana :: (MonadWidget t m) => m ()
bindWanaKana =
        void $ liftJSM $
          jsg0 ("globalFunc_wanakanaBind" :: Text)
