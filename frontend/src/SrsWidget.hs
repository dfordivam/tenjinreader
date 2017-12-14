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
  => SrsStats -> m (Event t SrsWidgetView)
showStatsWidget s = do
  divClass "" $ do
    divClass "" $ do
      divClass "" $
        divClass "" $
          text $ tshow s
      divClass "" $ do
        divClass "" $
          divClass "" $
            text ""


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

  ev1 <- button "Recog Review"
  ev2 <- button "Prod Review"
  browseEv <- button "Browse Srs Items"
  return $ leftmost [ShowReviewWindow ReviewTypeRecogReview <$ ev1
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
      modalWidget :: (AppMonad t m)
        => Maybe (SrsEntryId, SrsEntry)
        -> AppMonadT t m ()
      modalWidget (Just s) = do
        rec
          d <- widgetHold (editWidget s)
            ((return never) <$ switchPromptlyDyn d)
        return ()

      modalWidget Nothing = do
        text $ "Some Error"

      editWidget :: AppMonad t m
        => (SrsEntryId, SrsEntry)
        -> AppMonadT t m (Event t ())
      editWidget (sId, s) = do
        rec
          (sNew, saveEv, closeEv) <- editWidgetView s ev
          ev <- getWebSocketResponse $ EditSrsItem sId
            <$> tagDyn sNew saveEv
        return closeEv

  void $ widgetHold (return ()) (modalWidget <$> srsItEv)

modalDiv m = do
  divClass "modal-backdrop fade in" $ return ()
  elAttr "div" attr $ divClass "modal-dialog"
    $ divClass "modal-content" m
  where attr = ("class" =: "modal")
          <> ("style" =: "display: block;")

editWidgetView
  :: MonadWidget t m
  => SrsEntry
  -> Event t ()
  -> m (Dynamic t SrsEntry
       , Event t (), Event t ())
editWidgetView s savedEv = modalDiv $ do
  divClass "modal-header" $ el "h3" $ do
    text $ "Edit " <> (s ^. field . to (NE.head))

  let bodyAttr = ("class" =: "modal-body")
          <> ("style" =: "height: 400px;\
              \overflow-y: scroll")

  ret <- elAttr "div" bodyAttr $ do

    r <- divClass "" $
      editNonEmptyList (s ^. readings) Reading
        $ \r x -> text (unReading r) >> x

    m <- divClass "" $
      editNonEmptyList (s ^. meaning) Meaning
        $ \m x -> el "p" $ do
             text "> "
             text (unMeaning m)
             x

    rn <- divClass "" $ do
      text "Reading Notes"
      divClass "" $
        textArea $ def &
          textAreaConfig_initialValue .~
            (maybe "" unReadingNotes
             $ s ^. readingNotes)

    mn <- divClass "" $ do
      text "Meaning Notes"
      divClass "" $
        textArea $ def &
          textAreaConfig_initialValue .~
            (maybe "" unMeaningNotes
             $ s ^. meaningNotes)

    let
        g c v = gg c <$> value v
        gg c t
          | T.null t = Nothing
          | otherwise = Just $ c t

    return $ SrsEntry (_reviewState s)
              <$> r <*> m
              <*> (g ReadingNotes rn)
              <*> (g MeaningNotes mn)
              <*> pure (s ^. field)

  divClass "modal-footer" $ do
    let savedIcon = elClass "i" "" $ return ()
    saveEv <- button "Save"
    closeEv <- button "Close"
    widgetHold (return ()) (savedIcon <$ savedEv)
    return (ret, saveEv, closeEv)

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

  -- <!-- Modal -->
  -- <div class="modal fade" id="myModal" role="dialog">
  --   <div class="modal-dialog">

  --     <!-- Modal content-->
  --     <div class="modal-content">
  --       <div class="modal-header">
  --         <button type="button" class="close" data-dismiss="modal">&times;</button>
  --         <h3 class="">Edit 変</h3>
  --       </div>
  --       <div class="modal-body">
  --         <div class="">へん<a>(X)
  --   </a><input type="text"><button>Add</button>
  --   </div>
  --   <div class=""><p>&gt; strange<a>(X)</a></p>
  --       <p>&gt; odd<a>(X)</a></p>
  --       <input type="text"><button>Add</button></div>
  --      <div class="">Reading Notes<div class=""><textarea></textarea></div></div><div class="">Meaning Notes<div class=""><textarea></textarea></div></div><div class=""></div>
  -- --       </div>
  --       <div class="modal-footer">
  --       <button>Save</button>
  --         <button type="button" class="btn btn-default" data-dismiss="modal">Close</button>
  --       </div>
  --     </div>

  --   </div>
  -- </div>

editNonEmptyList :: (_)
  => NonEmpty v
  -> (Text -> v)
  -> (forall a . v -> m a -> m a)
  -> m (Dynamic t (NonEmpty v))
editNonEmptyList ne conT renderFun = do
  let
    rem = do
      -- (e,_) <- elClass' "span" "glyphicon glyphicon-remove" $ return ()
      (e,_) <- el' "a" $ text "(X)"
      return $ domEvent Click e

    initMap = Map.fromList $ zip [1..] (NE.toList ne)
    showItem k t = do
      ev <- renderFun t rem
      return (t, (k,Nothing) <$ ev)

  rec
    let
      remAddEv = Map.fromList . NE.toList <$> mergeList [addEv, remEv]
      addEv = attachDyn newKeyDyn (tagDyn (Just . conT <$> value ti) aEv)
      remEv1 = switchPromptlyDyn $
        (leftmost . (fmap snd) . Map.elems) <$> d
      remEv = fmapMaybe g (attachDyn d remEv1)
      g (m,e) = if Map.size m > 1
        then Just e
        else Nothing
      newKeyDyn = ((+ 1) . fst . Map.findMax) <$> d

    d <- listHoldWithKey initMap remAddEv showItem
    ti <- textInput def
    aEv <- button "Add"

  return $ (NE.fromList . (fmap fst) . Map.elems) <$> d

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
    return $ (\(_,(ri,rt)) -> (ri, getRandomRT rt toss)) <$> (headMay rss)

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
         <> ("style" =: "height: 10rem;")
      kanjiTextAttr = ("style" =: "font-size: 5rem;")

  elAttr "div" kanjiRowAttr $
    elAttr "span" kanjiTextAttr $ do
      let
        showNE (Just ne) = mapM_ text (NE.intersperse ", " ne)
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
    style = "text-align: center;" <> color
    color = getInputFieldStyle rt
    inputField ev = do
      let tiAttr = def
            & textInputConfig_setValue .~ ev
            & textInputConfig_attributes
            .~ constDyn ("style" =: style)
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
checkAnswer (Left m) t = elem t answers
  where answers = map unMeaning m
checkAnswer (Right r) t = elem t answers
  where answers = map unReading r

data AnswerBoxState = ReviewStart | ShowAnswer | NextReview
  deriving (Eq)
