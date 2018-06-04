{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ReadingPane where

import FrontendCommon

import qualified Data.Map as Map
import qualified Data.Array as A
import Data.Array (Array, Ix)
import qualified GHCJS.DOM.DOMRectReadOnly as DOM
import qualified GHCJS.DOM.Element as DOM
import qualified GHCJS.DOM.Types as DOM
import Control.Exception
import Data.Text.IO as T

checkVerticalOverflow
  :: (DOM.IsElement self1,
       DOM.IsElement self2, DOM.IsElement self, DOM.MonadJSM m)
  => (self2, self1)
  -> self
  -> ((Double, Maybe TextAdjust) -> IO b)
  -> Bool
  -> m b
checkVerticalOverflow (ie,oe) r action isVertical = do
  let g = if isVertical then DOM.getX else DOM.getY
  rx1 <- g =<< DOM.getBoundingClientRect r
  ix <- g =<< DOM.getBoundingClientRect ie
  ox <- g =<< DOM.getBoundingClientRect oe
  rxH <- DOM.getHeight =<< DOM.getBoundingClientRect r

  let rx = if isVertical then rx1 else rxH + rx1
      lt = if isVertical then (<) else (>)
      gt = if isVertical then (>) else (<)
#if defined (DEBUG)
  liftIO $ FrontendCommon.putStrLn $ (show (rx,ix,ox) :: Text)
#endif
  liftIO $ action $ if
    | (rx `gt` ox && rx `lt` ix) -> (0, Nothing) -- Outside
    | ix `lt` rx -> (0, Just ShrinkText)
    | ox `gt` rx -> (0, Just GrowText)
    | otherwise -> (0, Nothing) -- hidden

-- setupInterObs :: (DOM.MonadDOM m, DOM.IsElement e)
--   => (e, e)
--   -> _
--   -> ((Int, Maybe TextAdjust) -> IO ())
--   -> m DOM.IntersectionObserver
-- setupInterObs ind options action = do
--   cb <- DOM.newIntersectionObserverCallback
--     (intersectionObsCallback ind action)
--   DOM.newIntersectionObserver cb (Just options)

-- intersectionObsCallback (ie,oe) action (e:_) _  = do
--   rx <- DOM.getX =<< DOM.getRootBounds e
--   ix <- DOM.getX =<< DOM.getBoundingClientRect ie
--   ox <- DOM.getX =<< DOM.getBoundingClientRect oe
--   liftIO $ putStrLn $ (show (rx,ix,ox) :: Text)
--   liftIO $ action $ if (rx > ox && rx < ix) -- Outside
--     then (0, Nothing)
--     else if rx > ix
--             then (0, Just ShrinkText)
--             else (0, Just GrowText)

readingPane :: AppMonad t m
  => Event t (ReaderDocumentData)
  -> AppMonadT t m (Event t ())
readingPane docEv = do
  ev <- getPostBuild
  s <- getWebSocketResponse (GetReaderSettings <$ ev)
  v <- widgetHold (readingPaneInt docEv def)
    (readingPaneInt docEv <$> s)
  return (switch . current $ v)

readerSettingsControls
  :: (MonadFix m, MonadHold t m,
       PostBuild t m, DomBuilder t m)
  => ReaderSettingsTree CurrentDb
  -> Bool
  -> m (Dynamic t (ReaderSettingsTree CurrentDb))
readerSettingsControls rsDef full = divClass "col-sm-12 well well-sm form-inline" $ divClass "" $ do
  let
    ddConf :: _
    ddConf = def & dropdownConfig_attributes .~ (constDyn ddAttr)
    ddAttr = ("class" =: "form-control input-sm")
  fontSizeDD <- divClass "col-sm-3" $ do
    el "label" $ text "ぁあ："
    dropdown (rsDef ^. fontSize) (constDyn fontSizeOptions) ddConf
  rubySizeDD <- divClass "col-sm-3" $ do
    el "label" $ text "漢字："
    dropdown (rsDef ^. rubySize) (constDyn fontSizeOptions) ddConf
  lineHeightDD <- divClass "col-sm-3" $ do
    el "label" $ text "間："
    dropdown (rsDef ^. lineHeight) (constDyn lineHeightOptions) ddConf

  (h,v) <- if full
    then do
      heightDD <- divClass "col-sm-3" $ do
        el "label" $ text "高さ："
        dropdown (rsDef ^. numOfLines) (constDyn numOfLinesOptions) ddConf
      writingModeDD <- dropdown (rsDef ^. verticalMode)
        (constDyn writingModeOptions) ddConf
      return (value heightDD, value writingModeDD)
    else
      return (constDyn 400, constDyn False)

  let rsDyn = ReaderSettings <$> (value fontSizeDD) <*> (value rubySizeDD)
                <*> (value lineHeightDD) <*> v <*> h
  return rsDyn

divWrap :: (PostBuild t m, DomBuilder t m) =>
           Dynamic t (ReaderSettingsTree CurrentDb) -> Dynamic t Bool -> m a -> m a
divWrap rs fullscreenDyn w = do
  let
    divAttr = (\s l _ fs -> ("style" =:
      ("font-size: " <> tshow s <>"%;"
        <> "line-height: " <> tshow l <> "%;"
        -- <> "height: " <> tshow h <> "px;"
        <> (if fs then "position: fixed;" else "")
        <> "display: block;" <> "padding: 40px;"))
           <> ("class" =: (if fs then "modal modal-content" else "")))
      <$> (_fontSize <$> rs) <*> (_lineHeight <$> rs)
      <*> (_numOfLines <$> rs) <*> (fullscreenDyn)

  elDynAttr "div" divAttr w

readingPaneInt :: AppMonad t m
  => Event t (ReaderDocumentData)
  -> ReaderSettings CurrentDb
  -> AppMonadT t m (Event t ())
readingPaneInt docEv rsDef = do
  (closeEv,fullScrEv, rsDyn) <- divClass "row" $ do
    rsDyn <- divClass "col-sm-9" $ readerSettingsControls rsDef True
    (closeEv, fullScrEv) <- divClass "col-sm-3" $ do
      closeEv <- btn "btn-default btn-sm" "Close"
      fullScrEv <- btn "btn-default btn-sm" "Full Screen"
      return (closeEv, fullScrEv)
    return (closeEv,fullScrEv, rsDyn)

  getWebSocketResponse (SaveReaderSettings <$> (updated rsDyn))

  _ <- widgetHold ((text "waiting for document data"))
    (verticalReader rsDyn fullScrEv <$> docEv)
  return (closeEv)

----------------------------------------------------------------------------------
-- Vertical rendering

data TextAdjustDirection = ForwardGrow | BackwardGrow
  deriving (Show, Eq)

newtype ParaPos = ParaPos { unParaPos :: Int }
  deriving (Show, Eq, Ord, Ix, Num)

newtype ParaNum = ParaNum { unParaNum :: Int }
  deriving (Show, Eq, Ord, Ix, Num, Enum)

type ParaData = Array ParaNum (Array ParaPos (Either Text (Vocab, [VocabId], Bool)))

-- Start and end of each para being displayed
-- This is isomorphic to start (paranum, parapos) and end (paranum, parapos)
-- as it contains all the content in-between
-- But this is useful in listViewWithKey interface
type DisplayedPara = Map ParaNum (ParaPos, ParaPos)

-- For doing text adjust
newtype ParaOffset = ParaOffset { unParaOffset :: Int }
  deriving (Show, Eq, Ord, Ix, Num)

-- Maps the offset to (paranum, pos)
type ParaOffsetArray = Array ParaOffset (ParaNum, ParaPos)

verticalReader :: forall t m . AppMonad t m
  => Dynamic t (ReaderSettings CurrentDb)
  -> Event t ()
  -> (ReaderDocumentData)
  -> AppMonadT t m ()
verticalReader rs fullScrEv (docId, _, startParaMaybe, endParaNum, annText) = do
  (evVisible, action) <- newTriggerEvent

  visDyn <- holdDyn (0,Nothing) evVisible
#if defined (DEBUG)
  display visDyn
#endif

  let
    divAttr' = (\rs1 fs -> ("class" =: "col-xs-10") <> ("style" =:
      ("font-size: " <> tshow (_fontSize rs1) <>"%;"
        <> "line-height: " <> tshow (_lineHeight rs1) <> "%;"
        <> "height: " <> (if fs
                          then "100%;"
                          else tshow (_numOfLines rs1) <> "px;")
        <> "writing-mode: " <> (if (_verticalMode rs1)
                                then "vertical-rl;"
                                else "lr")
        <> "word-wrap: break-word;"
        <> "display: block;"))) <$> rs

    startParaData = makeParaData annText
    startPara = (\(p,v) -> (ParaNum p, ParaPos $ maybe 1 (max 1) v)) startParaMaybe
    initState = (ParaOffset 100
                , makeParaOffsetArray startParaData (startPara, ForwardGrow))

  rec
    let doStop (Just GrowText) Nothing = Just 1
        doStop (Just GrowText) (Just c) = Just (c + 1)
        doStop (Just ShrinkText) _ = Nothing
        doStop Nothing _ = Nothing
        stopTicks = leftmost [stopTicks1
                             , fforMaybe evVisible (\(_,a) -> if isNothing a
                                                     then Just ()
                                                     else Nothing)
                             , stopOnHitEnd
                             , stopOnHitStart
                             ]

        stopTicks1 = fforMaybe (tag (current stopTicksMaybe) evVisible) $ \case
          Nothing -> Nothing
          (Just c) -> if c > 20 then Just () else Nothing

    stopOnHitEnd <- (flip filterOnEq) Nothing <$>
      debounce 2 (updated nextParaMaybe)
    stopOnHitStart <- (flip filterOnEq) Nothing <$>
      debounce 2 (updated prevParaMaybe)

    stopTicksMaybe <- foldDyn doStop Nothing $
      leftmost [Nothing <$ stopTicks,(snd <$> evVisible)]


    nextParaMaybe <- holdUniqDyn $ getNextParaMaybe <$> lastDisplayedPara <*> textContent
    prevParaMaybe <- holdUniqDyn $ getPrevParaMaybe <$> firstDisplayedPara <*> textContent

    let
      pageChangeEv = leftmost [BackwardGrow <$ prev, ForwardGrow <$ next]

      firstDisplayedPara :: Dynamic t (ParaNum, ParaPos)
      firstDisplayedPara = fst . fst <$> row1Dyn

      lastDisplayedPara :: Dynamic t (ParaNum, ParaPos)
      lastDisplayedPara = snd . fst <$> row1Dyn

      row1Len :: Dynamic t ParaOffset
      row1Len = snd <$> row1Dyn

    let
        foldF :: (_)
          => (ParaOffset, ParaOffsetArray)
          -> AppMonadT t m (Dynamic t (((ParaNum, ParaPos), (ParaNum, ParaPos)), ParaOffset))
        foldF (len, poArray) = do
          stDyn <- foldDyn taF st ((\d -> (poArray,d)) <$> evVisible)
          return $ f <$> stDyn
          where
            taF a b =
#if defined (DEBUG)
              traceShow (snd a) $ traceShowId $
#endif
              (textAdjustF a b)
            f (_,o) = ((min o1 o2, max o1 o2), o)
              where o1 = poArray A.! (ParaOffset 1)
                    o2 = poArray A.! o

            st = ((ParaOffset 1, ui), size)
            size = min (len) (snd $ A.bounds poArray)
            ui = min (len * 2) (snd $ A.bounds poArray)

        newStateEv = attach (current row1Len) $
          attachWith makeParaOffsetArray (current textContent) pageChangeParaEv

        row1Dyn :: Dynamic t (((ParaNum, ParaPos),(ParaNum, ParaPos)), ParaOffset)
        row1Dyn = join row1Dyn'

        pageChangeParaEv = fforMaybe
          (attach (current $ (,) <$> prevParaMaybe <*> nextParaMaybe) pageChangeEv)
          (\((p,n),d) -> case d of
              ForwardGrow -> (,) <$> n <*> pure d
              BackwardGrow -> (,) <$> p <*> pure d)

    (row1Dyn') <- widgetHold (foldF initState) (foldF <$> newStateEv)

#if defined (DEBUG)
    display row1Dyn
#endif

    textContent <- fetchMoreContentF docId startParaData endParaNum
      pageChangeParaEv

    let
      wrapDynAttr = ffor fullscreenDyn $ \b -> if b
        then ("class" =: "modal-content") <>
          ("style" =: "position: fixed; padding: 1em; top: 0; bottom: 0; left: 0; right: 0;")
        else Map.empty
      divAttr = divAttr' <*> fullscreenDyn

    fullscreenDyn <- holdDyn False (leftmost [ True <$ fullScrEv
                                           , False <$ closeEv])

    let
      btnPress = leftmost [next,prev]
      prevNxtBtnWrapDivAttr = (\h fs -> ("class" =: "col-xs-1")
        <> ("style" =: ("height: " <> (if fs then "100%;" else tshow h <> "px;"))))
              <$> (_numOfLines <$> rs) <*> fullscreenDyn

      btnAttr vis = ("class" =: "btn btn-default")
        <> ("style" =: ("height: 80%; width: 1em;" <> visV))
        where
          visV = if vis then "" else "visibility: hidden;"

      leftBtrAttr = join $ ffor (_verticalMode <$> rs) $ \v -> if v
        then btnAttr <$> nextBtnVis
        else btnAttr <$> prevBtnVis

      rightBtrAttr = join $ ffor (_verticalMode <$> rs) $ \v -> if v
        then btnAttr <$> prevBtnVis
        else btnAttr <$> nextBtnVis

    nextBtnVis <- holdDyn (False) $
      leftmost [tag (current $ isJust <$> nextParaMaybe) stopTicks
               , False <$ btnPress]
    prevBtnVis <- holdDyn (False) $
      leftmost [tag (current $ isJust <$> prevParaMaybe) stopTicks
               , False <$ btnPress]

    --------------------------------
    (resizeEv, ((rowRoot, (inside, outside, vIdEv)), (next,(prev,closeEv)))) <- divClass "" $ resizeDetector $ do

      elDynAttr "div" wrapDynAttr $ do
        (leftEv, clearHighlight) <- elDynAttr "div" prevNxtBtnWrapDivAttr $ do
          c1 <- elAttr "div" ("style" =: "height: 10%;") $
            btn "btn-default" "X"
          (e,_) <- elDynAttr' "button" leftBtrAttr $ text "<"
          return (domEvent Click e, c1)

        v <- elDynAttr' "div" divAttr $ do
          vIdEv1 <- el "div" $ do
            let
              f :: _ -> ((ParaNum, ParaPos), (ParaNum, ParaPos))
                -> Map ParaNum (ParaPos, ParaPos)
              f tc ((fn,fs),(ln,ls)) = Map.fromList $ map (\p -> (p, g p)) [fn..ln]
                where g p | p == fn && p == ln = (fs,ls)
                          | p == fn = (fs, snd $ A.bounds $ tc A.! p)
                          | p == ln = (fst $ A.bounds $ tc A.! p, ls)
                          | otherwise = A.bounds $ tc A.! p
              dynMap = f <$> textContent <*> (fst <$> row1Dyn)

            rec
              evMap <- listViewWithKey dynMap
                (renderDynParas rs highlightDyn textContent)
              let vIdEv = fmapMaybe headMay $ Map.elems <$> evMap
                  f Nothing _ = ([],[])
                  f (Just v) (v1,v2) = (v,v1 ++ v2)
              highlightDyn <- foldDyn f ([],[]) (leftmost [Just . fst <$> vIdEv
                                                  , Nothing <$ clearHighlight])
            return $ vIdEv

          (i, _) <- elAttr' "div" ("style" =: "height: 1em; width: 1rem;") $ do
            text ""
          elAttr "div" ("style" =: "height: 1.5em; width: 1.5em;") $ do
            text ""
          (o, _) <- elAttr' "div" ("style" =: "height: 1em; width: 1em;") $ do
            text ""
            return ()
          return (i, o, vIdEv1)

        (rightEv, cEv1) <- elDynAttr "div" prevNxtBtnWrapDivAttr $ do
          let closeBtnAttr = ffor fullscreenDyn $ \fs -> if fs
                then ("style" =: "height: 10%;")
                else ("style" =: "height: 10%; visibility: hidden;")
          cEv <- elDynAttr "div" closeBtnAttr $ do
            btn "btn-default" "Close"
          (e,_) <- elDynAttr' "button" rightBtrAttr $ text ">"
          return (domEvent Click e, cEv)

        let
          -- The button on left is next in vertical and prev in horizontal
          next1 = switch . current $ ffor (_verticalMode <$> rs) $ \v -> if v
            then leftEv else rightEv
          prev1 = switch . current $ ffor (_verticalMode <$> rs) $ \v -> if v
            then rightEv else leftEv

        return (v, (next1, (prev1,cEv1)))

  showVocabDetailsWidget vIdEv

  firstParaDyn <- holdUniqDyn firstDisplayedPara

  _ <- getWebSocketResponse
    ((\(ParaNum p, ParaPos o) -> SaveReadingProgress docId (p,Just o))
      <$> updated firstParaDyn)

  --------------------------------

  let inEl = _element_raw inside
      outEl = _element_raw outside

  let
    -- TODO Stop if we hit end of text, close the document
      startTicksAgain = leftmost [() <$ updated rs
                                 , resizeEv
                                 , closeEv, fullScrEv
                                 , () <$ pageChangeEv]
      ticksWidget = do
        let init = widgetHold (tickW)
              (return never <$ stopTicks)
            tickW = do
              ev <- getPostBuild
              de <- delay 1.5 $ leftmost [ev, () <$ evVisible]
              return $ de
        t <- widgetHold init
          (init <$ startTicksAgain)
        return (switch . current $ join t)

  tickEv <- ticksWidget

  performEvent_ (checkVerticalOverflow (inEl, outEl)
                (_element_raw rowRoot) action
    <$> tag (current (_verticalMode <$> rs)) tickEv)

  return ()

fetchMoreContentF :: (AppMonad t m)
  => ReaderDocumentId
  -> ParaData
  -> Int
  -> Event t ((ParaNum, ParaPos), TextAdjustDirection)
  -> AppMonadT t m (Dynamic t ParaData)
fetchMoreContentF docId annText endParaNum pageChangeEv = do
  rec
    -- Fetch more contents
    -- Keep at most 120 paras in memory
    let

        lastAvailablePara = current $ (snd . A.bounds) <$> textContent
        firstAvailablePara = current $ (fst . A.bounds) <$> textContent
        hitEndEv = fmapMaybe hitEndF (attach lastAvailablePara pageChangeEv)
        hitEndF (ParaNum l,((ParaNum n,_), d))
          | l < endParaNum && d == ForwardGrow && (l - n < 30) = Just (l + 1)
          | otherwise = Nothing
        hitStartEv = fmapMaybe hitStartF (attach firstAvailablePara pageChangeEv)
        hitStartF (ParaNum f,((ParaNum n,_), d))
          | f > 0 && d == BackwardGrow && (n - f < 30) = Just (max 0 (f - 60))
          | otherwise = Nothing

    moreContentEv <- getWebSocketResponse $
      (\p -> ViewDocument docId (Just p)) <$> (leftmost [hitEndEv, hitStartEv])

    textContent <- foldDyn moreContentAccF annText ((\(_,_,_,_,c) -> c) <$>
                                    (fmapMaybe identity moreContentEv))

#if defined (DEBUG)
  -- text "("
  -- display lastAvailablePara
  -- text ", "
  -- display firstAvailablePara
  -- text ")"
#endif

  return textContent

makeParaData :: [(Int, AnnotatedPara)] -> ParaData
makeParaData [] = A.listArray (ParaNum 0, ParaNum (-1)) []
makeParaData n@(n1:_) = A.array (ParaNum (fst n1), ParaNum (fst l)) $ map f n
  where
    f (n1,c) = (ParaNum n1, A.listArray (ParaPos 1, ParaPos (length c)) c)
    (l:_) = reverse n

moreContentAccF :: [(Int, AnnotatedPara)] -> ParaData -> ParaData
moreContentAccF [] o = o
moreContentAccF n o = A.array newBounds $ filter (f newBounds) $
  ((A.assocs pd) ++ (A.assocs o))
  where
    f (l,u) (i,_) = l <= i && i <= u
    (curFirst, curLast) = A.bounds o
    pd = makeParaData n
    (newFirst, newLast) = A.bounds pd
    newBounds = if newFirst > curLast
      then assert (newFirst == (curLast + (ParaNum 1)))
        ((max curFirst (ParaNum $ (unParaNum newLast) - 120)) , newLast)
      else assert ((newLast + (ParaNum 1)) >= curFirst)
        (newFirst, (min curLast (ParaNum $ 120 + (unParaNum newFirst))))

-- Start of next page (one after end of current page)
getNextParaMaybe
  :: (ParaNum, ParaPos)
  -> ParaData
  -> Maybe (ParaNum, ParaPos)
getNextParaMaybe (p, pos) textContent
  | pos < (snd $ A.bounds pOT) = Just (p, pos + 1)
  | lp > p = Just (p + 1, ParaPos 1)
  | otherwise = Nothing
  where
    pOT = textContent A.! p
    (_,lp) = A.bounds textContent

-- End of previous page (one before start of current page)
getPrevParaMaybe
  :: (ParaNum, ParaPos)
  -> ParaData
  -> Maybe (ParaNum, ParaPos)
getPrevParaMaybe (p, pos) textContent
  | pos > (fst $ A.bounds pOT) = Just (p, pos - 1)
  | fp < p = Just (p - 1, snd (A.bounds pp))
  | otherwise = Nothing
  where
    pOT = textContent A.! p
    pp = textContent A.! (p - 1)
    (fp,_) = A.bounds textContent

data TextAdjust = ShrinkText | GrowText
  deriving (Show, Eq)

makeParaOffsetArray
  :: ParaData
  -> ((ParaNum, ParaPos), TextAdjustDirection)
  -> ParaOffsetArray
makeParaOffsetArray tc (start, d) =
  A.listArray (ParaOffset 1, ParaOffset (length allPs)) allPs
  where
    allPs = poss start
    (tcL, tcU) = A.bounds tc
    poss (spara, spos) = case d of
      ForwardGrow
        | spara > tcU -> []
        | otherwise -> (g $ A.range (spos, snd $ A.bounds (tc A.! spara)))
          ++ (poss (spara + ParaNum 1, ParaPos 1))
      BackwardGrow
        | spara < tcL -> []
        | spara == tcL -> this
        | otherwise -> this
          ++ (poss (spara - (ParaNum 1), nextLast))
      where
        this = g $ reverse $ A.range (fst $ A.bounds (tc A.! spara), spos)
        nextLast = snd $ A.bounds (tc A.! (spara - (ParaNum 1)))
        g = map (\x -> (spara, x))

halfParaOffset :: ParaOffset -> ParaOffset -> ParaOffset
halfParaOffset (ParaOffset u) (ParaOffset l)
  = ParaOffset $ ceiling $ (fromIntegral (u - l) / 2)

-- Converge on the text content size based on Events
-- The Input events will toggle between shrink and grow
-- This is equivalent to binary space search.
-- Keep track of low and upper bound
-- lower bound causes Grow event, upper bound causes Shrink event
-- Do binary search between these bounds
-- When a resize occurs (ie event goes Nothing -> Just)
-- The bounds will have to be re-calculated

textAdjustF
  :: (ParaOffsetArray, (Double, Maybe TextAdjust))
  -> ((ParaOffset, ParaOffset), ParaOffset)
  -> ((ParaOffset, ParaOffset), ParaOffset)

textAdjustF (_, (off, Just ShrinkText)) ((li,_), ps)
  -- During viewport size change li can become invalid
  | li >= ps
    = assert (li > ParaOffset 1)
      ((halfPs, ps), halfPs)
  | otherwise = ((li, ps), newV)
  where
    newV = ps - (max offV (halfParaOffset ps li))
    halfPs = if offV > 0
      then halfParaOffset ps (ParaOffset 1)
      else halfParaOffset ps (halfParaOffset ps (ParaOffset 1))
    offV = ParaOffset $ floor $ off / 20

textAdjustF (poArr, (off, Just GrowText)) ((_,ui), ps)
  -- During viewport size change ui can become invalid
  | ui <= ps && (ps < lastC)
    = ((ps, doublePs), doublePs)

  | otherwise = ((ps, ui), min lastC newV)
  where
    -- Heuristic, add some offset based on pixel difference
    newV = (ps + (max offV (halfParaOffset ui ps)))
    lastC = snd $ A.bounds poArr
    doublePs = min lastC $ if offV > 0
      then (ParaOffset (2 * (unParaOffset ps)))
      else (ps + (halfParaOffset ps (ParaOffset 1)))
    offV = ParaOffset $ floor $ off / 20

textAdjustF (_, (_,Nothing)) v = v

renderDynParas
  :: ((RawElement (DomBuilderSpace m) ~ e,
        DomBuilder t m,
        PostBuild t m,
        MonadHold t m,
        MonadFix m))
  => Dynamic t (ReaderSettings CurrentDb) -- Used for rubySize
  -> Dynamic t ([VocabId], [VocabId])
  -> Dynamic t ParaData
  -> ParaNum
  -> Dynamic t (ParaPos, ParaPos)
  -> m (Event t ([VocabId], (Text, Maybe e)))
renderDynParas rs vIdDyn textContent pn dynPos = do
  let
      renderF highlightDyn = renderOnePara highlightDyn (_rubySize <$> rs)
      pc = (\tc -> tc A.! pn) <$> textContent
      dt = (\p pc -> A.ixmap p identity pc) <$> dynPos <*> pc

  switchPromptly never
      =<< dyn ((\p -> renderF vIdDyn (A.elems p)) <$> dt)

vocabRubyLight :: (DomBuilder t m)
  => Int
  -> Bool
  -> Vocab -> m ()
vocabRubyLight fontSizePct vis (Vocab ks) = do
  let
    rubyAttr = (\s -> "style" =: ("font-size: " <> tshow s <> "%;")) fontSizePct
    g r True = r
    g _ _ = ""
    f (Kana k) = text k
    f (KanjiWithReading (Kanji k) r)
      = elAttr "ruby" rubyAttr $ do
          text k
          el "rt" $ text (g r vis)

  void $ el "span" $ mapM f ks

renderOneParaLight :: (DomBuilder t m)
  => Int
  -> [Either Text (Vocab, [VocabId], Bool)]
  -> m ()
renderOneParaLight rSize annTextPara = do
  el "p" $ do
    let f (Left t) = text t
        f (Right (v, _, vis)) = vocabRubyLight rSize vis v
    void $ mapM f (annTextPara)

renderDynParasLight
  :: (MonadSample t m,
       PostBuild t m,
       DomBuilder t m)
  => Dynamic t (ReaderSettings CurrentDb) -- Used for rubySize
  -> ParaData
  -> ParaNum
  -> Dynamic t (ParaPos, ParaPos)
  -> m (Event t ([VocabId], (Text, Maybe (DOM.Element))))
renderDynParasLight rsDyn tc pn dynPos = do
  let renderEachPara rs dt = do
        renderOneParaLight (_rubySize rs) dt

      pc = tc A.! pn
      dt = (\p -> A.ixmap p identity pc) <$> dynPos

  rsC <- sample $ current rsDyn
  void $ dyn ((\p -> renderEachPara rsC (A.elems p)) <$> dt)
  return never

----------------------------------------------------------------------------------
lineHeightOptions = Map.fromList $ (\x -> (x, (tshow x) <> "%"))
  <$> ([100,150..400]  :: [Int])

fontSizeOptions = Map.fromList $ (\x -> (x, (tshow x) <> "%"))
  <$> ([80,85..200]  :: [Int])

writingModeOptions = Map.fromList $
  [(False, "Horizontal" :: Text)
  , (True, "Vertical")]

numOfLinesOptions = Map.fromList $ (\x -> (x, (tshow x) <> "px"))
  <$> ([100,150..2000]  :: [Int])
