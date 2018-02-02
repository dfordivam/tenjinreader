{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ReadingPane where

import FrontendCommon

import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector as V
import qualified Data.Array as A
import Data.Array (Array, Ix)
import qualified GHCJS.DOM.DOMRectReadOnly as DOM
import qualified GHCJS.DOM.Element as DOM
import qualified GHCJS.DOM.Document as DOM
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.IntersectionObserverEntry as DOM hiding (getBoundingClientRect)
import qualified GHCJS.DOM.IntersectionObserverCallback as DOM
import qualified GHCJS.DOM.IntersectionObserver as DOM
import Control.Exception
import Reflex.Dom.Widget.Resize
import Language.Javascript.JSaddle.Value
import JavaScript.Object

checkOverFlow e heightDyn = do
  v <- sample $ current heightDyn
  let overFlowThreshold = fromIntegral v
  rect <- DOM.getBoundingClientRect (_element_raw e)
  trects <- DOM.getClientRects (_element_raw e)
  y <- DOM.getY rect
  h <- DOM.getHeight rect
  -- text $ "Coords: " <> (tshow y) <> ", " <> (tshow h)
  return (y + h > overFlowThreshold)

checkVerticalOverflow (ie,oe) r action = do
  rx <- DOM.getX =<< DOM.getBoundingClientRect r
  ix <- DOM.getX =<< DOM.getBoundingClientRect ie
  ox <- DOM.getX =<< DOM.getBoundingClientRect oe
  liftIO $ putStrLn $ (show (rx,ix,ox) :: Text)
  liftIO $ action $ if
    | (rx > ox && rx < ix) -> (0, Nothing) -- Outside
    | ix < rx -> (0, Just ShrinkText)
    | ox > rx -> (0, Just GrowText)
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
  -> AppMonadT t m (Event t (), Event t (ReaderDocument CurrentDb))
readingPane docEv = do
  ev <- getPostBuild
  s <- getWebSocketResponse (GetReaderSettings <$ ev)
  v <- widgetHold (readingPaneInt docEv def)
    (readingPaneInt docEv <$> s)
  return (switchPromptlyDyn (fst <$> v)
         , switchPromptlyDyn (snd <$> v))

readerSettingsControls rsDef = divClass "col-sm-10 form-inline" $ divClass "" $ do
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
  heightDD <- divClass "col-sm-3" $ do
    el "label" $ text "高さ："
    dropdown (rsDef ^. numOfLines) (constDyn numOfLinesOptions) ddConf
  -- writingModeDD <- dropdown (rsDef ^. verticalMode) (constDyn writingModeOptions) ddConf
  let rsDyn = ReaderSettings <$> (value fontSizeDD) <*> (value rubySizeDD)
                <*> (value lineHeightDD) <*> (writingModeDD)
                <*> (value heightDD)
      writingModeDD = constDyn True
  return rsDyn

divWrap rs fullscreenDyn w = do
  let
    divAttr = (\s l h fs -> ("style" =:
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
  -> AppMonadT t m (Event t (), Event t (ReaderDocument CurrentDb))
readingPaneInt docEv rsDef = do
  (closeEv,fullScrEv, rsDyn) <- divClass "row" $ do
    closeEv <- btn "btn-default btn-sm" "Close"
    fullScrEv <- btn "btn-default btn-sm" "Full Screen"

    rsDyn <- readerSettingsControls rsDef
    return (closeEv,fullScrEv, rsDyn)

  getWebSocketResponse (SaveReaderSettings <$> (updated rsDyn))

  widgetHold ((text "waiting for document data"))
    -- (readingPaneView <$> docEv)
    -- (paginatedReader rsDyn fullScrEv <$> docEv)
    (verticalReader rsDyn fullScrEv <$> docEv)
  return (closeEv, never)

-- Display the complete document in one page
readingPaneView :: AppMonad t m
  => (ReaderDocument CurrentDb)
  -> AppMonadT t m ()
readingPaneView (ReaderDocument _ title annText _) = do
  fontSizeDD <- dropdown 100 (constDyn fontSizeOptions) def
  rubySizeDD <- dropdown 120 (constDyn fontSizeOptions) def
  lineHeightDD <- dropdown 150 (constDyn lineHeightOptions) def
  let divAttr = (\s l -> "style" =: ("font-size: " <> tshow s <>"%;"
        <> "line-height: " <> tshow l <> "%;"))
        <$> (value fontSizeDD) <*> (value lineHeightDD)

  rec
    let
      -- vIdEv :: Event t ([VocabId], Text)
      vIdEv = leftmost $ V.toList vIdEvs

    vIdDyn <- holdDyn [] (fmap fst vIdEv)

    vIdEvs <- elDynAttr "div" divAttr $ do
      rec
        (resEv,v) <- resizeDetector $ do
          el "h3" $ text title
          V.mapM (renderOnePara vIdDyn (value rubySizeDD)) annText
      return v

  showVocabDetailsWidget vIdEv
  return ()


----------------------------------------------------------------------------------
-- Vertical rendering

data TextAdjustDirection = ForwardGrow | BackwardGrow
  deriving (Show, Eq)

newtype ParaPos = ParaPos { unParaPos :: Int }
  deriving (Show, Eq, Ord, Ix, Num)

newtype ParaNum = ParaNum { unParaNum :: Int }
  deriving (Show, Eq, Ord, Ix, Num)

type ParaData = Array ParaNum (Array ParaPos (Either Text (Vocab, [VocabId], Bool)))

verticalReader :: forall t m . AppMonad t m
  => Dynamic t (ReaderSettings CurrentDb)
  -> Event t ()
  -> (ReaderDocumentData)
  -> AppMonadT t m ()
verticalReader rs fullScrEv (docId, title, startParaMaybe, endParaNum, annText) = do
  (evVisible, action) <- newTriggerEvent

  visDyn <- holdDyn (0,Nothing) evVisible
  display visDyn

  let
    divAttr' = (\s l h fs -> ("style" =:
      ("font-size: " <> tshow s <>"%;"
        <> "line-height: " <> tshow l <> "%;"
        <> "height: " <> (if fs then "100%;" else tshow h <> "px;")
        <> "width: " <> (if fs then "100%;" else "80vw;")
        <> "writing-mode: vertical-rl;"
        <> "word-wrap: break-word;"
        -- <> (if fs then "position: fixed;" else "")
        <> "display: block;" <> "padding: 40px;"))
           <> ("class" =: (if fs then "modal modal-content" else "")))
      <$> (_fontSize <$> rs) <*> (_lineHeight <$> rs)
      <*> (_numOfLines <$> rs)

    startPara = (\(p,v) -> (ParaNum p, ParaPos $ maybe 1 (max 1) v)) startParaMaybe
    initState = (1, (ForwardGrow,
                 (getCurrentViewContent (makeParaData annText) (Just startPara))))
    dEv = snd <$> (evVisible)

  text $ tshow startParaMaybe
  rec
    let doStop (Just GrowText) Nothing = Just 1
        doStop (Just GrowText) (Just c) = Just (c + 1)
        doStop (Just ShrinkText) _ = Nothing
        doStop Nothing _ = Nothing
        stopTicks = traceEvent "stop" $ leftmost [stopTicks1
                             , fforMaybe evVisible (\(_,a) -> if isNothing a
                                                     then Just ()
                                                     else Nothing)
                             ]

        stopTicks1 = fforMaybe (tag (current stopTicksMaybe) evVisible) $ \case
          Nothing -> Nothing
          (Just c) -> if c > 20 then Just () else Nothing

    stopTicksMaybe <- foldDyn doStop Nothing $
      leftmost [Nothing <$ stopTicks,(snd <$> evVisible)]

  --------------------------------
  rec
    let
      pageChangeEv = leftmost [BackwardGrow <$ prev, ForwardGrow <$ next]

      firstDisplayedPara :: Dynamic t (ParaNum, ParaPos)
      firstDisplayedPara = ffor row1Dyn $ \(_,ps) ->
        (fst $ A.bounds ps, fst $ A.bounds $ (ps A.! (fst $ A.bounds ps)))

      lastDisplayedPara :: Dynamic t (ParaNum, ParaPos)
      lastDisplayedPara = ffor row1Dyn $ \(_,ps) ->
        (snd $ A.bounds ps, snd $ A.bounds $ (ps A.! (snd $ A.bounds ps)))

      nextParaMaybe = getNextParaMaybe <$> lastDisplayedPara <*> textContent
      prevParaMaybe = getPrevParaMaybe <$> firstDisplayedPara <*> textContent

      row1Len :: Dynamic t Int
      row1Len = ffor row1Dyn $ \(_,ps) ->
        foldl (\len (l,u) -> len + (unParaPos (u - l))) 0 (map A.bounds $ A.elems ps)

    let foldF (len, (d, tc)) =
          foldDyn f st ((\d -> (tc,d)) <$> dEv)
          where f = case d of
                  ForwardGrow -> textAdjustF
                  BackwardGrow -> textAdjustRevF
                st = getState tc len d

        newStateEv = attachDyn row1Len $ attachWith af (current
          ((,) <$> (getCurrentViewContent <$> textContent <*> nextParaMaybe)
               <*> (getPrevViewContent <$> textContent <*> prevParaMaybe)))
                     pageChangeEv
          where
            af (c,_) ForwardGrow = (ForwardGrow,c)
            af (_,c) BackwardGrow = (BackwardGrow,c)

        row1Dyn :: Dynamic t ((ParaPos, ParaPos), ParaData)
        row1Dyn = join row1Dyn'

    (row1Dyn') <- widgetHold (foldF initState) (foldF <$> newStateEv)

    display row1Len
    text "row1Dyn fst :"
    display $ (\(ParaPos l, ParaPos u) -> ("(" <> tshow l <> "," <> tshow u <> ")"))
      . fst <$> row1Dyn
    textContent <- fetchMoreContentF docId (makeParaData annText) endParaNum
      (traceEvent "fetchEv: " (attachDyn firstDisplayedPara pageChangeEv))

    let
      wrapDynAttr = ffor fullscreenDyn $ \b -> if b
        then ("style" =: "position: fixed; padding 1em; top: 0; bottom: 0; left: 0; right: 0;")
        else Map.empty
      divAttr = divAttr' <*> fullscreenDyn

    -- text "firstDisplayedPara:"
    -- display firstDisplayedPara
    -- text " lastDisplayedPara:"
    -- display lastDisplayedPara
    -- text " nextParaMaybe:"
    -- display nextParaMaybe
    -- text " prevParaMaybe:"
    -- display prevParaMaybe

    fullscreenDyn <- holdDyn False (leftmost [ True <$ fullScrEv
                                           , False <$ closeEv])

    --------------------------------
    (resizeEv, (rowRoot, (inside, outside, vIdEv, closeEv))) <- resizeDetector $ do
      rec
        let
          closeEv = tup ^. _2 . _4
          dispFullScr m = do
            dyn ((\fs -> if fs then m else return ()) <$> fullscreenDyn)

        tup <- elDynAttr "div" wrapDynAttr $ elDynAttr' "div" divAttr $ do
          closeEv <- do
            (e,_) <- elClass' "button" "close" $ do
              dispFullScr (text "Close")
            return (domEvent Click e)

          vIdEv <- el "div" $ do
            renderDynParas rs (snd <$> row1Dyn)

          (inside, _) <- elAttr' "div" ("style" =: "height: 1em; width: 1rem;") $ do
            text ""
          elAttr "div" ("style" =: "height: 1em; width: 2em;") $ do
            text ""
          (outside, _) <- elAttr' "div" ("style" =: "height: 1em; width: 2em;") $ do
            text ""
            return ()
          return (inside, outside, vIdEv, closeEv)
      return tup

    (next,prev) <- leftRightButtons fullscreenDyn nextParaMaybe prevParaMaybe stopTicks


  showVocabDetailsWidget vIdEv

  firstParaDyn <- holdUniqDyn firstDisplayedPara

  getWebSocketResponse
    ((\(ParaNum p, ParaPos o) -> SaveReadingProgress docId (p,Just o))
      <$> updated firstParaDyn)

  --------------------------------

  let inEl = _element_raw inside
      outEl = _element_raw outside

  time <- liftIO $ getCurrentTime

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
              de <- delay 1 $ leftmost [ev, () <$ evVisible]
              return $ de
        t <- widgetHold init
          (init <$ startTicksAgain)
        return (switchPromptlyDyn $ join t)

  tickEv <- ticksWidget

  performEvent (checkVerticalOverflow (inEl, outEl)
                (_element_raw rowRoot) action <$ tickEv)

  return ()

fetchMoreContentF :: (AppMonad t m)
  => _
  -> ParaData
  -> Int
  -> Event t ((ParaNum, ParaPos), TextAdjustDirection)
  -> AppMonadT t m (Dynamic t ParaData)
fetchMoreContentF docId annText endParaNum pageChangeEv = do
  rec
    -- Fetch more contents
    -- Keep at most 120 paras in memory
    let

        lastAvailablePara = (snd . A.bounds) <$> textContent
        firstAvailablePara = (fst . A.bounds) <$> textContent
        hitEndEv = fmapMaybe hitEndF (attachDyn lastAvailablePara pageChangeEv)
        hitEndF (ParaNum l,((ParaNum n,_), d))
          | l < endParaNum && d == ForwardGrow && (l - n < 20) = Just (l + 1)
          | otherwise = Nothing
        hitStartEv = fmapMaybe hitStartF (attachDyn firstAvailablePara pageChangeEv)
        hitStartF (ParaNum f,((ParaNum n,_), d))
          | f > 0 && d == BackwardGrow && (n - f < 20) = Just (max 0 (f - 60))
          | otherwise = Nothing

    moreContentEv <- getWebSocketResponse $
      (\p -> ViewDocument docId (Just p)) <$> (leftmost [hitEndEv, hitStartEv])

    textContent <- foldDyn moreContentAccF annText ((\(_,_,_,_,c) -> c) <$>
                                    (fmapMaybe identity moreContentEv))

  text "("
  display lastAvailablePara
  text ", "
  display firstAvailablePara
  text ")"

  return textContent

makeParaData :: [(Int, AnnotatedPara)] -> ParaData
makeParaData [] = A.listArray (ParaNum 0, ParaNum (-1)) []
makeParaData n@(n1:_) = A.array (ParaNum (fst n1), ParaNum (fst l)) $ map f n
  where
    f (n,c) = (ParaNum n, A.listArray (ParaPos 1, ParaPos (length c)) c)
    (l:_) = reverse n

moreContentAccF :: [(Int, AnnotatedPara)] -> ParaData -> ParaData
moreContentAccF [] o = o
moreContentAccF n@(n1:_) o = A.array newBounds ((A.assocs pd) ++ (A.assocs o))
  where
    (curFirst, curLast) = A.bounds o
    pd = makeParaData n
    (newFirst, newLast) = A.bounds pd
    newBounds = if newFirst > curLast
      then ((max curFirst (ParaNum $ 60 - (unParaNum newLast))) , newLast)
      else (newFirst, (min curLast (ParaNum $ 60 + (unParaNum newFirst))))

getState
  :: ParaData
  -> Int
  -> _
  -> ((ParaPos, ParaPos), ParaData)
getState tc len d = (A.bounds (snd lp), pd)
  where
    pd = A.array (minimum pns, maximum pns) ps
    pns = map fst ps
    ps = (lp:rp)
    (lp:rp) = reverse $ case d of
      ForwardGrow -> recF len tcL
      BackwardGrow -> recF len tcU

    recF :: Int -> ParaNum -> [(ParaNum, Array ParaPos _)]
    recF l n = case d of
      ForwardGrow
        | nl > l -> [(n, A.ixmap (lb, lb + (ParaPos l)) identity nd)]
        | n < tcU -> (n,nd) : recF (l - nl) (n + 1)
        | otherwise -> [(n,nd)]
      BackwardGrow
        | nl > l -> [(n, A.ixmap (ub - (ParaPos l), ub) identity nd)]
        | n > tcL -> (n,nd) : recF (l - nl) (n - 1)
        | otherwise -> [(n,nd)]
      where
        nd = tc A.! n
        nl = unParaPos $ ub - lb
        (lb,ub) = (A.bounds nd)

    (tcL, tcU) = A.bounds tc

getCurrentViewContent
  :: ParaData
  -> Maybe (ParaNum, ParaPos)
  -> ParaData
getCurrentViewContent pd Nothing = pd
getCurrentViewContent pd (Just (p,o)) = (A.ixmap newBounds identity pd) A.// [(p,newP)]
  where
    newBounds = A.bounds pd & _1 .~ p
    op = pd A.! p
    newP = A.ixmap (A.bounds op & _1 .~ o) identity op

-- Offsets are the Current start of page
getPrevViewContent
  :: ParaData
  -> Maybe (ParaNum, ParaPos)
  -> ParaData
getPrevViewContent pd Nothing = pd
getPrevViewContent pd (Just (p,o)) = (A.ixmap newBounds identity pd) A.// [(p,newP)]
  where
    newBounds = A.bounds pd & _2 .~ p
    op = pd A.! p
    newP = A.ixmap (A.bounds op & _2 .~ o) identity op

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

-- On click of left or right button hide both
-- wait for stopTicks, then show the button again if the next/prev value is Just
-- Change the button size if full screen
leftRightButtons fullscreenDyn nextParaMaybe prevParaMaybe stopTicks = do

  let
    btnCommonAttr stl vis = (\fs -> ("class" =: "btn btn-xs")
       <> ("style" =: ("height: " <> tshow (if fs then 80 else 60) <> "%;\
            \top: "<> tshow (if fs then 10 else 30) <> "%; width: 20px;"
            <> visV <>
            "position: absolute; z-index: 1060;"
          <> stl ))) <$> fullscreenDyn
       where
         visV = if vis then "" else "display: none;"

    leftBtnAttr vis = btnCommonAttr "left: 10px;" =<< vis
    rightBtnAttr vis = btnCommonAttr "right: 10px;" =<< vis

  -- Buttons
  rec
    let btnPress = leftmost [next,prev]
    leftBtnVis <- holdDyn (False) $
      leftmost [tagDyn (isJust <$> nextParaMaybe) stopTicks
               , False <$ btnPress]
    rightBtnVis <- holdDyn (False) $
      leftmost [tagDyn (isJust <$> prevParaMaybe) stopTicks
               , False <$ btnPress]

    prev <- do
      (e,_) <- elDynAttr' "button" (rightBtnAttr rightBtnVis) $ text ">"
      return (domEvent Click e)
    next <- do
      (e,_) <- elDynAttr' "button" (leftBtnAttr leftBtnVis) $ text "<"
      return (domEvent Click e)

  return (next,prev)

data TextAdjust = ShrinkText | GrowText
  deriving (Show, Eq)


halfParaPos (ParaPos u) (ParaPos l)
  = ParaPos $ ceiling $ (fromIntegral (u - l) / 2)

-- Converge on the text content size based on Events
-- The Input events will toggle between shrink and grow
-- This is equivalent to binary space search.
-- Keep track of low and upper bound
-- lower bound causes Grow event, upper bound causes Shrink event
-- Do binary search between these bounds
-- When a resize occurs (ie event goes Nothing -> Just)
-- The bounds will have to be re-calculated

textAdjustF, textAdjustRevF
  :: (ParaData, Maybe TextAdjust)
  -> ((ParaPos, ParaPos), ParaData)
  -> ((ParaPos, ParaPos), ParaData)

textAdjustF (viewPD, (Just ShrinkText)) ((li,ui), ps)
  | lpU == lpL
    -- drop the last Para
    = assert (lpN > fpN)
    ((A.bounds $ ps A.! (lpN - 1))
      , A.ixmap (fpN, lpN - 1) identity ps)

  -- During viewport size change li can become invalid
  | li >= lpU
    = textAdjustF (viewPD, Just ShrinkText) ((lpL, lpU), ps)
  | otherwise
    -- drop half
    = ((li, lpU), ps A.// [(lpN, newLp)])
  where
    (fpN,lpN) = A.bounds ps
    lp = ps A.! lpN
    (lpL,lpU) = A.bounds lp
    halfL = lpU - halfParaPos lpU  li
    newLp = A.ixmap (lpL, halfL) identity lp


textAdjustF (viewPD, (Just GrowText)) ((li,ui), ps)
  | lpU < ui = assert (ui <= lpOTU)
    ((lpU, ui), ps A.// [(lpN, A.ixmap (lpL, halfL1) identity lpOT)])
  | lpU < lpOTU -- ui is invalid
    = ((lpU, lpOTU), ps A.// [(lpN, A.ixmap (lpL, halfL2) identity lpOT)])
  | nextPN <= (snd $ A.bounds viewPD) =
    (A.bounds nextP, A.ixmap (fpN, nextPN) identity viewPD)
  | otherwise = textAdjustF (viewPD, Nothing) ((li,ui), ps)
  where
    (fpN,lpN) = A.bounds ps
    lp = ps A.! lpN
    (lpL,lpU) = A.bounds lp
    lpOT = viewPD A.! lpN
    (_,lpOTU) = A.bounds lpOT
    nextPN = (lpN + 1)
    nextP = viewPD A.! nextPN

    halfL1 = lpU + halfParaPos ui lpU
    halfL2 = lpU + halfParaPos lpOTU lpU

textAdjustF (viewPD, Nothing) (_, ps) = (A.bounds lp,ps)
  where
    (fpN,lpN) = A.bounds ps
    lp = viewPD A.! lpN

-- lower bound is towards end, upper bound is towards start
-- Do binary search between these bounds
textAdjustRevF (viewPD, (Just ShrinkText)) ((li,ui), ps)
  | fpL == fpU
    -- drop the first Para
    = assert (lpN > fpN)
    ((A.bounds $ ps A.! (fpN + 1))
      , A.ixmap (fpN + 1, lpN) identity ps)

  -- During viewport size change ui can become invalid
  | ui <= fpL
    = textAdjustRevF (viewPD, Just ShrinkText) ((fpL, fpU), ps)
  | otherwise
    -- drop half
    = ((fpL, ui), ps A.// [(fpN, newFp)])
  where
    (fpN,lpN) = A.bounds ps
    fp = ps A.! fpN
    (fpL,fpU) = A.bounds fp
    -- During viewport size change li can be invalid
    halfL = fpL + halfParaPos ui fpL
    newFp = A.ixmap (halfL, fpU) identity fp


textAdjustRevF (viewPD, (Just GrowText)) ((li,ui), ps)
  | fpL > li = assert (li >= fpOTL)
    ((li, fpL), ps A.// [(fpN, A.ixmap (halfL1, fpU) identity fpOT)])
  | fpL > fpOTL -- li is invalid
    = ((fpOTL, ui), ps A.// [(fpN, A.ixmap (halfL2, fpU) identity fpOT)])
  | prevPN >= (snd $ A.bounds viewPD) =
    (A.bounds prevP, A.ixmap (prevPN, lpN) identity viewPD)
  | otherwise = textAdjustRevF (viewPD, Nothing) ((li,ui), ps)
  where
    (fpN,lpN) = A.bounds ps
    fp = ps A.! fpN
    (fpL,fpU) = A.bounds fp
    fpOT = viewPD A.! fpN
    (fpOTL,_) = A.bounds fpOT
    prevPN = (fpN - 1)
    prevP = viewPD A.! prevPN

    halfL1 = fpL - (halfParaPos fpL li)
    halfL2 = fpL - (halfParaPos fpL fpOTL)

textAdjustRevF (viewPD, Nothing) (_, ps) = (A.bounds fp,ps)
  where
    (fpN,lpN) = A.bounds ps
    fp = viewPD A.! fpN

renderDynParas :: (_)
  => Dynamic t (ReaderSettings CurrentDb) -- Used for mark
  -> Dynamic t ParaData
  -> m (Event t ([VocabId], (Text, Maybe e)))
renderDynParas rs dynParas = do
  let dynMap = (Map.fromList . A.assocs) <$> dynParas
      renderF vIdDyn = renderOnePara vIdDyn (_rubySize <$> rs)
      renderEachPara vIdDyn dt = do
        ev <- dyn ((\p -> renderF vIdDyn (A.elems p)) <$> dt)
        switchPromptly never ev

  rec
    let
      vIdEv = switchPromptlyDyn $ (fmap (leftmost . Map.elems)) v
    v <- list dynMap (renderEachPara vIdDyn)
    vIdDyn <- holdDyn [] (fmap fst vIdEv)
  return (vIdEv)

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
