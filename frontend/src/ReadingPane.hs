{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
module ReadingPane where

import FrontendCommon

import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector as V
import qualified GHCJS.DOM.DOMRectReadOnly as DOM
import qualified GHCJS.DOM.Element as DOM
import qualified GHCJS.DOM.Document as DOM
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.IntersectionObserverEntry as DOM hiding (getBoundingClientRect)
import qualified GHCJS.DOM.IntersectionObserverCallback as DOM
import qualified GHCJS.DOM.IntersectionObserver as DOM
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

setupInterObs :: (DOM.MonadDOM m)
  => Int
  -> _
  -> ((Int, Bool) -> IO ())
  -> m DOM.IntersectionObserver
setupInterObs ind options action = do
  cb <- DOM.newIntersectionObserverCallback
    (intersectionObsCallback ind action)
  DOM.newIntersectionObserver cb (Just options)

intersectionObsCallback ind action [] _ = return ()
intersectionObsCallback ind action (e:es) _ = do
  r <- DOM.getIntersectionRatio e
  if (r > 0.9)
    then liftIO $ action (ind, True)
    else liftIO $ action (ind, False)

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

readingPaneInt :: AppMonad t m
  => Event t (ReaderDocumentData)
  -> ReaderSettings CurrentDb
  -> AppMonadT t m (Event t (), Event t (ReaderDocument CurrentDb))
readingPaneInt docEv rsDef = do
  closeEv <- btn "btn-default" "Close"
  -- editEv <- btn "btn-default" "Edit"

  fontSizeDD <- dropdown (rsDef ^. fontSize) (constDyn fontSizeOptions) def
  rubySizeDD <- dropdown (rsDef ^. rubySize) (constDyn fontSizeOptions) def
  lineHeightDD <- dropdown (rsDef ^. lineHeight) (constDyn lineHeightOptions) def
  writingModeDD <- dropdown (rsDef ^. verticalMode) (constDyn writingModeOptions) def
  heightDD <- dropdown (rsDef ^. numOfLines) (constDyn numOfLinesOptions) def
  let rsDyn = ReaderSettings <$> (value fontSizeDD) <*> (value rubySizeDD)
                <*> (value lineHeightDD) <*> (value writingModeDD)
                <*> (value heightDD)
  getWebSocketResponse (SaveReaderSettings <$> (updated rsDyn))


  widgetHold ((text "waiting for document data"))
    -- (readingPaneView <$> docEv)
    -- (paginatedReader rsDyn <$> docEv)
    (verticalReader rsDyn <$> docEv)
  -- rdDyn <- holdDyn Nothing (Just <$> docEv)
  return (closeEv, never)
         -- , fmapMaybe identity $ tagDyn rdDyn editEv)

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
          V.imapM (renderOnePara vIdDyn (value rubySizeDD)) annText
      return v

  divClass "" $ do
    detailsEv <- getWebSocketResponse $ GetVocabDetails
      <$> (fmap fst vIdEv)
    surfDyn <- holdDyn "" (fmap snd vIdEv)
    showVocabDetailsWidget (attachDyn surfDyn detailsEv)
  return ()

-- Remove itself on prev page click

renderParaWrap ::
     forall t m b . (AppMonad t m)
  => Dynamic t (ReaderSettingsTree CurrentDb)
  -> Event t b
  -> Dynamic t [VocabId]
  -> Dynamic t [(Int,AnnotatedPara)]
  -> (AppMonadT t m () -> AppMonadT t m (Event t ()))
  -> Dynamic t (Map Text Text)
  -> Int
  -> AppMonadT t m (Dynamic t ( (Event t (), Event t ())
                              , (Event t Int, Event t ([VocabId], Text))))

renderParaWrap rs prev vIdDyn textContent dispFullScr divAttr paraNum =
  widgetHold (renderFromPara paraNum)
    ((return nVal) <$ prev)
  where
    nVal = ((never,never), (never, never))

    renderParaNum 0 paraNum resizeEv = return (never,never)
    renderParaNum paraCount paraNum resizeEv = do
      cntnt <- sample $ current textContent
      let para = List.lookup paraNum cntnt
      case para of
        Nothing -> text "--- End of Text ---" >> return (never, never)
        (Just p) -> renderPara paraCount p paraNum resizeEv

    renderPara paraCount para paraNum resizeEv = do
      (e,v1) <- el' "div" $
        renderOnePara vIdDyn (_rubySize <$> rs) paraNum para

      ev <- delay 0.2 =<< getPostBuild
      overFlowEv <- holdUniqDyn
        =<< widgetHold (checkOverFlow e (_numOfLines <$> rs))
        (checkOverFlow e (_numOfLines <$> rs)
           <$ (leftmost [ev,resizeEv]))
      -- display overFlowEv

      let
        nextParaWidget b = if b
          then do
             (e,_) <- elAttr' "button" nextBtnAttr $ text ">"
             return ((paraNum + 1) <$ domEvent Click e, never)
          else renderParaNum (paraCount - 1) (paraNum + 1) resizeEv

      v2 <- widgetHold (nextParaWidget False)
            (nextParaWidget <$> updated overFlowEv)
      return $ (\(a,b) -> (a, leftmost [v1,b]))
        (switchPromptlyDyn $ fst <$> v2
        , switchPromptlyDyn $ snd <$> v2)

    btnCommonAttr stl = ("class" =: "btn btn-xs")
       <> ("style" =: ("height: 80%; top: 10%; width: 20px; position: absolute;"
          <> stl ))
    prevBtnAttr = btnCommonAttr "left: 10px;"
    nextBtnAttr = btnCommonAttr "right: 10px;"
    renderFromPara :: (_) => Int
      -> AppMonadT t m ((Event t () -- Close Full Screen
                       , Event t ()) -- Previous Page
      , (Event t Int, Event t ([VocabId], Text)))
    renderFromPara startPara = do
      rec
        (resizeEv,v) <- resizeDetector $ elDynAttr "div" divAttr $ do
          (e,_) <- elClass' "button" "close" $
            dispFullScr (text "Close")
          prev <- if startPara == 0
            then return never
            else do
              (e,_) <- elAttr' "button" prevBtnAttr $ text "<"
              return (domEvent Click e)
          v1 <- renderParaNum 20 startPara resizeEv
          return ((domEvent Click e, prev), v1)
      return v


-- Auto paginate text
--   - Split large para into different pages
--   - Small para move to next page
-- Forward and backward page turn buttons
-- Jump to page
-- variable height / content on page
-- store the page number (or para number) and restore progress
-- Bookmarks
paginatedReader :: forall t m . AppMonad t m
  => Dynamic t (ReaderSettings CurrentDb)
  -> (ReaderDocumentData)
  -> AppMonadT t m ()
paginatedReader rs (docId, title, (startPara, _), annText) = do
  fullScrEv <- btn "btn-default" "Full Screen"
  -- render one para then see check its height

  rec
    let
      dispFullScr m = do
        dyn ((\fs -> if fs then m else return ()) <$> fullscreenDyn)

      divAttr = (\s l h fs -> ("style" =:
        ("font-size: " <> tshow s <>"%;"
          <> "line-height: " <> tshow l <> "%;"
          -- <> "height: " <> tshow h <> "px;"
          <> (if fs then "position: fixed;" else "")
          <> "display: block;" <> "padding: 40px;"))
             <> ("class" =: (if fs then "modal modal-content" else "")))
        <$> (_fontSize <$> rs) <*> (_lineHeight <$> rs)
        <*> (_numOfLines <$> rs) <*> (fullscreenDyn)


      vIdEv = switchPromptlyDyn (snd . snd <$> val)
      fullScrCloseEv = switchPromptlyDyn (fst . fst <$> val)

      val = join valDDyn
      newPageEv :: Event t Int
      newPageEv = leftmost [switchPromptlyDyn (fst . snd <$> val), firstPara]

    vIdDyn <- holdDyn [] (fmap fst vIdEv)
    fullscreenDyn <- holdDyn False (leftmost [ True <$ fullScrEv
                                             , False <$ fullScrCloseEv])

    firstParaDyn <- holdDyn startPara newPageEv
    let lastAvailablePara = ((\(p:_) -> fst p) . reverse) <$> textContent
        firstAvailablePara = ((\(p:_) -> fst p)) <$> textContent
        hitEndEv = fmapMaybe hitEndF (attachDyn lastAvailablePara newPageEv)
        hitEndF (l,n)
          | l - n < 10 = Just (l + 1)
          | otherwise = Nothing
        hitStartEv = fmapMaybe hitStartF (attachDyn firstAvailablePara firstPara)
        hitStartF (f,n)
          | n - f < 10 = Just (max 0 (f - 30))
          | otherwise = Nothing

    moreContentEv <- getWebSocketResponse $
      (\p -> ViewDocument docId (Just p)) <$> (leftmost [hitEndEv, hitStartEv])

    display firstParaDyn
    text ", "
    display lastAvailablePara
    text ", "
    display (length <$> textContent)
    -- Keep at most 60 paras in memory, length n == 30
    let
        accF :: [(Int, AnnotatedPara)] -> [(Int, AnnotatedPara)] -> [(Int, AnnotatedPara)]
        accF [] o = o
        accF n@(n1:_) o@(o1:_)
          | (fst n1) > (fst o1) = (drop (length o - 30) o) ++ n -- More forward content
          | otherwise = n ++ (take 30 o) -- More previous paras
    textContent <- foldDyn accF annText ((\(_,_,_,c) -> c) <$>
                                    (fmapMaybe identity moreContentEv))

    -- Temporary render to find firstPara
    let prev = switchPromptlyDyn (snd . fst <$> val)
    firstPara <- (getFirstParaOfPrevPage rs prev vIdDyn textContent dispFullScr divAttr
      ((\p -> max 0 (p - 1)) <$> tagDyn firstParaDyn prev))

    let renderParaF = renderParaWrap rs prev vIdDyn textContent dispFullScr divAttr
    -- Render Actual content
    valDDyn <- widgetHold (renderParaF startPara)
      (renderParaF <$> newPageEv)

  divClass "" $ do
    detailsEv <- getWebSocketResponse $ GetVocabDetails
      <$> (fmap fst vIdEv)
    surfDyn <- holdDyn "" (fmap snd vIdEv)
    showVocabDetailsWidget (attachDyn surfDyn detailsEv)

  getWebSocketResponse ((\p -> SaveReadingProgress docId (p,Nothing)) <$> newPageEv)
  return ()


getFirstParaOfPrevPage ::
     forall t m b . (AppMonad t m)
  => Dynamic t (ReaderSettingsTree CurrentDb)
  -> Event t b
  -> Dynamic t [VocabId]
  -> Dynamic t [(Int,AnnotatedPara)]
  -> (AppMonadT t m () -> AppMonadT t m (Event t ()))
  -> Dynamic t (Map Text Text)
  -> Event t Int
  -> AppMonadT t m (Event t Int)
getFirstParaOfPrevPage rs prev vIdDyn textContent dispFullScr divAttr endParaEv = do
  rec
    let
      init endPara = do
        elDynAttr "div" divAttr $ do
          rec
            (e,v) <- el' "div" $
              bwdRenderParaNum 20 endPara e
          return v -- First Para

      -- Get para num and remove self
      getParaDyn endPara = do
        widgetHold (init endPara)
          ((return (constDyn 0)) <$ delEv)

    delEv <- delay 2 endParaEv
  pDyn <- widgetHold (return (constDyn (constDyn 0)))
    (getParaDyn <$> endParaEv)
  return (tagDyn (join $ join pDyn) delEv)
  where
    bwdRenderParaNum 0 paraNum e = return (constDyn paraNum)
    bwdRenderParaNum paraCount paraNum e = do
      cntnt <- sample $ current textContent
      let para = List.lookup paraNum cntnt
      case para of
        Nothing -> return (constDyn 0)
        (Just p) -> bwdRenderPara paraCount p paraNum e


    bwdRenderPara paraCount para paraNum e = do
      ev <- delay 0.1 =<< getPostBuild
      overFlowEv <- holdUniqDyn
        =<< widgetHold (return True)
        (checkOverFlow e (_numOfLines <$> rs) <$ ev)

      let
        prevParaWidget b = if b
          then return (constDyn paraNum)
          else bwdRenderParaNum (paraCount - 1) (paraNum - 1) e

      v2 <- widgetHold (prevParaWidget True)
            (prevParaWidget <$> updated overFlowEv)

      el "div" $
        renderOnePara vIdDyn (_rubySize <$> rs) paraNum para

      return $ join v2

-- Algo
-- Start of page
  -- (ParaId, Maybe Offset) -- (Int , Maybe Int)

-- How to determine the
-- End of page
  -- (ParaId, Maybe Offset)

-- Get the bounding rect of each para
-- if Y + Height > Div Height then para overflows
-- Show the para in next page

----------------------------------------------------------------------------------
-- Vertical rendering


verticalReader :: forall t m . AppMonad t m
  => Dynamic t (ReaderSettings CurrentDb)
  -> (ReaderDocumentData)
  -> AppMonadT t m ()
verticalReader rs (docId, title, (startPara, _), annText) = do
  -- fullScrEv <- btn "btn-default" "Full Screen"

  (evVisible, action) <- newTriggerEvent

  rec
    let
      fullscreenDyn = constDyn False
      divAttr = (\s l h fs -> ("style" =:
        ("font-size: " <> tshow s <>"%;"
          <> "line-height: " <> tshow l <> "%;"
          <> "height: " <> tshow h <> "px;"
          <> "width: " <> tshow h <> "px;"
          <> "writing-mode: vertical-rl;"
          <> (if fs then "position: fixed;" else "")
          <> "display: block;" <> "padding: 40px;"))
             <> ("class" =: (if fs then "modal modal-content" else "")))
        <$> (_fontSize <$> rs) <*> (_lineHeight <$> rs)
        <*> (_numOfLines <$> rs) <*> (fullscreenDyn)

    (rowRoot, clash) <- elDynAttr' "div" divAttr $ do

      let para = maybe [] identity $ List.lookup paraNum annText
          vIdDyn = constDyn []
          paraNum = 0
      el "div" $
        renderOnePara vIdDyn (_rubySize <$> rs) paraNum para

      (clash, _) <- elAttr' "div" ("style" =: "height: 1px;") $ return ()
      return (clash)

  --------------------------------

  v <- liftJSM $ do
    o <- create
    m <- toJSVal (0 :: Double)
    t <- toJSVal (1 :: Double)
    r <- toJSVal (_element_raw rowRoot)
    setProp "root" r o
    setProp "margin" m o
    setProp "threshold" t o
    toJSVal (ValObject o)

  io <- setupInterObs 0 (DOM.IntersectionObserverInit v) action
  DOM.observe io (_element_raw clash)

  d <- holdDyn (0,False) evVisible
  display d

  return ()
----------------------------------------------------------------------------------
vocabRuby :: (_)
  => Dynamic t Bool
  -> Dynamic t Int
  -> Dynamic t Bool
  -> Vocab -> m (_)
vocabRuby markDyn fontSizePctDyn visDyn v@(Vocab ks) = do
  let
    spClass = ffor markDyn $ \b -> if b then "mark" else ""
    rubyAttr = (\s -> "style" =: ("font-size: " <> tshow s <> "%;")) <$> fontSizePctDyn
    g r True = r
    g _ _ = ""
    f (Kana k) = text k
    f (KanjiWithReading (Kanji k) r)
      = elDynAttr "ruby" rubyAttr $ do
          text k
          el "rt" $ dynText (g r <$> visDyn)
  (e,_) <- elDynClass' "span" spClass $ mapM f ks
  return $ (domEvent Click e, domEvent Mouseenter e, domEvent Mouseleave e)

lineHeightOptions = Map.fromList $ (\x -> (x, (tshow x) <> "%"))
  <$> ([100,150..400]  :: [Int])

fontSizeOptions = Map.fromList $ (\x -> (x, (tshow x) <> "%"))
  <$> ([80,85..200]  :: [Int])

writingModeOptions = Map.fromList $
  [(False, "Horizontal" :: Text)
  , (True, "Vertical")]

numOfLinesOptions = Map.fromList $ (\x -> (x, (tshow x) <> "px"))
  <$> ([100,150..2000]  :: [Int])

renderOnePara :: (_)
  => Dynamic t [VocabId] -- Used for mark
  -> Dynamic t Int
  -> Int
  -> [Either Text (Vocab, [VocabId], Bool)]
  -> m (Event t ([VocabId], Text))
renderOnePara vIdDyn rubySize ind annTextPara = do
  let showAllFurigana = constDyn True
  el "p" $ do
    let f (Left t) = never <$ text t
        f (Right (v, vId, vis)) = do
          rec
            let evVis = leftmost [True <$ eme, tagDyn showAllFurigana eml]
                markDyn = (any (\eId -> (elem eId vId))) <$> vIdDyn
            visDyn <- holdDyn vis evVis
            (ek, eme, eml) <-
              vocabRuby markDyn rubySize visDyn v
          return $ (vId, vocabToText v) <$ ek
        -- onlyKana (Vocab ks) = (flip all) ks $ \case
        --   (Kana _) -> True
        --   _ -> False
        -- addSpace [] = []
        -- addSpace (l@(Left _):r@(Right _):rs) =
        --   l : (Left "　") : (addSpace (r:rs))
        -- addSpace (r1@(Right (v1,_,_)):r2@(Right _):rs)
        --   | onlyKana v1 = r1 : (Left "　") : (addSpace (r2:rs))
        --   | otherwise = r1:(addSpace (r2:rs))
        -- addSpace (r:rs) = r : (addSpace rs)

    leftmost <$> mapM f (annTextPara)

showVocabDetailsWidget :: (AppMonad t m)
  => Event t (Text, [(Entry, Maybe SrsEntryId)])
  -> AppMonadT t m ()
showVocabDetailsWidget detailsEv = do
  let

    attrBack = ("class" =: "modal")
          <> ("style" =: "display: block;\
              \opacity: 0%; z-index: 1050;")
    attrFront = ("class" =: "nav navbar-fixed-bottom")
          <> ("style" =: "z-index: 1060;\
                         \padding: 10px;")

    wrapper :: (_) => m a -> m (Event t ())
    wrapper m = do
      (e1,_) <- elAttr' "div" attrBack $ return ()
      elAttr "div" attrFront $
        divClass "container-fluid" $
          elAttr "div" (("class" =: "panel panel-default")
            <> ("style" =: "max-height: 200px;\
                           \overflow-y: auto;\
                           \padding: 15px;")) $ do
            (e,_) <- elClass' "button" "close" $ text "Close"
            m
            return $ leftmost
              [domEvent Click e
              , domEvent Click e1]

    wd :: AppMonad t m
      => Maybe _
      -> AppMonadT t m (Event t ())
    wd (Just (s,es)) = wrapper
      (mapM_ (showEntry s) (orderEntries (fst) es))
    wd Nothing = return never

  rec
    let ev = leftmost [Just <$> detailsEv
             , Nothing <$ (switchPromptlyDyn closeEv)]
    closeEv <- widgetHold (return never)
      (wd <$> ev)

  return ()

showEntry surface (e, sId) = do
  divClass "" $ do
    elClass "span" "" $ do
      entryKanjiAndReading surface e
    addEditSrsEntryWidget (Right $ e ^. entryUniqueId) (Just surface) sId

  let
    showGlosses ms = mapM_ text $ intersperse ", " $
      map (\m -> T.unwords $ T.words m & _head  %~ capitalize)
      ms
    showInfo [] = return ()
    showInfo is = do
      mapM_ text $ ["("] ++ (intersperse ", " is) ++ [")"]
    showSense s = divClass "" $ do
      showPos $ s ^.. sensePartOfSpeech . traverse
      showInfo $ s ^.. senseInfo . traverse
      showGlosses $ take 5 $ s ^.. senseGlosses . traverse . glossDefinition

  divClass "" $ do
    mapM showSense $ take 3 $ e ^.. entrySenses . traverse

capitalize t
  | T.head t == ('-') = t
  | elem t ignoreList = t
  | otherwise = T.toTitle t
  where ignoreList = ["to"]

showPos ps = do
  elClass "span" "small" $ do
    mapM_ text $ p $ (intersperse ", ") psDesc
  where
    p [] = []
    p c = ["("] ++ c ++ [") "]
    psDesc = catMaybes $ map f ps
    f PosNoun = Just $ "Noun"
    f PosPronoun = Just $ "Pronoun"
    f (PosVerb _ _) = Just $ "Verb"
    f (PosAdverb _) = Just $ "Adv."
    f (PosAdjective _) = Just $ "Adj."
    f PosSuffix = Just $ "Suffix"
    f PosPrefix = Just $ "Prefix"
    f _ = Nothing

entryKanjiAndReading :: (_) => Text -> Entry -> m ()
entryKanjiAndReading surface e = do
  sequenceA_ (intersperse sep els)
  where
  els = map (renderElement surface (restrictedKanjiPhrases e)
    (e ^. entryReadingElements . to (NE.head) . readingPhrase))
    (orderElements e)
  sep = text ", "

restrictedKanjiPhrases :: Entry
  -> Map KanjiPhrase ReadingElement
restrictedKanjiPhrases e = Map.fromList $ concat $
  e ^.. entryReadingElements . traverse
    . to (\re -> re ^.. readingRestrictKanji . traverse
           . to (\kp -> (kp, re)))

-- Priority of entries
-- Entry with priority elements
-- Entry normal
-- Entry with Info elements
orderEntries :: (a -> Entry) -> [a] -> [a]
orderEntries g es = sortBy (comparing (f . g)) es
  where
    f e
      | any (not . null) $
        (ke ^.. traverse . kanjiPriority) ++
        (re ^.. traverse . readingPriority)
        = 1
      | any (not . null)
        (ke ^.. traverse . kanjiInfo) ||
        any (not . null)
        (re ^.. traverse . readingInfo)
        = 3
      | otherwise = 2
      where
        ke = e ^. entryKanjiElements
        re = e ^. entryReadingElements

-- Priority of elements
-- Kanji with priority
-- Reading with priority
-- Kanji with reading
-- Kanji With restricted reading
-- Reading
-- Kanji with Info
-- Reading with Info
orderElements
  :: Entry
  -> [(Either KanjiElement ReadingElement)]
orderElements e = sortBy (comparing f)
  ((e ^.. entryKanjiElements . traverse . to (Left)) ++
  readingWithoutRes)

  where
    f (Left ke)
      | (ke ^. kanjiPriority . to (not . null)) = 1
      | (ke ^. kanjiInfo . to (not . null)) = 6
      | Map.member (ke ^. kanjiPhrase)
        (restrictedKanjiPhrases e) = 4
      | otherwise = 3

    f (Right re)
      | (re ^. readingPriority . to (not . null)) = 2
      | (re ^. readingInfo . to (not . null)) = 7
      | otherwise = 5

    readingWithoutRes = map Right $
      filter (view $ readingRestrictKanji . to (null)) $
      (e ^.. entryReadingElements . traverse)

renderElement :: (_)
  => Text
  -> Map KanjiPhrase ReadingElement
  -> ReadingPhrase
  -> (Either KanjiElement ReadingElement)
  -> m ()
renderElement surface restMap defR (Left ke) = case v of
  (Right v) -> dispInSpan (vocabToText v) $ displayVocabT v
  (Left _) ->
    (\t -> dispInSpan t $ text t) $ unKanjiPhrase $ ke ^. kanjiPhrase
  where
    dispInSpan t = el (spanAttr t)
    spanAttr t = if (T.isPrefixOf surface t) then "strong" else "span"
    kp = (ke ^. kanjiPhrase)
    v = case Map.lookup kp restMap of
          (Just r) -> makeFurigana kp (r ^. readingPhrase)
          Nothing -> makeFurigana kp defR

renderElement surface _ _ (Right re) =
  (\t -> dispInSpan t $ text t) $ unReadingPhrase $ re ^. readingPhrase
  where
    dispInSpan t = el (spanAttr t)
    spanAttr t = if (T.isPrefixOf surface t) then "strong" else "span"
