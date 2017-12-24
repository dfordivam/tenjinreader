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

checkOverFlow e overFlowThreshold = do
  rect <- DOM.getBoundingClientRect (_element_raw e)
  trects <- DOM.getClientRects (_element_raw e)
  y <- DOM.getY rect
  h <- DOM.getHeight rect
  -- text $ "Coords: " <> (tshow y) <> ", " <> (tshow h)
  return (y + h > overFlowThreshold)

setupInterObs :: (DOM.MonadDOM m)
  => Int
  -> ((Int, Bool) -> IO ())
  -> m DOM.IntersectionObserver
setupInterObs ind action = do
  cb <- DOM.newIntersectionObserverCallback
    (intersectionObsCallback ind action)
  DOM.newIntersectionObserver cb Nothing

intersectionObsCallback ind action [] _ = return ()
intersectionObsCallback ind action (e:es) _ = do
  r <- DOM.getIntersectionRatio e
  if (r > 0.9)
    then liftIO $ action (ind, True)
    else liftIO $ action (ind, False)


readingPane :: AppMonad t m
  => Event t ReaderDocument
  -> AppMonadT t m (Event t (), Event t ReaderDocument)
readingPane docEv = do
  closeEv <- button "Close"
  editEv <- button "Edit"
  rdDyn <- holdDyn Nothing (Just <$> docEv)
  widgetHold (return ())
    -- (readingPaneView <$> docEv)
    (paginatedReader <$> docEv)
  return (closeEv
         , fmapMaybe identity $ tagDyn rdDyn editEv)

readingPaneView :: AppMonad t m
  => ReaderDocument
  -> AppMonadT t m ()
readingPaneView (ReaderDocument _ title annText) = do
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

-- Auto paginate text
--   - Split large para into different pages
--   - Small para move to next page
-- Forward and backward page turn buttons
-- Jump to page
-- variable height / content on page
-- store the page number (or para number) and restore progress
-- Bookmarks
paginatedReader :: forall t m . AppMonad t m
  => ReaderDocument
  -> AppMonadT t m ()
paginatedReader (ReaderDocument _ title annText) = do
  fontSizeDD <- dropdown 100 (constDyn fontSizeOptions) def
  rubySizeDD <- dropdown 120 (constDyn fontSizeOptions) def
  lineHeightDD <- dropdown 150 (constDyn lineHeightOptions) def
  -- render one para then see check its height

  let divAttr = (\s l -> "style" =:
        ("font-size: " <> tshow s <>"%;"
          <> "line-height: " <> tshow l <> "%;"
          <> "height: 400;"))
        <$> (value fontSizeDD) <*> (value lineHeightDD)

  rec
    let
      overFlowThreshold = 400

      renderParaNum paraNum resizeEv = do
        let para = annText V.!? paraNum
        case para of
          Nothing -> text "--- End of Text ---" >> return (never, never)
          (Just p) -> renderPara p paraNum resizeEv

      renderPara para paraNum resizeEv = do
        (e,v1) <- el' "div" $
          renderOnePara vIdDyn (value rubySizeDD) paraNum para

        ev <- delay 0.2 =<< getPostBuild
        overFlowEv <- holdUniqDyn
          =<< widgetHold (checkOverFlow e overFlowThreshold)
          (checkOverFlow e overFlowThreshold
             <$ (leftmost [ev,resizeEv]))
        -- display overFlowEv

        let
          nextParaWidget b = if b
            then do
               ev <- button "Next Page"
               return ((paraNum + 1) <$ ev, never)
            else renderParaNum (paraNum + 1) resizeEv

        v2 <- widgetHold (nextParaWidget False)
              (nextParaWidget <$> updated overFlowEv)
        return $ (\(a,b) -> (a, leftmost [v1,b]))
          (switchPromptlyDyn $ fst <$> v2
          , switchPromptlyDyn $ snd <$> v2)

      renderFromPara :: (_) => Int
        -> AppMonadT t m (Event t Int, Event t ([VocabId], Text))
      renderFromPara startPara = do
        rec
          (resizeEv,v) <- resizeDetector $ elDynAttr "div" divAttr $
            renderParaNum startPara resizeEv
        return v

      bwdRenderParaNum paraNum e = do
        let para = annText V.!? paraNum
        case para of
          Nothing -> return (constDyn 0)
          (Just p) -> bwdRenderPara p paraNum e


      bwdRenderPara para paraNum e = do
        ev <- delay 0.1 =<< getPostBuild
        overFlowEv <- holdUniqDyn
          =<< widgetHold (return True)
          (checkOverFlow e overFlowThreshold <$ ev)

        let
          prevParaWidget b = if b
            then return (constDyn paraNum)
            else bwdRenderParaNum (paraNum - 1) e

        v2 <- widgetHold (prevParaWidget True)
              (prevParaWidget <$> updated overFlowEv)

        el "div" $
          renderOnePara vIdDyn (value rubySizeDD) paraNum para

        return $ join v2

      getFirstParaOfPrevPage :: (_)
        => Event t Int
        -> AppMonadT t m (Event t Int)
      getFirstParaOfPrevPage endParaEv = do
        rec
          let
            init endPara = do
              rec
                (e,v) <- elDynAttr' "div" divAttr $
                  bwdRenderParaNum endPara e
              return v -- First Para

            getParaDyn endPara = do
              widgetHold (init endPara)
                ((return (constDyn 0)) <$ delEv)

          delEv <- delay 1 endParaEv
        pDyn <- widgetHold (return (constDyn (constDyn 0)))
          (getParaDyn <$> endParaEv)
        return (tagDyn (join $ join pDyn) delEv)

    vIdDyn <- holdDyn [] (fmap fst vIdEv)

    vIdEv <- do
      rec
        let
          newPageEv :: Event t Int
          newPageEv = leftmost [switchPromptlyDyn (fst <$> val), firstPara]
        firstParaDyn <- holdDyn 0 newPageEv

        prev <- button "Previous Page"
        firstPara <- (getFirstParaOfPrevPage
          ((\p -> max 0 (p - 1)) <$> tagDyn firstParaDyn prev))

        val <- widgetHold (renderFromPara 0)
          (renderFromPara <$> newPageEv)
      return $ switchPromptlyDyn (snd <$> val)

  divClass "" $ do
    detailsEv <- getWebSocketResponse $ GetVocabDetails
      <$> (fmap fst vIdEv)
    surfDyn <- holdDyn "" (fmap snd vIdEv)
    showVocabDetailsWidget (attachDyn surfDyn detailsEv)
  return ()
-- Algo
-- Start of page
  -- (ParaId, Maybe Offset) -- (Int , Maybe Int)

-- How to determine the
-- End of page
  -- (ParaId, Maybe Offset)

-- Get the bounding rect of each para
-- if Y + Height > Div Height then para overflows
-- Show the para in next page

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
