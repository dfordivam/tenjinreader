{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}

module FrontendCommon
  ( module FrontendCommon
  , module X
  , module DOM
  )
  where

import Message as X
import Common as X
import Radicals as X

import Protolude as X hiding (link, (&), list, Alt, to)
import Control.Lens as X ((.~), (^.), (?~), (^?), (%~), _1, _2, _3, _4, sets, each
  , _head, _Just, view, over, views, preview, (^..), to, mapped, forMOf_)
import Control.Monad.Fix as X

import Reflex.Dom as X
import Reflex.Dom.WebSocket.Monad as X
import Reflex.Dom.WebSocket.Message as X
import Reflex.Time as X (delay)
import Data.Time.Clock as X
import Data.Time.Calendar as X
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.List as List
import Data.These as X
import Data.Align as X

import Language.Javascript.JSaddle as X (call, eval)
import qualified GHCJS.DOM.DOMRectReadOnly as DOM
import qualified GHCJS.DOM.Element as DOM
import qualified GHCJS.DOM.Document as DOM
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Types as DOM
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)

--
type AppMonadT t m = WithWebSocketT AppRequest t m
type AppMonad t m = (MonadWidget t m, DOM.MonadJSM m)

handleVisibility
  :: (DomBuilder t m, PostBuild t m, Eq a)
  => a -> Dynamic t a -> m v -> m v
handleVisibility v dv mv = elDynAttr "div" (f <$> dv) mv
  where
    f dv =
      if v == dv
        then Map.empty
        else ("style" =: "display: none;")

tshow :: (Show a) => a -> Text
tshow = (T.pack . show)

displayVocabT :: DomBuilder t m => Vocab -> m ()
displayVocabT (Vocab ks) = do
  let
    f (Kana k) = text k
    f (KanjiWithReading (Kanji k) r)
      = el "ruby" $ do
          text k
          el "rt" $ text r
  mapM_ f ks

btn :: (_) => Text -> Text -> m (Event t ())
btn cl t = do
  (e,_) <- elClass' "button" ("btn " <> cl) $ text t
  return $ domEvent Click e

-- Controls to add/edit related srs items
addEditSrsEntryWidget :: AppMonad t m
  => (Either KanjiId VocabId)
  -> Maybe Text -- Surface
  -> Maybe SrsEntryId
  -> AppMonadT t m ()
addEditSrsEntryWidget i t s = do
  let
    widget s = case s of
      (Just sId) -> do
        ev <- btn "btn-xs btn-primary" "Edit SRS"
        openEditSrsItemWidget (sId <$ ev)
        return never

      (Nothing) -> do
        ev <- btn "btn-xs btn-primary" "Add to SRS"
        resp <- getWebSocketResponse $ QuickAddSrsItem i t <$ ev
        showWSProcessing ev resp
        return resp
  rec
    sDyn <- holdDyn s resp
    resp <- switchPromptly never
      =<< (dyn $ widget <$> sDyn)

  return ()

openEditSrsItemWidget
  :: (AppMonad t m)
  => Event t (SrsEntryId)
  -> AppMonadT t m (Event t (SrsEntryId, SrsEntry))
openEditSrsItemWidget ev = do
  srsItEv <- getWebSocketResponse $ GetSrsItem <$> ev
  showWSProcessing ev srsItEv

  let
      modalWidget :: (AppMonad t m)
        => Maybe (SrsEntryId, SrsEntry)
        -> AppMonadT t m (Event t (SrsEntryId, SrsEntry))
      modalWidget (Just s) = do
        rec
          d <- widgetHold (editWidget s)
            ((return (never,never)) <$ switchPromptlyDyn (fst <$> d))
        return (switchPromptlyDyn $ snd <$> d)

      modalWidget Nothing = do
        return never

      editWidget :: AppMonad t m
        => (SrsEntryId, SrsEntry)
        -> AppMonadT t m (Event t (), Event t (SrsEntryId, SrsEntry))
      editWidget (sId, s) = do
        rec
          (sNew, saveEv, closeEv) <- editWidgetView s ev
          let sEv = tagDyn sNew saveEv
          ev <- getWebSocketResponse $ EditSrsItem sId <$> sEv
        return (closeEv, (,) sId <$> sEv)

  switchPromptlyDyn
    <$> widgetHold (return never) (modalWidget <$> srsItEv)

modalDiv m = do
  divClass "modal-backdrop fade in" $ return ()
  elAttr "div" attr $ elAttr "div" attr2
    $ divClass "modal-content" m
  where attr = ("class" =: "modal")
          <> ("style" =: "display: block;")
        attr2 = ("style" =: "width 90vw; height 90vh;\
                            \ max-width: 40em;\
                            \ margin: 30px auto;")

editWidgetView
  :: MonadWidget t m
  => SrsEntry
  -> Event t ()
  -> m (Dynamic t SrsEntry
       , Event t (), Event t ())
editWidgetView s savedEv = modalDiv $ do
  closeEvTop <- divClass "modal-header" $ el "h3" $ do
    (e,_) <- elClass' "button" "close" $ text "Close"
    text $ "Edit " <> (s ^. field . to (NE.head))
    return (domEvent Click e)

  let bodyAttr = ("class" =: "modal-body")
          <> ("style" =: "height: 400px;\
              \overflow-y: auto")
      formAttr = ("class" =: "form-horizontal")
        <> ("onsubmit" =: "return false;")
      bodyForm m = elAttr "div" bodyAttr $
        elAttr "form" formAttr m

  ret <- bodyForm $ do

    f <- divClass "form-group" $ do
      elClass "label" "control-label col-sm-1" $
        text "Field:"
      editNonEmptyList (s ^. field) identity
        $ \t x -> do
             text t
             e <- x
             text ", "
             return e

    r <- divClass "form-group" $ do
      elClass "label" "control-label col-sm-2" $
        text "Reading:"
      editNonEmptyList (s ^. readings) Reading
        $ \t x -> do
             text $ unReading t
             e <- x
             text ", "
             return e

    -- m <- elAttr "div" (("class" =: "form-group")
    --       <> ("style" =: "height: 100px;\
    --           \overflow-y: scroll")) $ do
    m <- divClass "form-group" $ do
      elClass "label" "control-label col-sm-2" $
        text "Meanings:"
      editNonEmptyList (s ^. meaning) Meaning
        $ \t x -> do
             text $ unMeaning t
             e <- x
             text ", "
             return e

    rn <- divClass "form-group" $ do
      elClass "label" "control-label col-sm-2" $
        text "Reading Notes:"
      divClass "" $
        textArea $ def &
          textAreaConfig_initialValue .~
            (maybe "" unReadingNotes
             $ s ^. readingNotes)

    mn <- divClass "form-group" $ do
      elClass "label" "control-label col-sm-2" $
        text "Meaning Notes:"
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
              <*> f

  divClass "modal-footer" $ do
    let savedIcon = elClass "i" "" $ return ()
    saveEv <- btn "btn-primary" "Save"
    closeEv <- btn "btn-default" "Close"
    showWSProcessing saveEv savedEv
    widgetHold (return ()) (savedIcon <$ savedEv)
    return (ret, saveEv, leftmost[closeEv, closeEvTop])


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
      addEv = attachDyn newKeyDyn (tagDyn (Just . conT <$> value ti) enterPress)
      remEv1 = switchPromptlyDyn $
        (leftmost . (fmap snd) . Map.elems) <$> d
      remEv = fmapMaybe g (attachDyn d remEv1)
      g (m,e) = if Map.size m > 1
        then Just e
        else Nothing
      newKeyDyn = ((+ 1) . fst . Map.findMax) <$> d

      enterPress = ffilter (==13) (ti ^. textInput_keypress) -- 13 -> Enter
      tiAttr = constDyn $ ("style" =: "width: 100%;")

    ti <- divClass "col-sm-2" $ textInput $ def
      & textInputConfig_attributes .~ tiAttr
      & textInputConfig_setValue .~ ("" <$ enterPress)
    d <- divClass "col-sm-6" $
      elClass "p" "form-control-static" $
        listHoldWithKey initMap remAddEv showItem

  return $ (NE.fromList . (fmap fst) . Map.elems) <$> d

showWSProcessing :: (_)
  => Event t a
  -> Event t b
  -> m ()
showWSProcessing evStart evFinish = do
  let
    showSpinner = divClass "spinner" $ do
      divClass "bounce1" $ return ()
      divClass "bounce2" $ return ()
      divClass "bounce3" $ return ()

  let waitForStart = do
        void $ widgetHold (showSpinner)
          (return () <$ evFinish)
  void $ widgetHold (return ())
    (waitForStart <$ evStart)

openSentenceWidget :: AppMonad t m
  => (Text, [Text])
  -> Event t (Either VocabId SrsEntryId)
  -> AppMonadT t m ()
openSentenceWidget header open = do
  resp <- getWebSocketResponse $ GetVocabSentences <$> open
  showWSProcessing open resp

  widgetHoldWithRemoveAfterEvent (sentenceWidgetView header <$> resp)
  return ()

sentenceWidgetView :: AppMonad t m
  => (Text, [Text])
  -> ([VocabId], [SentenceData], [(NonJpSentenceId,Text)])
  -> AppMonadT t m (Event t ())
sentenceWidgetView (surface, meanings) (vIds, ss, njps) = modalDiv $ do
  closeEvTop <- divClass "modal-header" $ do
    text surface
    text " : "
    let m = mconcat $ intersperse ", " $ meanings
    text $ if T.length m > 40
      then (T.take 40 m) <> "..."
      else m
    (e,_) <- elClass' "button" "close" $ text "Close"
    return (domEvent Click e)

  let bodyAttr = ("class" =: "modal-body")
          <> ("style" =: "height: 80vh;\
              \overflow-y: auto")
      sMap :: Map SentenceId SentenceData
      sMap = Map.fromList $ map (\sd -> (sd ^. sentenceId,sd)) ss
      njpMap = Map.fromList njps
      sentenceGroups :: [([SentenceData],[Text])]
      sentenceGroups = over (each . _1 . each) (view _2) $
        ff  $ map (\sd -> (sd ^. sentenceId,sd)) ss
      ff [] = []
      ff (s:[]) = [([s], njs)]
        where
          njs = catMaybes $ map (\k -> Map.lookup k njpMap) $ s ^. _2 . sentenceLinkedEng
      ff (s:sss) = (s:linked, njs) : ff ss2
        where
          linked = map (\sd -> (sd ^. sentenceId,sd)) $
            catMaybes $ map (\i -> List.lookup i sss) $
            s ^. _2 . sentenceLinkedJp
          ss2 = List.deleteFirstsBy (\a b -> fst a == fst b) sss linked
          njs = catMaybes $ map (\k -> Map.lookup k njpMap) $ s ^. _2 . sentenceLinkedEng

  vIdEvs <- elAttr "div" bodyAttr $ do
    forM sentenceGroups $ \(sg, njps) -> do
      evs <- divClass "" $ forM sg $ \s -> do
        renderOnePara (constDyn vIds) (constDyn 100) (s ^. sentenceContents)
      divClass "" $ forM njps $ \t -> do
        el "p" $ text t
      return evs

  let vIdEv = leftmost $ concat vIdEvs

  divClass "" $ do
    detailsEv <- getWebSocketResponse $ GetVocabDetails
      <$> (fmap fst vIdEv)
    surfDyn <- holdDyn ("", Nothing) (fmap snd vIdEv)
    showVocabDetailsWidget (attachDyn surfDyn detailsEv)

  return closeEvTop

vocabRuby :: (_)
  => Dynamic t Bool
  -> Dynamic t Int
  -> Dynamic t Bool
  -> Vocab -> m (_)
vocabRuby markDyn fontSizePctDyn visDyn v@(Vocab ks) = do
  let
    attr = ffor markDyn $ \b -> if b then ("class" =: "highlight-word") else Map.empty
    rubyAttr = (\s -> "style" =: ("font-size: " <> tshow s <> "%;")) <$> fontSizePctDyn
    g r True = r
    g _ _ = ""
    f (Kana k) = text k
    f (KanjiWithReading (Kanji k) r)
      = elDynAttr "ruby" rubyAttr $ do
          text k
          el "rt" $ dynText (g r <$> visDyn)

  (e,_) <- elDynAttr' "span" attr $ mapM f ks
  return (Just $ _element_raw e
         , domEvent Click e
         , domEvent Mouseenter e
         , domEvent Mouseleave e)

renderOnePara :: (_)
  => Dynamic t [VocabId] -- Used for mark
  -> Dynamic t Int
  -> [Either Text (Vocab, [VocabId], Bool)]
  -> m (Event t ([VocabId], (Text,_)))
renderOnePara vIdDyn rubySize annTextPara = do
  let showAllFurigana = constDyn True
  el "p" $ do
    let f (Left t) = never <$ text t
        f (Right (v, vId, vis)) = do
          rec
            let evVis = leftmost [True <$ eme, tagDyn showAllFurigana eml]
                markDyn = (any (\eId -> (elem eId vId))) <$> vIdDyn
            visDyn <- holdDyn vis evVis
            (e, ek, eme, eml) <-
              vocabRuby markDyn rubySize visDyn v
          return $ (vId, (vocabToText v, e)) <$ ek

    leftmost <$> mapM f (annTextPara)

showVocabDetailsWidget :: forall t m e . (AppMonad t m, DOM.IsElement e)
  => Event t ((Text, Maybe e) , [(Entry, Maybe SrsEntryId)])
  -> AppMonadT t m ()
showVocabDetailsWidget detailsEv = do
  let

    attrBack = ("class" =: "modal")
          <> ("style" =: "display: block;\
              \opacity: 0%; z-index: 1050;")
    attrFront y h
      | h > 300 && y < 300 = f "navbar-fixed-top" -- Should be middle?
      | y > 300 = f "navbar-fixed-top"
      | otherwise = f "navbar-fixed-bottom"
      where f p = ("class" =: ("nav " <> p))
              <> ("style" =: "z-index: 1060;\
                         \padding: 10px;")

    wrapper :: (_) => _ -> AppMonadT t m a -> AppMonadT t m (Event t ())
    wrapper e m = do
      (y,h) <- case e of
        Nothing -> return (0,0)
        (Just e) -> DOM.liftJSM $ do
          rect <- DOM.getBoundingClientRect (e)
          y <- DOM.getY rect
          h <- DOM.getHeight rect
          return (y,h)
      (e1,_) <- elAttr' "div" attrBack $ return ()
      elAttr "div" (attrFront y h) $
        divClass "container-fluid" $
          elAttr "div" (("class" =: "panel panel-default")
            <> ("style" =: "max-height: 200px;\
                           \overflow-y: auto;\
                           \overflow-x: hidden;\
                           \padding: 15px;")) $ do
            (e,_) <- elClass' "button" "close" $ text "Close"
            -- text $ ("(" <> tshow y <> "," <> tshow h <> ")")
            m
            return $ leftmost
              [domEvent Click e
              , domEvent Click e1]

    wd :: AppMonad t m
      => Maybe ((Text, Maybe e) , [(Entry, Maybe SrsEntryId)])
      -> AppMonadT t m (Event t ())
    wd (Just ((s,e),es)) = (wrapper e)
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
    openEv <- btn "btn-xs btn-primary" "Sentences"
    openSentenceWidget (surface, e ^.. entrySenses . traverse .
                         senseGlosses . traverse . glossDefinition)
      ((Left $ e ^. entryUniqueId) <$ openEv)

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

widgetHoldWithRemoveAfterEvent :: (_)
  => Event t (m (Event t a))
  -> m (Event t a)
widgetHoldWithRemoveAfterEvent w = do
  let
    w1 w = do
      rec
        let ev = switchPromptlyDyn evDyn
        evDyn <- widgetHold (w)
          (return never <$ ev)
      return ev
  evDyn <- widgetHold (return never)
    (w1 <$> w)
  return $ switchPromptlyDyn evDyn
