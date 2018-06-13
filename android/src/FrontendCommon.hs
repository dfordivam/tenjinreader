{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

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
import Control.Lens.Indexed
import Control.Monad.Fix as X

import Reflex.Dom as X
import Reflex.Dom.WebSocket.Monad as X
import Reflex.Dom.WebSocket.Message as X
import Reflex.Time as X (delay)
import Data.Time.Clock as X
import Data.Time.Calendar as X
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.These as X
import Data.Align as X

import Language.Javascript.JSaddle as X (call, eval)
import qualified GHCJS.DOM.DOMRectReadOnly as DOM
import qualified GHCJS.DOM.Element as DOM
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

filterOnEq :: (Eq a, Functor f, FunctorMaybe f) => f a -> a -> f ()
filterOnEq ev v = fmapMaybe identity $ ffor ev
  (\b -> if b == v then Just () else Nothing)

displayVocabT :: DomBuilder t m => Vocab -> m ()
displayVocabT (Vocab ks) = do
  let
    f (Kana k) = text k
    f (KanjiWithReading (Kanji k) r)
      = el "ruby" $ do
          text k
          el "rt" $ text r
  mapM_ f ks

icon t = do
  (e,_) <- elClass' "button" "button" $
    elAttr "span" (("class" =: "icon")) $
      elClass "i" ("fa " <> t) $ return ()
  return $ domEvent Click e

btn :: (DomBuilder t m)
  => Text
  -> Text
  -> m (Event t ())
btn cl t = do
  (e,_) <- elClass' "button" ("button " <> cl) $ text t
  return $ domEvent Click e

btnLoading :: (_)
  => Text
  -> Text
  -> Event t a
  -> m (Event t ())
btnLoading cl t doneEv = do
  let cl1 = cl <> " button"
  rec
    clDyn <- holdDyn cl1 (leftmost [cl1 <$ doneEv
                                  , (cl1 <> " is-loading") <$ ev])
    ev <- do
      (e,_) <- elDynClass' "button" clDyn $ text t
      return $ domEvent Click e
  return ev

-- Controls to add/edit related srs items
addEditSrsEntryWidget :: AppMonad t m
  => (Either KanjiId VocabId)
  -> Maybe Text -- Surface
  -> VocabSrsState
  -> AppMonadT t m ()
addEditSrsEntryWidget i t s = do
  let
    widget = \case
      (InSrs sId) -> do
        ev <- btn "" "Edit SRS"
        _ <- openEditSrsItemWidget (sId <$ ev)
        return never

      (IsWakaru) -> do
        rec
          ev <- btnLoading "" "わからない" resp
          resp <- getWebSocketResponse $ QuickToggleWakaru i <$ ev
        return resp

      (NotInSrs) -> do
        rec
          ev <- btnLoading "" "Add to SRS" resp
          resp <- getWebSocketResponse $ QuickAddSrsItem i t <$ ev
          ev2 <- btnLoading "" "わかる" resp2
          resp2 <- getWebSocketResponse $ QuickToggleWakaru i <$ ev2
        return $ leftmost [resp,resp2]
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
            ((return (never,never)) <$ (switch . current) (fst <$> d))
        return ((switch . current) $ snd <$> d)

      modalWidget Nothing = do
        return never

      editWidget :: AppMonad t m
        => (SrsEntryId, SrsEntry)
        -> AppMonadT t m (Event t (), Event t (SrsEntryId, SrsEntry))
      editWidget (sId, s) = do
        rec
          (sNew, saveEv, closeEv) <- editWidgetView s ev
          let sEv = tag (current sNew) saveEv
          ev <- getWebSocketResponse $ EditSrsItem sId <$> sEv
        return (closeEv, (,) sId <$> sEv)

  (switch . current)
    <$> widgetHold (return never) (modalWidget <$> srsItEv)

modalDiv :: DomBuilder t m => m b -> m b
modalDiv m = do
  elAttr "div" attr $ do
    divClass "modal-background" $ return ()
    elAttr "div" attr2 $ divClass "modal-card" m
  where attr = ("class" =: "modal")
          <> ("style" =: "display: block;")
  -- unset max-height to reverse bulma css
        attr2 = ("style" =: "width: 90vw; height: 96vh;\
                            \ max-width: 40em;\
                            \ max-height: unset;\
                            \ overflow-y: auto;\
                            \ overflow-y: auto;\
                            \ margin: 2vh auto;")
                <> ("class" =: "modal-content")

editWidgetView
  :: MonadWidget t m
  => SrsEntry
  -> Event t ()
  -> m (Dynamic t SrsEntry
       , Event t (), Event t ())
editWidgetView s savedEv = modalDiv $ do
  closeEvTop <- elClass "header" "modal-card-head" $ do
    (e,_) <- elClass' "button" "delete" $ return ()
    divClass "column is-1" $ return ()
    elClass "p" "modal-card-title" $
      text $ "Edit " <> (s ^. field . to (NE.head))
    return (domEvent Click e)

  let bodyAttr = ("class" =: "modal-card-body")
          <> ("style" =: "")
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

  divClass "modal-card-foot" $ do
    let savedIcon = elClass "span" "icon" $
          elClass "i" "fa fa-check-circle" $ return ()
    saveEv <- btnLoading "is-success" "Save" savedEv
    closeEv <- btn "" "Close"
    _ <- widgetHold (return ()) (savedIcon <$ savedEv)
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
      addEv = attach (current newKeyDyn) (tag (current $ Just . conT <$> value ti) enterPress)
      remEv1 = (switch . current) $
        (leftmost . (fmap snd) . Map.elems) <$> d
      remEv = fmapMaybe g (attach (current d) remEv1)
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

showWSProcessing :: (MonadHold t m, DomBuilder t m)
  => Event t a
  -> Event t b
  -> m ()
showWSProcessing evStart evFinish = do
  let
    showSpinner = elClass "a" "button is-loading" $ return ()
      -- divClass "bounce1" $ return ()
      -- divClass "bounce2" $ return ()
      -- divClass "bounce3" $ return ()

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
  -> ([VocabId], [((Bool, SentenceId), SentenceData)])
  -> AppMonadT t m (Event t ())
sentenceWidgetView (surface, meanings) (vIds, ss) = modalDiv $ do
  (headElm, closeEvTop) <- elClass' "header" "modal-card-head" $ do
    (e,_) <- elClass' "button" "delete" $ return ()
    divClass "column is-1" $ return ()
    elClass "p" "modal-card-title" $ do
      text surface
      text " : "
      let m = mconcat $ intersperse ", " $ meanings
      text $ if T.length m > 40
        then (T.take 40 m) <> "..."
        else m
    return (domEvent Click e)

  let bodyAttr = ("class" =: "modal-card-body")
          <> ("style" =: "")

      fg ls = Map.fromList $ ls & each . _2 %~ Just

  (cl2, vIdEv) <- elAttr "div" bodyAttr $ do
    rec
      vIdMap <- listHoldWithKey (Map.fromList ss) addMoreEv $
        renderOneSentence vIds

      (addMoreEv, closeBot) <- divClass "columns is-mobile" $ do
        rec
          loadMoreEv <- divClass "column" $ do
            btnLoading "" "Load More" addMoreEv1
          addMoreEv1 <- fmap fg <$> getWebSocketResponse
            (LoadMoreSentences vIds <$> tag (current $ (map snd) . Map.keys <$> vIdMap) loadMoreEv)
        divClass "column" $ do
          topEv <- btn "btn-block btn-primary" "Top"
          performEvent_ $ topEv $> DOM.scrollIntoView (_element_raw headElm) True
        closeBot1 <- divClass "column" $ do
          btn "btn-block btn-primary" "Close"
        return (addMoreEv1, closeBot1)

    return $ (closeBot
      , (switch . current) ((leftmost . concat . Map.elems) <$> vIdMap))

  showVocabDetailsWidget vIdEv
  return (leftmost [closeEvTop, cl2])

-- The notFav in key puts the favourite sentences first
renderOneSentence
  :: AppMonad t m
  => [VocabId]
  -> (Bool, SentenceId)
  -> SentenceData
  -> AppMonadT t m [Event t ([VocabId], (Text, Maybe (RawElement (DomBuilderSpace m))))]
renderOneSentence vIds (notFav, sId) (SentenceData sg njps) = divClass "box" $ do
  let hasEng = not $ null njps
      rowAttr = ("class" =: "columns") <> ("style" =: "width: 100%;")
  (evs, visDyn, showPlainEv) <- elAttr "div" rowAttr $ do
    evs <- divClass "column" $
      forM sg $ \s -> do
        renderOnePara (constDyn (vIds,[])) (constDyn 100) s

    rec
      (visDyn, showPlainEv) <- divClass "column is-narrow" $ do
        rec
          isFav <- toggle (not notFav) tEv
          tEv <- switchPromptly never
            =<< (dyn ((\f -> icon
                        (if f then "fa-ban" else "fa-bookmark")) <$> isFav))
          _ <- getWebSocketResponse $ ToggleSentenceFav sId <$ tEv

        bEv <- icon "fa-clipboard"

        d <- if hasEng
          then toggle False
            =<< (btn "btn-xs btn-primary" "意味")
          else return $ constDyn False
        return (d,bEv)

    return (evs,visDyn, showPlainEv)

  widgetHold (return ())
    ((divClass "" $ showPlainForm sg) <$ showPlainEv)

  when hasEng $ void $ handleVisibility True visDyn $
    forM njps $ \t -> el "p" $ text t
  return $ NE.toList evs

showPlainForm :: (_) => t AnnotatedPara -> m ()
showPlainForm = mapM_ $ \s -> do
  textInput $ def & textInputConfig_initialValue .~ (plainText s)
  where
    plainText :: AnnotatedPara -> Text
    plainText = mconcat . (map (f))
    f (Left t) = t
    f (Right ((Vocab vs),_,_)) = mconcat $ map g vs
    g (Kana k) = k
    g (KanjiWithReading (Kanji k) r) = k <> "《" <> r <> "》"

vocabRuby :: (_)
  => Dynamic t (Maybe Bool)
  -> Dynamic t Int
  -> Dynamic t Bool
  -> Vocab
  -> m (Maybe (RawElement (DomBuilderSpace m)), Event t1 (),
         Event t2 (), Event t3 ())
vocabRuby markDyn fontSizePctDyn visDyn (Vocab ks) = do
  let
    attr = ffor markDyn $ \b -> case b of
      Nothing -> Map.empty
      (Just True) -> ("class" =: "has-background-warning")
      (Just False) -> ("class" =: "has-background-grey-lighter")

    rubyAttr = (\s -> "style" =: ("font-size: " <> tshow s <> "%;")) <$> fontSizePctDyn
    g r True = r
    g _ _ = ""
    f (Kana k) = text k
    f (KanjiWithReading (Kanji k) r)
      = elDynAttr "ruby" rubyAttr $ do
          text k
          elDynAttr "rt" attr $ dynText (g r <$> visDyn)

  (e,_) <- elDynAttr' "span" attr $ mapM f ks
  return (Just $ _element_raw e
         , domEvent Click e
         , domEvent Mouseenter e
         , domEvent Mouseleave e)

renderOnePara
  :: (MonadFix m,
       MonadHold t m,
       PostBuild t m,
       DomBuilder t m)
  => Dynamic t ([VocabId], [VocabId]) -- Used for mark
  -> Dynamic t Int
  -> [Either Text (Vocab, [VocabId], Bool)]
  -> m (Event t ([VocabId], (Text, Maybe (RawElement (DomBuilderSpace m)))))
renderOnePara vIdDyn rSizeDyn annTextPara = do
  -- let showAllFurigana = constDyn False
  el "p" $ do
    let f (Left t) = never <$ text t
        f (Right (v, vId, vis)) = do
          rec
            let evVis = leftmost [True <$ eme, vis <$ eml]
                markDyn = ffor vIdDyn $ \(a,b) ->
                  if any (\eId -> (elem eId vId)) a
                    then Just True
                    else if any (\eId -> (elem eId vId)) b
                      then Just False
                      else Nothing

            visDyn <- holdDyn vis evVis
            (e, ek, eme, eml) <-
              vocabRuby markDyn rSizeDyn visDyn v
          return $ (vId, (vocabToText v, e)) <$ ek

    leftmost <$> mapM f (annTextPara)

showVocabDetailsWidget :: forall t m e . (AppMonad t m, DOM.IsElement e)
  => Event t ([VocabId], (Text, Maybe e))
  -> AppMonadT t m ()
showVocabDetailsWidget vIdEv = divClass "" $ do
  let

    attrBack = ("class" =: "modal")
          <> ("style" =: "display: block;\
              \opacity: 0%; z-index: 1050;")
    attrFront y h
      | h > 300 && y < 300 = f "is-fixed-top" -- Should be middle?
      | y > 300 = f "is-fixed-top"
      | otherwise = f "is-fixed-bottom"
      where f p = ("class" =: ("navbar " <> p))
              <> ("style" =: "z-index: 1060; padding: 2vw;")

    wrapper :: (DOM.IsElement e)
      => Maybe e
      -> AppMonadT t m a
      -> AppMonadT t m (Event t ())
    wrapper e m = do
      (y,h) <- case e of
        Nothing -> return (0,0)
        (Just e') -> DOM.liftJSM $ do
          rect <- DOM.getBoundingClientRect (e')
          y <- DOM.getY rect
          h <- DOM.getHeight rect
          return (y,h)
      (e1,_) <- elAttr' "div" attrBack $ return ()
      elAttr "div" (attrFront y h) $
        divClass "" $
          elAttr "div" (("class" =: "notification")
            <> ("style" =: "max-height: 30vh;\
                           \overflow-y: auto;\
                           \overflow-x: hidden;\
                           \padding: 2vw;")) $ do
            (e2,_) <- elClass' "button" "delete" $ return ()
            _ <- m
            return $ leftmost
              [domEvent Click e2
              , domEvent Click e1]

    wd :: (AppMonad t m, DOM.IsElement e)
      => Maybe ((Text, Maybe e) , [(Entry, VocabSrsState)])
      -> AppMonadT t m (Event t ())
    wd (Just ((s,e),es)) = (wrapper e)
      (mapM_ (showEntry $ Just s) (orderEntries (fst) es))
    wd Nothing = return never

  detailsEv1 <- getWebSocketResponse $ GetVocabDetails
    <$> fmapMaybeCheap ((fmap NE.toList) . NE.nonEmpty)
        (fmap fst vIdEv)
  surfDyn <- holdDyn ("", Nothing) (fmap snd vIdEv)
  let detailsEv = attach (current surfDyn) detailsEv1

  rec
    let ev = leftmost [Just <$> detailsEv
             , Nothing <$ ((switch . current) closeEv)]
    closeEv <- widgetHold (return never)
      (wd <$> ev)

  return ()

showEntry :: AppMonad t m
  => Maybe Text
  -> (Entry, VocabSrsState)
  -> AppMonadT t m ()
showEntry surfaceMB (e, sId) = divClass "box" $ do
  let
    surface = maybe (e ^. entryReadingElements . to (NE.head)
             . readingPhrase . to (unReadingPhrase))
      identity surfaceMB

  divClass "columns" $ divClass "column" $ do
    divClass "columns" $ do
      entryKanjiAndReading surface e
    divClass "column" $ do
      mapM_ (\s -> elClass "p" "columns" $
              text $ showSense s) $ take 3 $ e ^.. entrySenses . traverse

    divClass "column is-narrow" $ do
      addEditSrsEntryWidget (Right $ e ^. entryUniqueId) surfaceMB sId
      openEv <- btn "btn-xs btn-primary" "Sentences"
      openSentenceWidget (surface, e ^.. entrySenses . traverse .
                           senseGlosses . traverse . glossDefinition)
        ((Left $ e ^. entryUniqueId) <$ openEv)


entryKanjiAndReading :: (DomBuilder t m) => Text -> Entry -> m ()
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

renderElement :: (DomBuilder t m)
  => Text
  -> Map KanjiPhrase ReadingElement
  -> ReadingPhrase
  -> (Either KanjiElement ReadingElement)
  -> m ()
renderElement surface restMap defR (Left ke) = case v1 of
  (Right v) -> dispInSpan (vocabToText v) $ displayVocabT v
  (Left _) ->
    (\t -> dispInSpan t $ text t) $ unKanjiPhrase $ ke ^. kanjiPhrase
  where
    dispInSpan t = el (spanAttr t)
    spanAttr t = if (T.isPrefixOf surface t) then "strong" else "span"
    kp = (ke ^. kanjiPhrase)
    v1 = case Map.lookup kp restMap of
          (Just r) -> makeFurigana kp (r ^. readingPhrase)
          Nothing -> makeFurigana kp defR

renderElement surface _ _ (Right re) =
  (\t -> dispInSpan t $ text t) $ unReadingPhrase $ re ^. readingPhrase
  where
    dispInSpan t = el (spanAttr t)
    spanAttr t = if (T.isPrefixOf surface t) then "strong" else "span"

widgetHoldWithRemoveAfterEvent
  :: (MonadFix m,
       MonadHold t m,
       DomBuilder t m)
  => Event t (m (Event t a))
  -> m (Event t a)
widgetHoldWithRemoveAfterEvent wEv = do
  let
    f1 w = do
      rec
        let ev = (switch . current) evDyn
        evDyn <- widgetHold (w)
          (return never <$ ev)
      return ev
  evDyn <- widgetHold (return never)
    (f1 <$> wEv)
  return $ (switch . current) evDyn

-- | A widget to construct a tabbed view that shows only one of its child widgets at a time.
--   Creates a header bar containing a <ul> with one <li> per child; clicking a <li> displays
--   the corresponding child and hides all others.
tabDisplayUI :: forall m k t . (MonadFix m
                               , DomBuilder t m
                               , MonadHold t m
                               , PostBuild t m
                               , Ord k)
  => (forall a . m a -> m a)       -- ^ Wrapper around the nav list
  -> Text               -- ^ Class applied to top <div> element
  -> Text               -- ^ Class applied to currently active <div> element
  -> Text               -- ^ Class applied to currently non-active <div> element
  -> Map k (Text, m ()) -- ^ Map from (arbitrary) key to (tab label, child widget)
  -> m ()
tabDisplayUI wrapNav ulClass activeClass nonActiveClass tabItems = do
  let t0 = listToMaybe $ Map.keys tabItems
  rec currentTab :: Demux t (Maybe k) <- wrapNav $ elAttr "ul" ("class" =: ulClass) $ do
        tabClicksList :: [Event t k] <- Map.elems <$> imapM (\k (s,_) -> headerBarLink s k $ demuxed currentTab (Just k)) tabItems
        let eTabClicks :: Event t k = leftmost tabClicksList
        fmap demux $ holdDyn t0 $ fmap Just eTabClicks
  el "div" $ do
    iforM_ tabItems $ \k (_, w) -> do
      let isSelected = demuxed currentTab $ Just k
          attrs = ffor isSelected $ \s -> if s then Map.empty else Map.singleton "style" "display:none;"
      elDynAttr "div" attrs w
    return ()
  where
    headerBarLink :: Text -> k -> Dynamic t Bool -> m (Event t k)
    headerBarLink x k isSelected = do
      let attrs = fmap (\b -> if b then Map.singleton "class" activeClass else Map.singleton "class" nonActiveClass) isSelected
      elDynAttr "li" attrs $ do
        a <- link x
        return $ fmap (const k) (_link_clicked a)
