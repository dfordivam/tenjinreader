{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
module TextReader where

import FrontendCommon
import ReadingPane

import qualified Data.Map as Map

data TextReaderWidgetView
  = ListOfDocumentsView
  | EditDocumentView
  | ReadingView
  deriving (Eq)

textReaderTop
  :: AppMonad t m
  => AppMonadT t m ()
textReaderTop = do
  rec
    let visEv = leftmost
          [ListOfDocumentsView <$ closeEv
          , ListOfDocumentsView <$ cancelEv
          , EditDocumentView <$ editEv
          , ReadingView <$ viewEv]
        viewEv = leftmost [viewEv1
                          , viewEv2]
        editEv = leftmost [editEv1]

    vis <- holdDyn ListOfDocumentsView visEv

    (viewEv1, editEv1) <- handleVisibility ListOfDocumentsView vis $
      (documentListViewer (closeEv))

    (viewEv2, cancelEv) <- handleVisibility EditDocumentView vis $
      documentEditor editEv

    (closeEv) <- handleVisibility ReadingView vis $
      readingPane viewEv
  return ()

documentListViewer
  :: forall t m . AppMonad t m
  => Event t () -- refresh
  -> AppMonadT t m (Event t (ReaderDocumentData)
                   , Event t (Maybe (ReaderDocumentId, Text, Text)))
documentListViewer refreshEv = do
  (_, evAll) <- runEventWriterT $
    tabDisplayUI wrapperSW "" "is-active" "" $
    Map.fromList
      [ (1, ("My Reading List", myDocumentsListViewer refreshEv))
      , (2, ("Books", booksListViewer))
      -- , (3, ("Articles", articlesListViewer))
      ]

  return $ (fmapMaybe (headMay . lefts) evAll
           , fmapMaybe (headMay . rights) evAll)

type ListViewTabsM t m a =
  EventWriterT t
    [Either (ReaderDocumentData)
     (Maybe (ReaderDocumentId, Text, Text))]
    (AppMonadT t m) a

myDocumentsListViewer
  :: forall t m . AppMonad t m
  => Event t () -- refresh
  -> ListViewTabsM t m ()
myDocumentsListViewer refreshEv = do
  ev <- getPostBuild
  listEv <- getWebSocketResponse
    (ListDocuments Nothing <$ (leftmost [ev,refreshEv]))
  newDocEv <- divClass "columns" $ divClass "column" $ btn "is-medium" "新作"

  rec
    let
      viewEv :: Event t ViewDocument
      viewEv = (switch . current) (leftmost <$> ((map fst) <$> evDyn))
      viewRawEv = (switch . current) (leftmost <$> ((map (fst . snd)) <$> evDyn))

    evDyn <- lift $ widgetHold (return [])
      (viewList <$> (leftmost [listEv, delDone]))
    let
      deleteEv :: Event t DeleteDocument
      deleteEv = (switch . current) (leftmost <$> ((map (snd . snd)) <$> evDyn))
    delDone <- lift $ getWebSocketResponse deleteEv
    resp <- lift $ getWebSocketResponse viewEv

  editEv <- lift $ getWebSocketResponse viewRawEv

  tellEvent ((\e -> [Left e]) <$> fmapMaybe identity resp)
  tellEvent ((\e -> [Right e]) <$> editEv)
  tellEvent ([Right Nothing] <$ newDocEv)

  return ()

booksListViewer
  :: forall t m . AppMonad t m
  => ListViewTabsM t m ()
booksListViewer = viewerCommon ListBooks ViewBook

articlesListViewer
  :: forall t m . AppMonad t m
  => ListViewTabsM t m ()
articlesListViewer = viewerCommon ListArticles ViewArticle

viewerCommon
  :: forall t m req i . (AppMonad t m
                        , WebSocketMessage AppRequest req
                        , ResponseT AppRequest req ~ [(i, Text, Text)])
  => (Maybe Int -> req)
  -> (i -> ViewDocument)
  -> ListViewTabsM t m ()
viewerCommon fetchF viewDocF = do
  ev <- getPostBuild
  listEv <- getWebSocketResponse (fetchF Nothing <$ ev)

  let
    viewF lss = do
      elClass "table" "table is-striped is-narrow" $ do
        el "tbody" $  do
          forM lss $ \(i, t, c) -> do
            el "tr" $ do
              ev <- el "td" $ tileBoxLoading $ do
                elClass "p" "title" $ text t
                el "p" $ text c
              return (viewDocF i <$ ev)
  evDyn <- widgetHold (return [])
                (viewF <$> listEv)

  let
    viewEv :: Event t ViewDocument
    viewEv = (switch . current) (leftmost <$> evDyn)
  resp <- lift $ getWebSocketResponse viewEv
  tellEvent ((\e -> [Left e]) <$> fmapMaybe identity resp)

viewList :: (AppMonad t m)
  => [(ReaderDocumentId, Text, Text)]
  -> AppMonadT t m [(Event t ViewDocument
                    , (Event t ViewRawDocument
                    , Event t DeleteDocument))]
viewList [] = do
  el "p" $ text "Reading list empty..."
  el "p" $ text "Please click 新作 to add your content, or checkout some books!"
  return []

viewList lss = do
  elClass "table" "table is-striped is-narrow" $ do
    el "tbody" $  do
      forM lss $ \(i, t, c) -> do
        el "tr" $ do
          ev1 <- el "td" $ tileBoxLoading $ do
            elClass "p" "title" $ text t
            el "p" $ text c
          el "td" $ do
            ed <- btnLoading "btn-xs btn-primary" "Edit" never
            d <- btnLoading "btn-xs btn-warning" "Delete" never
            return (ViewDocument i Nothing <$ ev1
               , (ViewRawDocument i <$ ed
               , DeleteDocument i <$ d))

tileBoxLoading m = do
  let cl1 = "tile box control is-large"
  rec
    let ev = domEvent Click e
    clDyn <- holdDyn cl1 ((cl1 <> " is-loading") <$ ev)
    (e,_) <- elDynClass' "div" clDyn m
  return ev

documentEditor
  :: AppMonad t m
  => Event t (Maybe (ReaderDocumentId, Text, Text))
  -> AppMonadT t m (Event t (ReaderDocumentData), Event t ())
documentEditor editEv = divClass "" $ do
  let
    tiAttr = constDyn $ (("style" =: "width: 100%;")
                        <> ("class" =: "input")
                        <> ("placeholder" =: "Title"))
    taAttr = constDyn $ (("style" =: "width: 100%;")
                        <> ("rows" =: "10")
                        <> ("class" =: "textarea")
                        <> ("placeholder" =: "Contents"))

    titleSetEv = editEv & mapped %~ (maybe "" (view _2))
    contentSetEv = editEv & mapped %~ (maybe "" (view _3))

  rdDyn <- holdDyn Nothing editEv

  ti <- textInput $ def
    & textInputConfig_attributes .~ tiAttr
    & textInputConfig_setValue .~ titleSetEv
  ta <- textArea $ def
    & textAreaConfig_attributes .~ taAttr
    & textAreaConfig_setValue .~ contentSetEv

  saveEv <- btnLoading "is-success" "Save" never
  cancelEv <- btn "is-danger" "Cancel"

  let evDyn = AddOrEditDocument
        <$> (rdDyn & mapped %~ preview (_Just . _1))
        <*> (value ti)
        <*> (value ta)
  annTextEv <- getWebSocketResponse
    $ tag (current evDyn) saveEv
  return $ (fmapMaybe identity annTextEv
    , cancelEv)

sentenceWidget
  :: AppMonad t m
  => AppMonadT t m ()
sentenceWidget = divClass "coloumns" $ do
  tabDisplayUI wrapperSW "" "is-active" "" $
    Map.fromList
      [ (1, ("Analyze", quickAnalyzeTop))
      , (2, ("Random", randomSentenceTop))]

wrapperSW m = do
  a <- divClass "tabs is-centered" $ m
  return (a,())

quickAnalyzeTop
  :: AppMonad t m
  => AppMonadT t m ()
quickAnalyzeTop = do
  let
    taAttr = constDyn $ (("style" =: "width: 100%;")
                        <> ("rows" =: "4")
                        <> ("class" =: "textarea")
                        <> ("placeholder" =: "文章"))
  ta <- divClass "" $ do
    ev <- btn "btn-xs btn-default" "Clear"
    textArea $ def
      & textAreaConfig_attributes .~ taAttr
      & textAreaConfig_setValue .~ ("" <$ ev)
  resp <- getWebSocketResponse $ QuickAnalyzeText <$> (_textArea_input ta)

  v <- divClass "" $ do
    (_, rsDyn, _) <- readerSettingsControls def False
    let renderF = renderOnePara (constDyn ([],[]))
          (_rubySize <$> rsDyn)
    divWrap rsDyn (constDyn False) $ do
      widgetHold (return [])
        ((\r -> mapM renderF $ map snd r) <$> resp)

  do
    p <- holdDyn [] $ (map snd) <$> resp
    bEv <- icon "fa-clipboard"
    widgetHold (return ())
      ((\p -> divClass "" $ showPlainForm p) <$> tagDyn p bEv)

  elAttr "div" ("style" =: "height: 20vh;") $ return ()
  let vIdEv = (switch . current) $ leftmost <$> v

  showVocabDetailsWidget vIdEv
  return ()

randomSentenceTop
  :: AppMonad t m
  => AppMonadT t m ()
randomSentenceTop = do
  rec
    v <- divClass "" $ do
      (_, rsDyn, _) <- readerSettingsControls def False
      elAttr "div" ("style" =: "height: 10vh;") $ return ()
      divWrap rsDyn (constDyn False) $ do
        widgetHold (return [])
          (uncurry (renderOneSentence []) <$> resp)

    resp <- divClass "columns is-centered" $ do
      ev1 <- divClass "column is-narrow" $
        btn "btn-primary" "Random Sentence"
      ev2 <- divClass "column is-narrow" $
        btn "btn-primary" "Random Fav Sentence"
      pb <- getPostBuild
      resp <- getWebSocketResponse $ leftmost [GetRandomSentence <$ ev1
                                    , GetRandomFavSentence <$ ev2
                                    , GetRandomSentence <$ pb]
      return resp

  elAttr "div" ("style" =: "height: 20vh;") $ return ()

  let vIdEv = (switch . current) $ leftmost <$> v

  showVocabDetailsWidget vIdEv
  return ()
