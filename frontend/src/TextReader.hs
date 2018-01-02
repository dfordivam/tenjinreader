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

import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector as V

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

    (closeEv, editEv2) <- handleVisibility ReadingView vis $
      readingPane viewEv
  return ()

documentListViewer
  :: forall t m . AppMonad t m
  => Event t () -- refresh
  -> AppMonadT t m (Event t (ReaderDocumentData)
                   , Event t (Maybe (ReaderDocumentId, Text, Text)))
documentListViewer refreshEv = do
  ev <- getPostBuild
  listEv <- getWebSocketResponse
    (ListDocuments <$ (leftmost [ev,refreshEv]))

  newDocEv <- button "New"
  rec
    showWSProcessing deleteEv delDone
    evDyn <- widgetHold (return [])
      (viewList <$> (leftmost [listEv, delDone]))
    let
      deleteEv :: Event t DeleteDocument
      deleteEv = switchPromptlyDyn (leftmost <$> ((map (snd . snd)) <$> evDyn))
    delDone <- getWebSocketResponse deleteEv

  let
    viewEv :: Event t ViewDocument
    viewEv = switchPromptlyDyn (leftmost <$> ((map fst) <$> evDyn))
    viewRawEv = switchPromptlyDyn (leftmost <$> ((map (fst . snd)) <$> evDyn))

  editEv <- getWebSocketResponse viewRawEv
  resp <- getWebSocketResponse viewEv
  return $ (fmapMaybe identity resp
           , leftmost [Nothing <$ newDocEv, editEv])

viewList :: (AppMonad t m)
  => [(ReaderDocumentId, Text, Text)]
  -> AppMonadT t m [(Event t ViewDocument
                    , (Event t ViewRawDocument
                    , Event t DeleteDocument))]
viewList lss = do
  elClass "table" "table table-striped" $ do
    el "thead" $ do
      el "tr" $ do
        el "th" $ text "Title"
        el "th" $ text "Contents"
        el "th" $ text ""
    el "tbody" $  do
      forM lss $ \(i, t, c) -> do
        el "tr" $ do
          (e1,_) <- el' "td" $ text t
          (e2,_) <- el' "td" $ text c
          el "td" $ do
            ed <- btn "btn-xs" "Edit"
            d <- btn "btn-xs" "Delete"
            return (ViewDocument i Nothing
                    <$ (leftmost [domEvent Click e1, domEvent Click e2])
               , (ViewRawDocument i <$ ed
               , DeleteDocument i <$ d))

documentEditor
  :: AppMonad t m
  => Event t (Maybe (ReaderDocumentId, Text, Text))
  -> AppMonadT t m (Event t (ReaderDocumentData), Event t ())
documentEditor editEv = divClass "" $ do
  let
    tiAttr = constDyn $ (("style" =: "width: 100%;")
                        <> ("placeholder" =: "Title"))
    taAttr = constDyn $ (("style" =: "width: 100%;")
                        <> ("rows" =: "10")
                        <> ("placeholder" =: "Contents"))

    titleSetEv = editEv & mapped %~ (maybe "" (view _2))
    contentSetEv = editEv & mapped %~ (maybe "" (view _3))
    getContentText (ReaderDocument _ _ c _)
      = T.unlines $ map toT (V.toList c)
      where
        toT ap = (mconcat $ map getT ap)
        getT (Left t) = t
        getT (Right (v,_,_)) = vocabToText v

  rdDyn <- holdDyn Nothing editEv

  ti <- textInput $ def
    & textInputConfig_attributes .~ tiAttr
    & textInputConfig_setValue .~ titleSetEv
  ta <- textArea $ def
    & textAreaConfig_attributes .~ taAttr
    & textAreaConfig_setValue .~ contentSetEv

  saveEv <- button "Save"
  cancelEv <- button "Cancel"

  let evDyn = AddOrEditDocument
        <$> (rdDyn & mapped %~ preview (_Just . _1))
        <*> (value ti)
        <*> (value ta)
  annTextEv <- getWebSocketResponse
    $ tagDyn evDyn saveEv
  showWSProcessing saveEv annTextEv
  return $ (fmapMaybe identity annTextEv
    , cancelEv)

quickAnalyzeTop
  :: AppMonad t m
  => AppMonadT t m ()
quickAnalyzeTop = do
  let
    taAttr = constDyn $ (("style" =: "width: 100%;")
                        <> ("rows" =: "4")
                        <> ("class" =: "form-control")
                        <> ("placeholder" =: "Enter text to search all Kanjis in it"))
  ta <- divClass "col-md-6" $ do
    textArea $ def
      & textAreaConfig_attributes .~ taAttr
  resp <- getWebSocketResponse $ QuickAnalyzeText <$> (_textArea_input ta)

  v <- divClass "col-md-6" $ do
    rsDyn <- readerSettingsControls def
    let renderF = renderOnePara (constDyn [])
          (_rubySize <$> rsDyn) 0
    divWrap rsDyn (constDyn False) $ do
      widgetHold (return [])
        ((\r -> mapM renderF $ map snd r) <$> resp)

  let vIdEv = switchPromptlyDyn $ leftmost <$> v

  divClass "" $ do
    detailsEv <- getWebSocketResponse $ GetVocabDetails
      <$> (fmap fst vIdEv)
    surfDyn <- holdDyn "" (fmap snd vIdEv)
    showVocabDetailsWidget (attachDyn surfDyn detailsEv)
  return ()
