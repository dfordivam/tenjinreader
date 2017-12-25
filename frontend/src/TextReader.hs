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
          , EditDocumentView <$ editEv
          , ReadingView <$ viewEv]
        viewEv = leftmost [viewEv1, viewEv2]
        editEv = leftmost [Nothing <$ newDocEv
                          , Just <$> editEv2]

    vis <- holdDyn ListOfDocumentsView visEv

    (viewEv1, newDocEv) <- handleVisibility ListOfDocumentsView vis $
      (documentListViewer (closeEv))

    (viewEv2) <- handleVisibility EditDocumentView vis $
      documentEditor editEv

    (closeEv, editEv2) <- handleVisibility ReadingView vis $
      readingPane viewEv
  return ()

documentListViewer
  :: AppMonad t m
  => Event t () -- refresh
  -> AppMonadT t m (Event t (ReaderDocument CurrentDb)
                   , Event t ())
documentListViewer refreshEv = do
  ev <- getPostBuild
  listEv <- getWebSocketResponse
    (ListDocuments <$ (leftmost [ev,refreshEv]))

  newDocEv <- button "New"
  evDyn <- widgetHold (return [])
    (viewList <$> listEv)
  resp <- getWebSocketResponse
    (switchPromptlyDyn (leftmost <$> evDyn))
  return $ (fmapMaybe identity resp
           , newDocEv)

viewList lss = do
  elClass "table" "table table-striped" $ do
    el "thead" $ do
      el "tr" $ do
        el "th" $ text "Title"
        el "th" $ text "Contents"
    el "tbody" $  do
      forM lss $ \(i, t, c) -> do
        (e,_) <- el' "tr" $ do
          el "td" $ text t
          el "td" $ text c
        return (ViewDocument i <$ domEvent Click e)

documentEditor
  :: AppMonad t m
  => Event t (Maybe (ReaderDocument CurrentDb))
  -> AppMonadT t m (Event t (ReaderDocument CurrentDb))
documentEditor editEv = divClass "" $ do
  let
    tiAttr = constDyn $ (("style" =: "width: 100%;")
                        <> ("placeholder" =: "Title"))
    taAttr = constDyn $ (("style" =: "width: 100%;")
                        <> ("rows" =: "10")
                        <> ("placeholder" =: "Contents"))

    titleSetEv = (maybe "" _readerDocTitle) <$> editEv
    contentSetEv = (maybe "" getContentText) <$> editEv
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
        <$> (fmap _readerDocId <$> rdDyn)
        <*> (value ti)
        <*> (value ta)
  annTextEv <- getWebSocketResponse
    $ tagDyn evDyn saveEv
  showWSProcessing saveEv annTextEv
  delEv <- delay 0.1 (fmapMaybe identity $ tagDyn rdDyn cancelEv)
  return $ leftmost [fmapMaybe identity annTextEv
    , delEv]
