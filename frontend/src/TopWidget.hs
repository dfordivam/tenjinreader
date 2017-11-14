{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}

module TopWidget
  (topWidget)
  where

import FrontendCommon
import SrsWidget
import KanjiBrowser

-- from package common
import Common
import Message

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Set as Set
import Control.Lens.Indexed

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.Text as T

topWidget :: MonadWidget t m => m ()
topWidget = do
  let url = "ws://localhost:3000/websocket"
  withWSConnection
    url
    never -- close event
    True -- reconnect
    widget
  return ()

widget :: AppMonad t m => AppMonadT t m ()
widget = divClass "container" $ do
  -- navigation with visibility control
  tabDisplayUI "" "" "" $
    Map.fromList
      [(0, ("SRS", srsWidget))
      ,(1, ("Kanji", kanjiBrowseWidget))
      , (2, ("Vocab", vocabSearchWidget))]

-- | A widget to construct a tabbed view that shows only one of its child widgets at a time.
--   Creates a header bar containing a <ul> with one <li> per child; clicking a <li> displays
--   the corresponding child and hides all others.
tabDisplayUI :: forall m k t . (MonadFix m, DomBuilder t m, MonadHold t m, PostBuild t m, Ord k)
  => Text               -- ^ Class applied to top <div> element
  -> Text               -- ^ Class applied to currently active <div> element
  -> Text               -- ^ Class applied to currently non-active <div> element
  -> Map k (Text, m ()) -- ^ Map from (arbitrary) key to (tab label, child widget)
  -> m ()
tabDisplayUI ulClass activeClass nonActiveClass tabItems = do
  let t0 = listToMaybe $ Map.keys tabItems
  rec currentTab :: Demux t (Maybe k) <- elAttr "div" ("class" =: ulClass) $ do
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
      elDynAttr "div" attrs $ do
        a <- link x
        return $ fmap (const k) (_link_clicked a)
