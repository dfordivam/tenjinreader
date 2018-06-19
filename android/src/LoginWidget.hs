{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RankNTypes #-}

module LoginWidget
  (loginWidget, logout)
  where

import FrontendCommon
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.Storage as DOM
import qualified GHCJS.DOM.Window as DOM
import qualified GHCJS.DOM as DOM

keyV :: Text
keyV = "tenjinreader_com_user_secret_key"

saveValue :: _ => Text -> m ()
saveValue v = DOM.liftJSM $ do
  window <-  DOM.currentWindowUnchecked
  storage <- DOM.getLocalStorage window
  DOM.setItem storage keyV v

getValue :: _ => m (Maybe Text)
getValue = DOM.liftJSM $ do
  window <-  DOM.currentWindowUnchecked
  storage <- DOM.getLocalStorage window
  v <- DOM.getItem storage keyV
  if v == (Just "undefined")
    then return Nothing
    else return v

logout :: _ => m ()
logout = saveValue "undefined"

loginWidget :: MonadWidget t m
  => m (Event t (Maybe Text), Event t ())
loginWidget = do
  v <- getValue
  liftIO $ putStrLn $ (show v :: Text)
  case v of
    (Just s) -> do
      pb <- getPostBuild
      return $ (Just s <$ pb, never)
    Nothing -> do
      rec
        lEv <- showLogin incorrectEv
        (incorrectEv, sEv) <- checkLogin lEv
      ev2 <- performEvent $ ffor sEv $ \(Just s) -> do
        saveValue s
        return $ (Just s)
      return (ev2, never)

showLogin :: MonadWidget t m
  => Event t ()
  -> m (Event t Text)
showLogin incorrectEv = elClass "section" "hero is-light is-fullheight" $ do
  divClass "hero-head" $
    elClass "header" "navbar" $
      divClass "container" $
        divClass "navbar-brand" $
          elClass "a" "navbar-item" $
            elAttr "img" ("src" =: "https://tenjinreader.com/static/logo.png") $ return ()

  ev <- divClass "hero-body" $
    divClass "container has-text-centered" $ do
      elClass "h1" "title" $
        text "Enter your secret key"
      el "div" $ do
        ti <- textInput $ def
          & textInputConfig_attributes .~ (constDyn ("class" =: "input"))
        ev <- btnLoading "is-large" "Login" incorrectEv
        widgetHold (return ())
          (ffor incorrectEv $ \_ -> do
              divClass "message is-warning" $
                divClass "message-body" $
                  text "Incorrect key, please try again"
          )
        return (tagDyn (value ti) ev)

  divClass "hero-foot" $
    divClass "container has-text-centered" $ do
      text "Please login at tenjinreader.com to obtain your secret key"
  return ev

checkLogin lEv = do
  let url s = "http://192.168.0.31:3000/websocket/app/" <> s
  r <- getAndDecode (url <$> lEv)
  let incorrectEv = fforMaybe r (\case
             (Just True) -> Nothing
             _ -> Just ())
  sDyn <- holdDyn Nothing (Just <$> lEv)
  return $ (,) incorrectEv $
    tagDyn sDyn $ fforMaybe r $ \case
      (Just True) -> Just ()
      _ -> Nothing
