{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

loginWidget :: MonadWidget t m => m (Event t (Maybe Text))
loginWidget = do
  v <- getValue
  liftIO $ putStrLn $ (show v :: Text)
  case v of
    (Just s) -> do
      pb <- getPostBuild
      return $ (Just s) <$ pb
    Nothing -> do
      el "h3" $ text "Please login at tenjinreader.com"
      lEv <- el "div" $ do
        text "Enter your secret key"
        ti <- textInput def
        ev <- btn "" "Login"
        return (tagDyn (value ti) ev)

      sEv <- checkLogin lEv
      performEvent $ ffor sEv $ \(Just s) -> do
        saveValue s
        return $ Just s

checkLogin lEv = do
  let url s = "http://localhost:3000/websocket/app/" <> s
      req s = XhrRequest "GET" (url s) $ def
        & xhrRequestConfig_responseType .~ Just XhrResponseType_Default
  resp <- performRequestAsync (req <$> lEv)
  let r = (\s -> g <$> (view xhrResponse_response s)) <$> resp
      h = (\s ->(view xhrResponse_headers s)) <$> resp
      g (XhrResponseBody_Default t) = "def" <> t
      g (XhrResponseBody_Text t) = "text" <> t
      g (XhrResponseBody_Blob t) = "blob"
      g (XhrResponseBody_ArrayBuffer t) = "bytestring"
  d <- holdDyn Nothing (r)
  d2 <- holdDyn Nothing (Just <$> h)
  display d
  display d2
  sDyn <- holdDyn Nothing (Just <$> lEv)
  return $ tagDyn sDyn resp
