{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
  muser <- maybeAuthPair
  -- let muser = Just (undefined, undefined)
  defaultLayout $ do
    $(widgetFile "homepage")

getNewKeyR :: Handler Html
getNewKeyR = do
  uId <- requireAuthId
  s <- liftIO $ makeRandomKey
  runDB $ update uId [UserSecretKey =. Just s]
  redirect HomeR
