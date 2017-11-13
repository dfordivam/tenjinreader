{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
module Message
  where

import Protolude
import Data.Aeson

import Reflex.Dom.WebSocket.Message
-- Messages

type AppRequest =
  StoreData
  :<|> RetrieveData

data StoreData =
  StoreData Text Text
  deriving (Generic, Show, ToJSON, FromJSON)

data StoreDataResp =
  StoreDataResp Bool
  deriving (Generic, Show, ToJSON, FromJSON)

instance WebSocketMessage AppRequest StoreData where
  type ResponseT AppRequest StoreData = StoreDataResp

data RetrieveData
  = RetrieveData Text
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

data RetrieveDataResp
  = RetrieveDataResp (Maybe Text)
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

instance WebSocketMessage AppRequest RetrieveData where
  type ResponseT AppRequest RetrieveData = RetrieveDataResp
