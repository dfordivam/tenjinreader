{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Common.Api where

import Servant
import Data.Text
import Data.Proxy

type ReaderAPI =
  "api" :> "book" :> Capture "bookid" Int :> Get '[JSON] Text
  :<|> "api" :> "book" :> Capture "bookid" Int :> Delete '[JSON] ()

readerAPI :: Proxy ReaderAPI
readerAPI = Proxy

commonStuff :: String
commonStuff = "Here is a string defined in code common to the frontend and backend."
