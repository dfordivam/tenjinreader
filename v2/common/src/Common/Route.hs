{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Common.Route where

{- -- You will probably want these imports for composing Encoders.
import Prelude hiding (id, (.))
import Control.Category
-}

import Data.Text (Text)
import Data.Functor.Identity
import Data.Functor.Sum
import Servant.API
import Obelisk.Route
import Obelisk.Route.TH

data BackendRoute :: * -> * where
  BackendRoute_Missing :: BackendRoute ()
  BackendRoute_Api :: BackendRoute ()
deriving instance Show (BackendRoute a)

data FrontendRoute :: * -> * where
  FrontendRoute_Home :: FrontendRoute ()
  FrontendRoute_Reader :: FrontendRoute ()
  FrontendRoute_SRS :: FrontendRoute ()
  FrontendRoute_Analyze :: FrontendRoute ()
  FrontendRoute_Sentences :: FrontendRoute ()
deriving instance Show (FrontendRoute a)

fullRouteEncoder
  :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
fullRouteEncoder = mkFullRouteEncoder
  (FullRoute_Backend BackendRoute_Missing :/ ())
  (\case
     BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
     BackendRoute_Api     -> PathSegment "api" $ unitEncoder mempty
  )
  (\case
      FrontendRoute_Home -> PathEnd $ unitEncoder mempty
      FrontendRoute_Reader -> PathSegment "reader" $ unitEncoder mempty
      FrontendRoute_SRS -> PathSegment "srs" $ unitEncoder mempty
      FrontendRoute_Analyze -> PathSegment "sentence" $ unitEncoder mempty
      FrontendRoute_Sentences -> PathSegment "sentences" $ unitEncoder mempty
  )

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  ]
