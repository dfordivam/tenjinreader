{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
module DerivingInstances where

import Protolude
import Data.List.NonEmpty (NonEmpty(..))
import Data.JMDict.AST
import Data.Aeson hiding (Value)

deriving instance ToJSON EntryId
deriving instance FromJSON EntryId

deriving instance ToJSON Sense
deriving instance FromJSON Sense

deriving instance ToJSON Entry
deriving instance FromJSON Entry

deriving instance ToJSON KanjiElement
deriving instance FromJSON KanjiElement

deriving instance ToJSON ReadingElement
deriving instance FromJSON ReadingElement

deriving instance ToJSON Priority
deriving instance FromJSON Priority

deriving instance ToJSON ReadingPhrase
deriving instance FromJSON ReadingPhrase

deriving instance ToJSON KanjiPhrase
deriving instance FromJSON KanjiPhrase

deriving instance ToJSON KanjiInfo
deriving instance FromJSON KanjiInfo

deriving instance ToJSON ReadingInfo
deriving instance FromJSON ReadingInfo

deriving instance ToJSON PartOfSpeech
deriving instance FromJSON PartOfSpeech

deriving instance ToJSON Auxiliary
deriving instance FromJSON Auxiliary

deriving instance ToJSON NounType
deriving instance FromJSON NounType

deriving instance ToJSON VerbType
deriving instance FromJSON VerbType

deriving instance ToJSON Dialect
deriving instance FromJSON Dialect

deriving instance ToJSON Gloss
deriving instance FromJSON Gloss

deriving instance ToJSON SenseField
deriving instance FromJSON SenseField

deriving instance ToJSON LanguageSource
deriving instance FromJSON LanguageSource

deriving instance ToJSON SenseMisc
deriving instance FromJSON SenseMisc

deriving instance ToJSON Adjective
deriving instance FromJSON Adjective

deriving instance ToJSON Adverb
deriving instance FromJSON Adverb

deriving instance ToJSON SpecialVerb
deriving instance FromJSON SpecialVerb

deriving instance ToJSON RegularVerb
deriving instance FromJSON RegularVerb

deriving instance ToJSON IrregularVerb
deriving instance FromJSON IrregularVerb

deriving instance ToJSON VerbEnding
deriving instance FromJSON VerbEnding

deriving instance ToJSON IsTransitive
deriving instance FromJSON IsTransitive
