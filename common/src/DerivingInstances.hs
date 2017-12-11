{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
module DerivingInstances where

import Protolude
import Data.Binary
import Data.List.NonEmpty (NonEmpty(..))
import Data.JMDict.AST
import Data.Aeson hiding (Value)
import Data.BTree.Primitives (Value)

deriving instance Generic EntryId
deriving instance ToJSON EntryId
deriving instance FromJSON EntryId
deriving instance Binary EntryId
deriving instance Value EntryId

deriving instance Generic Sense
deriving instance ToJSON Sense
deriving instance FromJSON Sense
deriving instance Binary Sense
deriving instance Value Sense

deriving instance Generic Entry
deriving instance ToJSON Entry
deriving instance FromJSON Entry
deriving instance Binary Entry
deriving instance Value Entry

deriving instance Generic KanjiElement
deriving instance ToJSON KanjiElement
deriving instance FromJSON KanjiElement
deriving instance Binary KanjiElement
deriving instance Value KanjiElement

deriving instance Generic ReadingElement
deriving instance ToJSON ReadingElement
deriving instance FromJSON ReadingElement
deriving instance Binary ReadingElement
deriving instance Value ReadingElement

deriving instance Generic Priority
deriving instance ToJSON Priority
deriving instance FromJSON Priority
deriving instance Binary Priority
deriving instance Value Priority

deriving instance Generic ReadingPhrase
deriving instance ToJSON ReadingPhrase
deriving instance FromJSON ReadingPhrase
deriving instance Binary ReadingPhrase
deriving instance Value ReadingPhrase

deriving instance Generic KanjiPhrase
deriving instance ToJSON KanjiPhrase
deriving instance FromJSON KanjiPhrase
deriving instance Binary KanjiPhrase
deriving instance Value KanjiPhrase

deriving instance Generic KanjiInfo
deriving instance ToJSON KanjiInfo
deriving instance FromJSON KanjiInfo
deriving instance Binary KanjiInfo
deriving instance Value KanjiInfo

deriving instance Generic ReadingInfo
deriving instance ToJSON ReadingInfo
deriving instance FromJSON ReadingInfo
deriving instance Binary ReadingInfo
deriving instance Value ReadingInfo

deriving instance Generic PartOfSpeech
deriving instance ToJSON PartOfSpeech
deriving instance FromJSON PartOfSpeech
deriving instance Binary PartOfSpeech
deriving instance Value PartOfSpeech

deriving instance Generic Auxiliary
deriving instance ToJSON Auxiliary
deriving instance FromJSON Auxiliary
deriving instance Binary Auxiliary
deriving instance Value Auxiliary

deriving instance Generic NounType
deriving instance ToJSON NounType
deriving instance FromJSON NounType
deriving instance Binary NounType
deriving instance Value NounType

deriving instance Generic VerbType
deriving instance ToJSON VerbType
deriving instance FromJSON VerbType
deriving instance Binary VerbType
deriving instance Value VerbType

deriving instance Generic Dialect
deriving instance ToJSON Dialect
deriving instance FromJSON Dialect
deriving instance Binary Dialect
deriving instance Value Dialect

deriving instance Generic Gloss
deriving instance ToJSON Gloss
deriving instance FromJSON Gloss
deriving instance Binary Gloss
deriving instance Value Gloss

deriving instance Generic SenseField
deriving instance ToJSON SenseField
deriving instance FromJSON SenseField
deriving instance Binary SenseField
deriving instance Value SenseField

deriving instance Generic LanguageSource
deriving instance ToJSON LanguageSource
deriving instance FromJSON LanguageSource
deriving instance Binary LanguageSource
deriving instance Value LanguageSource

deriving instance Generic SenseMisc
deriving instance ToJSON SenseMisc
deriving instance FromJSON SenseMisc
deriving instance Binary SenseMisc
deriving instance Value SenseMisc

deriving instance Generic Adjective
deriving instance ToJSON Adjective
deriving instance FromJSON Adjective
deriving instance Binary Adjective
deriving instance Value Adjective

deriving instance Generic Adverb
deriving instance ToJSON Adverb
deriving instance FromJSON Adverb
deriving instance Binary Adverb
deriving instance Value Adverb

deriving instance Generic SpecialVerb
deriving instance ToJSON SpecialVerb
deriving instance FromJSON SpecialVerb
deriving instance Binary SpecialVerb
deriving instance Value SpecialVerb

deriving instance Generic RegularVerb
deriving instance ToJSON RegularVerb
deriving instance FromJSON RegularVerb
deriving instance Binary RegularVerb
deriving instance Value RegularVerb

deriving instance Generic IrregularVerb
deriving instance ToJSON IrregularVerb
deriving instance FromJSON IrregularVerb
deriving instance Binary IrregularVerb
deriving instance Value IrregularVerb

deriving instance Generic VerbEnding
deriving instance ToJSON VerbEnding
deriving instance FromJSON VerbEnding
deriving instance Binary VerbEnding
deriving instance Value VerbEnding

deriving instance Generic IsTransitive
deriving instance ToJSON IsTransitive
deriving instance FromJSON IsTransitive
deriving instance Binary IsTransitive
deriving instance Value IsTransitive

deriving instance Binary (NonEmpty ReadingElement)
