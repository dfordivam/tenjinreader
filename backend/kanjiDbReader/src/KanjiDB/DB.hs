{-# LANGUAGE TemplateHaskell #-}

module KanjiDB.DB where

import KanjiDB.Model

import Protolude
import Database.Beam
import Control.Lens.TH

data JMdictDb f = JMdictDb {
  _jmdictKanji :: f (TableEntity KanjiT)
  , _jmdictVocab :: f (TableEntity VocabT)
  , _jmdictRadical :: f (TableEntity RadicalT)
  , _jmdictKanjiRadical :: f (TableEntity KanjiRadicalT)
  , _jmdictKanjiStrokes :: f (TableEntity KanjiStrokesT)
  , _jmdictKanjiMeaning :: f (TableEntity KanjiMeaningT)
  , _jmdictVocabCategory :: f (TableEntity VocabCategoryT)
  , _jmdictVocabMeaning :: f (TableEntity VocabMeaningT)
  , _jmdictVocabVocabMeaning :: f (TableEntity VocabVocabMeaningT)
  , _jmdictVocabMeaningVocabCategory :: f (TableEntity VocabMeaningVocabCategoryT)
  , _jmdictVocabVocabCategory :: f (TableEntity VocabVocabCategoryT)
  , _jmdictVocabKanji :: f (TableEntity VocabKanjiT)
  }
  deriving (Generic)

makeLenses ''JMdictDb

instance Database JMdictDb

data SrsDb f = SrsDb
  { srsDbTable :: f (TableEntity SrsEntryT)
  } deriving (Generic)

instance Database SrsDb

jmdictDb :: DatabaseSettings be JMdictDb
jmdictDb = defaultDbSettings `withDbModification`
  dbModification
  { _jmdictKanji = modifyTable (\_ -> "KanjiSet") $
      tableModification
      {
         _kanjiId             = fieldNamed "ID"
       , _kanjiCharacter      = fieldNamed "Character"
       , _kanjiStrokeCount    = fieldNamed "StrokeCount"
       , _kanjiGrade          = fieldNamed "Grade"
       , _kanjiMostUsedRank   = fieldNamed "MostUsedRank"
       , _kanjiJlptLevel      = fieldNamed "JlptLevel"
       , _kanjiOnyomi         = fieldNamed "OnYomi"
       , _kanjiKunyomi        = fieldNamed "KunYomi"
       , _kanjiNanori         = fieldNamed "Nanori"
       , _kanjiUnicodeValue   = fieldNamed "UnicodeValue"
       , _kanjiNewpaperRank   = fieldNamed "NewspaperRank"
       , _kanjiWkLevel        = fieldNamed "WkLevel"
      }
  , _jmdictVocab = modifyTable (\_ -> "VocabSet") $
      tableModification
      {
         _vocabId             = fieldNamed "ID"
       , _vocabKanjiWriting   = fieldNamed "KanjiWriting"
       , _vocabKanaWriting    = fieldNamed "KanaWriting"
       , _vocabIsCommon       = fieldNamed "IsCommon"
       , _vocabFreqRank       = fieldNamed "FrequencyRank"
       , _vocabFurigana       = fieldNamed "Furigana"
       , _vocabJlptLevel      = fieldNamed "JlptLevel"
       , _vocabWkLevel        = fieldNamed "WkLevel"
       , _vocabWikiRank       = fieldNamed "WikiRank"
       , _vocabGroupId        = fieldNamed "GroupId"
       , _vocabIsMain         = fieldNamed "IsMain"
      }
  , _jmdictRadical = modifyTable (\_ -> "RadicalSet") $ tableModification
      {
         _radicalId             = fieldNamed "ID"
       , _radicalCharacter      = fieldNamed "Character"
      }
  , _jmdictKanjiRadical = modifyTable (\_ -> "KanjiRadical") $ tableModification
      {
         _kanjiRadicalKanji     = KanjiId $ fieldNamed "Kanji_ID"
       , _kanjiRadicalRadical   = RadicalId $ fieldNamed "Radicals_ID"
      }
  , _jmdictKanjiStrokes = modifyTable (\_ -> "KanjiStrokes") $ tableModification
      {
         _kanjiStrokesId = fieldNamed "ID"
      }
  , _jmdictKanjiMeaning = modifyTable (\_ -> "KanjiMeaningSet") $ tableModification
      {
         _kanjiMeaningId = fieldNamed "ID"
       , _kanjiMeaningKanji = KanjiId $ fieldNamed "Kanji_ID"
       , _kanjiMeaningLanguage = fieldNamed "Language"
       , _kanjiMeaningMeaning = fieldNamed "Meaning"
      }
  , _jmdictVocabCategory = modifyTable (\_ -> "VocabCategorySet") $ tableModification
      {
         _vocabCategoryId = fieldNamed "ID"
       , _vocabCategoryName = fieldNamed "ShortName"
       , _vocabCategoryLabel = fieldNamed "Label"
      }
  , _jmdictVocabMeaning = modifyTable (\_ -> "VocabMeaningSet") $ tableModification
      {
         _vocabMeaningId  = fieldNamed "ID"
       , _vocabMeaningMeaning = fieldNamed "Meaning"
      }
  , _jmdictVocabVocabMeaning = modifyTable (\_ -> "VocabEntityVocabMeaning") $ tableModification
      {
         _vocabVocabMeaningVocab   = VocabId $ fieldNamed "VocabEntity_ID"
       , _vocabVocabMeaningMeaning = VocabMeaningId $ fieldNamed "Meanings_ID"
      }
  , _jmdictVocabMeaningVocabCategory = modifyTable (\_ -> "VocabMeaningVocabCategory") $ tableModification
      {
         _vocabMeaningVocabCategoryMeaning   = VocabMeaningId $ fieldNamed "VocabMeaningVocabCategory_VocabCategory_ID"
       ,  _vocabMeaningVocabCategoryCategory = VocabCategoryId $ fieldNamed "Categories_ID"
      }
  , _jmdictVocabVocabCategory = modifyTable (\_ -> "VocabCategoryVocabEntity") $ tableModification
      {
         _vocabVocabCategoryVocab   = VocabId $ fieldNamed "VocabCategoryVocabEntity_VocabCategory_ID"
       , _vocabVocabCategoryCategory = VocabCategoryId $ fieldNamed "Categories_ID"
      }
  , _jmdictVocabKanji = modifyTable (\_ -> "KanjiEntityVocabEntity") $ tableModification
      {
         _vocabKanjiVocab  = VocabId $ fieldNamed "Vocabs_ID"
       , _vocabKanjiKanji  = KanjiId $ fieldNamed "Kanji_ID"
      }
  }

srsDb :: DatabaseSettings be SrsDb
srsDb = defaultDbSettings `withDbModification`
  dbModification
  { srsDbTable = modifyTable (\_ -> "SrsEntrySetNew") $
      tableModification
      {
         _srsEntryId                = fieldNamed "ID"
       , _srsEntryCreationDate      = fieldNamed "CreationDate"
       , _srsEntryNextAnswerDate    = fieldNamed "NextAnswerDate"
       , _srsEntryMeanings          = fieldNamed "Meanings"
       , _srsEntryReadings          = fieldNamed "Readings"
       , _srsEntryCurrentGrade      = fieldNamed "CurrentGrade"
       , _srsEntryFailureCount      = fieldNamed "FailureCount"
       , _srsEntrySuccessCount      = fieldNamed "SuccessCount"
       , _srsEntryAssociatedVocab   = fieldNamed "AssociatedVocab"
       , _srsEntryAssociatedKanji   = fieldNamed "AssociatedKanji"
       , _srsEntryMeaningNote       = fieldNamed "MeaningNote"
       , _srsEntryReadingNote       = fieldNamed "ReadingNote"
       , _srsEntrySuspensionDate    = fieldNamed "SuspensionDate"
       , _srsEntryTags              = fieldNamed "Tags"
       , _srsEntryLastUpdateDate    = fieldNamed "LastUpdateDate"
       , _srsEntryIsDeleted         = fieldNamed "IsDeleted"
      }
  }
