{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module KanjiDB.Model where

import Protolude
import Database.Beam
import Control.Lens.TH
import Data.Coerce
import Data.Time (UTCTime)

class UniqKey k where
  getKey :: (Coercible Int t) => k -> Maybe t
  makeKey :: (Coercible Int t) => Maybe t -> k

getKeys :: (UniqKey k, Coercible Int t) => [k] -> [t]
getKeys ks = catMaybes (map  getKey ks)

makeKeys :: (UniqKey k, Coercible Int t) => [t] -> [k]
makeKeys = map (makeKey . Just)

----------------------------------------------------------------------
data KanjiT f = Kanji {
    _kanjiId             :: C f (Auto Int)
  , _kanjiCharacter      :: C f (Text)
  , _kanjiStrokeCount    :: C f (Maybe Int)
  , _kanjiGrade          :: C f (Maybe Int)
  , _kanjiMostUsedRank   :: C f (Maybe Int)
  , _kanjiJlptLevel      :: C f (Maybe Int)
  , _kanjiOnyomi         :: C f (Maybe Text)
  , _kanjiKunyomi        :: C f (Maybe Text)
  , _kanjiNanori         :: C f (Maybe Text)
  , _kanjiUnicodeValue   :: C f (Int)
  , _kanjiNewpaperRank   :: C f (Maybe Int)
  , _kanjiWkLevel        :: C f (Maybe Int)
  }
  deriving (Generic)

makeLenses ''KanjiT

type Kanji = KanjiT Identity
deriving instance Show Kanji
deriving instance Show KanjiId
deriving instance Ord KanjiId
deriving instance Eq KanjiId

instance Table KanjiT where

  data PrimaryKey KanjiT f = KanjiId (Columnar f (Auto Int)) deriving Generic
  primaryKey = KanjiId . _kanjiId

type KanjiId = PrimaryKey KanjiT Identity

instance UniqKey KanjiId where
  getKey (KanjiId k) = coerce k
  makeKey t = KanjiId (coerce t)

instance Beamable KanjiT
instance Beamable (PrimaryKey KanjiT)

----------------------------------------------------------------------
data VocabT f = Vocab {
    _vocabId             :: C f (Auto Int)
  , _vocabKanjiWriting   :: C f (Maybe Text)
  , _vocabKanaWriting    :: C f (Text)
  , _vocabIsCommon       :: C f (Bool)
  , _vocabFreqRank       :: C f (Maybe Int)
  , _vocabFurigana       :: C f (Maybe Text)
  , _vocabJlptLevel      :: C f (Maybe Int)
  , _vocabWkLevel        :: C f (Maybe Int)
  , _vocabWikiRank       :: C f (Maybe Int)
  , _vocabGroupId        :: C f (Int)
  , _vocabIsMain         :: C f (Bool)
  }
  deriving (Generic)

makeLenses ''VocabT

type Vocab = VocabT Identity
deriving instance Show Vocab
deriving instance Show VocabId
deriving instance Ord VocabId
deriving instance Eq VocabId

instance Table VocabT where
    data PrimaryKey VocabT f = VocabId (Columnar f (Auto Int)) deriving Generic
    primaryKey = VocabId . _vocabId

type VocabId = PrimaryKey VocabT Identity

instance UniqKey VocabId where
  getKey (VocabId k) = coerce k
  makeKey t = VocabId (coerce t)

instance Beamable VocabT
instance Beamable (PrimaryKey VocabT)

----------------------------------------------------------------------
data RadicalT f = Radical {
    _radicalId             :: C f (Auto Int)
  , _radicalCharacter      :: C f (Text)
  }
  deriving (Generic)

makeLenses ''RadicalT

type Radical = RadicalT Identity
deriving instance Show Radical
deriving instance Show RadicalId
deriving instance Eq RadicalId
deriving instance Ord RadicalId

instance Table RadicalT where
    data PrimaryKey RadicalT f = RadicalId (Columnar f (Auto Int)) deriving Generic
    primaryKey = RadicalId . _radicalId

type RadicalId = PrimaryKey RadicalT Identity

instance UniqKey RadicalId where
  getKey (RadicalId k) = coerce k
  makeKey t = RadicalId (coerce t)

instance Beamable RadicalT
instance Beamable (PrimaryKey RadicalT)

----------------------------------------------------------------------
data KanjiRadicalT f = KanjiRadical {
    _kanjiRadicalKanji             :: (PrimaryKey KanjiT f)
  , _kanjiRadicalRadical           :: (PrimaryKey RadicalT f)
  }
  deriving (Generic)

makeLenses ''KanjiRadicalT

type KanjiRadical = KanjiRadicalT Identity
deriving instance Show KanjiRadical
deriving instance Show KanjiRadicalId

instance Table KanjiRadicalT where
    data PrimaryKey KanjiRadicalT f =
      KanjiRadicalId (PrimaryKey KanjiT f) (PrimaryKey RadicalT f) deriving Generic
    primaryKey = KanjiRadicalId <$> _kanjiRadicalKanji <*> _kanjiRadicalRadical

type KanjiRadicalId = PrimaryKey KanjiRadicalT Identity

instance Beamable KanjiRadicalT
instance Beamable (PrimaryKey KanjiRadicalT)

----------------------------------------------------------------------
-- TODO - Implement SVG
data KanjiStrokesT f = KanjiStrokes {
    _kanjiStrokesId             :: C f (Auto Int)
  }
  deriving (Generic)

makeLenses ''KanjiStrokesT

type KanjiStrokes = KanjiStrokesT Identity
deriving instance Show KanjiStrokes
deriving instance Show KanjiStrokesId

instance Table KanjiStrokesT where
    data PrimaryKey KanjiStrokesT f =
      KanjiStrokesId (Columnar f (Auto Int)) deriving Generic
    primaryKey = KanjiStrokesId . _kanjiStrokesId

type KanjiStrokesId = PrimaryKey KanjiStrokesT Identity

instance Beamable KanjiStrokesT
instance Beamable (PrimaryKey KanjiStrokesT)

-- KanjiStrokesDB                sql=KanjiStrokes
--     Id                        sql=ID
--     framesSvg     ByteString  sql=FramesSvg

----------------------------------------------------------------------
data KanjiMeaningT f = KanjiMeaning {
    _kanjiMeaningId         :: C f (Auto Int)
  , _kanjiMeaningKanji      :: (PrimaryKey KanjiT f)
  , _kanjiMeaningLanguage   :: C f (Maybe Text)
  , _kanjiMeaningMeaning    :: C f (Text)
  }
  deriving (Generic)

makeLenses ''KanjiMeaningT

type KanjiMeaning = KanjiMeaningT Identity
deriving instance Show KanjiMeaning
deriving instance Show KanjiMeaningId

instance Table KanjiMeaningT where
    data PrimaryKey KanjiMeaningT f = KanjiMeaningId (Columnar f (Auto Int)) deriving Generic
    primaryKey = KanjiMeaningId . _kanjiMeaningId

type KanjiMeaningId = PrimaryKey KanjiMeaningT Identity

instance Beamable KanjiMeaningT
instance Beamable (PrimaryKey KanjiMeaningT)

-- KanjiMeaningDB                sql=KanjiMeaningSet
--     Id                        sql=ID
--     kanji         KanjiId     sql=Kanji_ID
--     language      Text Maybe  sql=Language
--     meaning       Text        sql=Character
--     deriving Show

----------------------------------------------------------------------
data VocabCategoryT f = VocabCategory {
    _vocabCategoryId         :: C f (Auto Int)
  , _vocabCategoryName       :: C f (Text)
  , _vocabCategoryLabel      :: C f (Text)
  }
  deriving (Generic)

makeLenses ''VocabCategoryT

type VocabCategory = VocabCategoryT Identity
deriving instance Show VocabCategory
deriving instance Show VocabCategoryId

instance Table VocabCategoryT where
    data PrimaryKey VocabCategoryT f = VocabCategoryId (Columnar f (Auto Int)) deriving Generic
    primaryKey = VocabCategoryId . _vocabCategoryId

type VocabCategoryId = PrimaryKey VocabCategoryT Identity

instance Beamable VocabCategoryT
instance Beamable (PrimaryKey VocabCategoryT)

-- VocabCategoryDB               sql=VocabCategorySet
--     Id                        sql=ID
--     name          Text        sql=ShortName
--     label         Text        sql=Label
--     deriving Show

----------------------------------------------------------------------
data VocabMeaningT f = VocabMeaning {
    _vocabMeaningId         :: C f (Auto Int)
  , _vocabMeaningMeaning    :: C f (Text)
  }
  deriving (Generic)

makeLenses ''VocabMeaningT

type VocabMeaning = VocabMeaningT Identity
deriving instance Show VocabMeaning
deriving instance Show VocabMeaningId

instance Table VocabMeaningT where
    data PrimaryKey VocabMeaningT f = VocabMeaningId (Columnar f (Auto Int)) deriving Generic
    primaryKey = VocabMeaningId . _vocabMeaningId

type VocabMeaningId = PrimaryKey VocabMeaningT Identity

instance Beamable VocabMeaningT
instance Beamable (PrimaryKey VocabMeaningT)
-- VocabMeaningDB                sql=VocabMeaningSet
--     Id                        sql=ID
--     meaning       Text        sql=Meaning
--     deriving Show

----------------------------------------------------------------------
data VocabVocabMeaningT f = VocabVocabMeaning {
    _vocabVocabMeaningVocab             :: (PrimaryKey VocabT f)
  , _vocabVocabMeaningMeaning           :: (PrimaryKey VocabMeaningT f)
  }
  deriving (Generic)

makeLenses ''VocabVocabMeaningT

type VocabVocabMeaning = VocabVocabMeaningT Identity
deriving instance Show VocabVocabMeaning
deriving instance Show VocabVocabMeaningId

instance Table VocabVocabMeaningT where
    data PrimaryKey VocabVocabMeaningT f =
      VocabVocabMeaningId (PrimaryKey VocabT f) (PrimaryKey VocabMeaningT f) deriving Generic
    primaryKey = VocabVocabMeaningId <$> _vocabVocabMeaningVocab <*> _vocabVocabMeaningMeaning

type VocabVocabMeaningId = PrimaryKey VocabVocabMeaningT Identity

instance Beamable VocabVocabMeaningT
instance Beamable (PrimaryKey VocabVocabMeaningT)
-- VocabEntityVocabMeaningDB     sql=VocabEntityVocabMeaning
--     vocab         VocabId     sql=VocabEntity_ID
--     meaning    VocabMeaningId sql=Meanings_ID
--     Primary vocab meaning
--     deriving Show

----------------------------------------------------------------------
data VocabMeaningVocabCategoryT f = VocabMeaningVocabCategory {
    _vocabMeaningVocabCategoryMeaning           :: (PrimaryKey VocabMeaningT f)
  , _vocabMeaningVocabCategoryCategory          :: (PrimaryKey VocabCategoryT f)
  }
  deriving (Generic)

makeLenses ''VocabMeaningVocabCategoryT

type VocabMeaningVocabCategory = VocabMeaningVocabCategoryT Identity
deriving instance Show VocabMeaningVocabCategory
deriving instance Show VocabMeaningVocabCategoryId

instance Table VocabMeaningVocabCategoryT where
    data PrimaryKey VocabMeaningVocabCategoryT f =
      VocabMeaningVocabCategoryId (PrimaryKey VocabMeaningT f) (PrimaryKey VocabCategoryT f) deriving Generic
    primaryKey = VocabMeaningVocabCategoryId <$> _vocabMeaningVocabCategoryMeaning <*> _vocabMeaningVocabCategoryCategory

type VocabMeaningVocabCategoryId = PrimaryKey VocabMeaningVocabCategoryT Identity

instance Beamable VocabMeaningVocabCategoryT
instance Beamable (PrimaryKey VocabMeaningVocabCategoryT)
-- VocabMeaningVocabCategoryDB   sql=VocabMeaningVocabCategory
--     meaning   VocabMeaningId  sql=VocabMeaningVocabCategory_VocabCategory_ID
--     category  VocabCategoryId sql=Categories_ID
--     Primary meaning category
--     deriving Show

----------------------------------------------------------------------
data VocabVocabCategoryT f = VocabVocabCategory {
    _vocabVocabCategoryVocab             :: (PrimaryKey VocabT f)
  , _vocabVocabCategoryCategory          :: (PrimaryKey VocabCategoryT f)
  }
  deriving (Generic)

makeLenses ''VocabVocabCategoryT

type VocabVocabCategory = VocabVocabCategoryT Identity
deriving instance Show VocabVocabCategory
deriving instance Show VocabVocabCategoryId

instance Table VocabVocabCategoryT where
    data PrimaryKey VocabVocabCategoryT f =
      VocabVocabCategoryId (PrimaryKey VocabT f) (PrimaryKey VocabCategoryT f) deriving Generic
    primaryKey = VocabVocabCategoryId <$> _vocabVocabCategoryVocab <*> _vocabVocabCategoryCategory

type VocabVocabCategoryId = PrimaryKey VocabVocabCategoryT Identity

instance Beamable VocabVocabCategoryT
instance Beamable (PrimaryKey VocabVocabCategoryT)
-- VocabCategoryVocabEntityDB    sql=VocabCategoryVocabEntity
--     vocab     VocabId         sql=VocabCategoryVocabEntity_VocabCategory_ID
--     category  VocabCategoryId sql=Categories_ID
--     Primary category vocab
--     deriving Show

----------------------------------------------------------------------
data VocabKanjiT f = VocabKanji {
    _vocabKanjiVocab             :: (PrimaryKey VocabT f)
  , _vocabKanjiKanji             :: (PrimaryKey KanjiT f)
  }
  deriving (Generic)

makeLenses ''VocabKanjiT

type VocabKanji = VocabKanjiT Identity
deriving instance Show VocabKanji
deriving instance Show VocabKanjiId

instance Table VocabKanjiT where
    data PrimaryKey VocabKanjiT f =
      VocabKanjiId (PrimaryKey VocabT f) (PrimaryKey KanjiT f) deriving Generic
    primaryKey = VocabKanjiId <$> _vocabKanjiVocab <*> _vocabKanjiKanji

type VocabKanjiId = PrimaryKey VocabKanjiT Identity

instance Beamable VocabKanjiT
instance Beamable (PrimaryKey VocabKanjiT)
-- KanjiEntityVocabEntityDB      sql=KanjiEntityVocabEntity
--     vocab         VocabId     sql=Vocabs_ID
--     kanji         KanjiId     sql=Kanji_ID
--     Primary kanji vocab
--     deriving Show

----------------------------------------------------------------------
data SrsEntryT f = SrsEntry {
    _srsEntryId               :: C f (Auto Int)
  , _srsEntryCreationDate     :: C f (UTCTime)
  , _srsEntryNextAnswerDate   :: C f (Maybe UTCTime)
  , _srsEntryMeanings         :: C f (Text)
  , _srsEntryReadings         :: C f (Text)
  , _srsEntryCurrentGrade     :: C f (Int)
  , _srsEntryFailureCount     :: C f (Int)
  , _srsEntrySuccessCount     :: C f (Int)
  , _srsEntryAssociatedVocab  :: C f (Maybe Text)
  , _srsEntryAssociatedKanji  :: C f (Maybe Text)
  , _srsEntryMeaningNote      :: C f (Maybe Text)
  , _srsEntryReadingNote      :: C f (Maybe Text)
  , _srsEntrySuspensionDate   :: C f (Maybe UTCTime)
  , _srsEntryTags             :: C f (Maybe Text)
  , _srsEntryLastUpdateDate   :: C f (Maybe UTCTime)
  , _srsEntryIsDeleted        :: C f (Bool)
  }
  deriving (Generic)

makeLenses ''SrsEntryT

type SrsEntry = SrsEntryT Identity
deriving instance Show SrsEntry
deriving instance Show SrsEntryId
deriving instance Ord SrsEntryId
deriving instance Eq SrsEntryId

instance Table SrsEntryT where
    data PrimaryKey SrsEntryT f = SrsEntryId (Columnar f (Auto Int)) deriving Generic
    primaryKey = SrsEntryId . _srsEntryId

type SrsEntryId = PrimaryKey SrsEntryT Identity

instance UniqKey SrsEntryId where
  getKey (SrsEntryId k) = coerce k
  makeKey t = SrsEntryId (coerce t)

instance Beamable SrsEntryT
instance Beamable (PrimaryKey SrsEntryT)

----------------------------------------------------------------------
