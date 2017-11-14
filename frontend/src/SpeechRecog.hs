{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
module SpeechRecog where

import Protolude hiding (on)

import FrontendCommon
import qualified Data.Text as T

-- import GHCJS.DOM.SpeechRecognition
-- import GHCJS.DOM.SpeechRecognitionEvent
-- import GHCJS.DOM.SpeechGrammar
-- import GHCJS.DOM.EventM
import Data.JSString.Text

speechRecogWidget reading = do
  text "Speech Recog Widget"
  recordEv <- button ("Record")
  resEv <- speechRecogSetup recordEv

  respEv <- getWebSocketResponse $ CheckAnswer reading <$> resEv
  widgetHold (text "Waiting for Resp") $
    (\t -> text $ T.pack $ show t) <$> respEv
  ansDyn <- holdDyn (AnswerIncorrect "3") respEv
  sendEv <- button "Next"
  return $ (== AnswerCorrect) <$>
    tagPromptlyDyn ansDyn sendEv

type Result = [[(Double, T.Text)]]

speechRecogSetup :: MonadWidget t m
  => Event t () -> m (Event t Result)
speechRecogSetup startEv = do
  return never

--   (trigEv, trigAction) <- newTriggerEvent

--   recog <- liftIO $ do
--     recog <- newSpeechRecognition
--     recogGrammar <- newSpeechGrammarList
--     let grammar = "#JSGF V1.0; grammar phrases; public <phrase> = (けいい) ;" :: [Char]
--     addFromString recogGrammar grammar (Just 1.0)
--     setGrammars recog recogGrammar
--     setLang recog ("ja" :: [Char])
--     setMaxAlternatives recog 5

--     GHCJS.DOM.EventM.on recog result (onResultEv trigAction)
--     return recog

--   performEvent (startRecognition recog <$ startEv)

--   return trigEv

-- onResultEv :: (Result -> IO ())
--   -> EventM SpeechRecognition SpeechRecognitionEvent ()
-- onResultEv trigAction = do
--   ev <- ask

--   resL <- getResultList ev
--   len <- getResultListLength resL

--   let forM = flip mapM
--   result <- forM [0 .. (len - 1)] $ (\i -> do
--     res <- getResult resL i
--     altLen <- getResultLength res
--     forM [0 .. (altLen - 1)] $ (\j -> do
--       alt <- getAlternative res j
--       t <- getTranscript alt
--       c <- getConfidence alt
--       return (c, textFromJSString t)))

--   liftIO $ trigAction result
