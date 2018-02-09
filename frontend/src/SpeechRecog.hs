{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
module SpeechRecog
  (speechRecogSetup, Result
  , initWanakaBindFn, bindWanaKana)
  where

import Protolude hiding (on)
import Control.Lens
import FrontendCommon
import qualified Data.Text as T
import Data.JSString.Text

#if defined (ghcjs_HOST_OS)
import GHCJS.DOM.SpeechRecognition
import GHCJS.DOM.SpeechRecognitionEvent
import GHCJS.DOM.SpeechGrammar
import GHCJS.DOM.EventM
import Language.Javascript.JSaddle.Object
import Language.Javascript.JSaddle.Types
#endif

-- speechRecogWidget readings = do
--   -- TODO Do check on recogEv before this
--   respEv <- getWebSocketResponse $ CheckAnswer readings <$> recogEv
--   widgetHold (text "Waiting for Resp") $
--     (\t -> text $ T.pack $ show t) <$> respEv
--   ansDyn <- holdDyn (AnswerIncorrect "3") respEv
--   sendEv <- button "Next"
--   return $ (== AnswerCorrect) <$>
--     tagPromptlyDyn ansDyn sendEv

type Result = [[(Double, T.Text)]]

-- CPP has issues with multi-line literal, so move all ifdef here
initWanakaBindFn :: (MonadWidget t m) => m ()
initWanakaBindFn =
#if defined (ghcjs_HOST_OS)
  void $ liftJSM $ eval ("globalFunc = function () {"
                <> "var input = document.getElementById('JP-TextInput-IME-Input');"
                <> "wanakana.bind(input);}" :: Text)
#else
  return ()
#endif

bindWanaKana :: (MonadWidget t m) => m ()
bindWanaKana =
#if defined (ghcjs_HOST_OS)
        void $ liftJSM $
          jsg0 ("globalFunc" :: Text)
#else
  return ()
#endif

speechRecogSetup :: MonadWidget t m
  => m (Event t () -> Event t () -> m (Event t Result, Event t (), Event t (), Event t ()))
speechRecogSetup = do
#if defined (ghcjs_HOST_OS)
  (trigEv, trigAction) <- newTriggerEvent
  (speechStartEv, speechStartAction) <- newTriggerEvent
  (errorEv, errorAction) <- newTriggerEvent
  (timeoutEv, timeoutAction) <- newTriggerEvent

  recog <- liftIO $ do
    recog <- newSpeechRecognition
    -- Grammar does not work on chrome atleast
    recogGrammar <- newSpeechGrammarList
    let grammar = "#JSGF V1.0; grammar phrases; public <phrase> = (けいい) ;" :: [Char]
    addFromString recogGrammar grammar (Just 1.0)
    setGrammars recog recogGrammar
    setLang recog ("ja" :: [Char])
    setMaxAlternatives recog 5

    GHCJS.DOM.EventM.on recog result (onResultEv trigAction)
    GHCJS.DOM.EventM.on recog speechstart (liftIO $ speechStartAction ())
    GHCJS.DOM.EventM.on recog end (liftIO $ timeoutAction ())
    GHCJS.DOM.EventM.on recog nomatch (liftIO $ timeoutAction ())
    GHCJS.DOM.EventM.on recog audioend (liftIO $ timeoutAction ())
    GHCJS.DOM.EventM.on recog soundend (liftIO $ timeoutAction ())
    GHCJS.DOM.EventM.on recog GHCJS.DOM.SpeechRecognition.error (liftIO $ errorAction ())
    return recog

  return (\stop e -> do
    ed <- delay 0.2 e
    performEvent ((abort recog) <$ (leftmost [e,stop]))
    performEvent ((startRecognition recog) <$ ed)
    return (trigEv, speechStartEv, errorEv, timeoutEv))
#else
  return (const $ const (return (never, never, never, never)))
#endif

#if defined (ghcjs_HOST_OS)
onResultEv :: (Result -> IO ())
  -> EventM SpeechRecognition SpeechRecognitionEvent ()
onResultEv trigAction = do
  ev <- ask

  resL <- getResultList ev
  len <- getResultListLength resL

  let forM = flip mapM
  result <- forM [0 .. (len - 1)] $ (\i -> do
    res <- getResult resL i
    altLen <- getResultLength res
    forM [0 .. (altLen - 1)] $ (\j -> do
      alt <- getAlternative res j
      t <- getTranscript alt
      c <- getConfidence alt
      return (c, textFromJSString t)))

  liftIO $ trigAction result
#endif
