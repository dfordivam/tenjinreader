#! /usr/bin/env nix-shell
#! nix-shell -i runghc -p 'ghc.withPackages (self: with self; [ turtle directory async text])'

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
import Turtle
import System.Directory
import Control.Concurrent.Async
import Data.Text as T
import Data.Text.IO as T

main = do
  -- let
  --   beBuildCmd = "nix-build -o backend-result -A ghc.backend"
  --   feBuildCmd = "nix-build -o frontend-result -A ghcjs.frontend"

  -- (be,fe) <- concurrently (shell beBuildCmd empty)
  --   (shell feBuildCmd empty)

  -- if not (be == ExitSuccess && fe == ExitSuccess)
  --   then putStrLn "Problem with nix-build"
  --   else do
  --     bePath <- canonicalizePath "backend-result"
  --     fePath <- canonicalizePath "frontend-result"
  --     putStrLn bePath
  --     putStrLn fePath

  makeStaticDirContents exampleBePath exampleFePath
  doDeploy exampleBePath

  return ()

destHost = "tenjinreader.com"
deployDir = "deploy-outputs"

exampleBePath :: Text
exampleBePath = "/nix/store/kwvhlj6a0bvapm3rhgkhd737j5jxmy60-server-0.1.0"
exampleFePath :: Text
exampleFePath = "/nix/store/l91myjy2xq3054hb5sgrbml4xf7j8911-frontend-0.1.0.0"

makeStaticDirContents bePath fePath = do
  let
    (Just beHash) = T.stripSuffix "-server-0.1.0" =<< T.stripPrefix "/nix/store/" bePath
    outJsFile = beHash <> ".js"

    makeFeCode (s,d) = do
      let dest = appDir <> "/" <> d
          src = fePath <> "/" <> s
          alljs = src <> "/all.js"
          allextern = src <> "/all.js.externs"
          outF = dest <> "/" <> outJsFile
          cpCmd = "cp " <> indexHtml <> " " <> dest
          ccCmd = closureCompilerPath <> "/bin/closure-compiler"
            <> " --compilation_level ADVANCED "
            <> " --js_output_file " <> outF
            <> " --externs " <> allextern
            <> " --js " <> alljs
      T.putStrLn $ "Creating : '" <> outF <> "', from '" <> src <> "'."
      T.putStrLn ccCmd
      T.putStrLn cpCmd

    stDir = (deployDir  <> "/static")
    appDir = stDir <> "/app"
    indexHtml = deployDir <> "/index.html"

  makeIndexHtml outJsFile indexHtml
  -- mkdir deployDir
  -- mkdir stDir
  -- mkdir appDir
  mapM_ makeFeCode feDestPaths

  let
    lnCmd = "ln -s " <> bePath <> " " <> deployDir <> "/server"
  T.putStrLn lnCmd

doDeploy bePath = do
  let
    (Just beHash) = T.stripSuffix "-server-0.1.0" =<< T.stripPrefix "/nix/store/" bePath
    ccCmd = "nix-copy-closure --sign --include-outputs " <> destHost <> " " <> bePath
    targetDir = destHost <> ":~/" <> beHash
    scpCmd = "scp -r " <> deployDir <> " " <> targetDir
  T.putStrLn ccCmd
  T.putStrLn scpCmd

closureCompilerPath = "/nix/store/n7h36mmdgd9y5g59qasznjvsl3psg0g2-closure-compiler-20170218"

feDestPaths :: [(Text,Text)]
feDestPaths =
  [ ("bin/frontend.jsexe"
   , "complete-norecog")
  , ("bin/frontend-srs-only.jsexe"
   , "srsonly-norecog")
  , ("bin/frontend-recog.jsexe"
   , "complete")
  , ("bin/frontend-reader-only.jsexe"
   , "readeronly")
  , ("bin/frontend-srs-only-recog.jsexe"
   , "srsonly")]

makeIndexHtml jsName outF = do
  let sedCmd = "sed \"s@all_cc_js_name@" <> jsName <> "@\" index.html.template > " <> outF
  T.putStrLn $ "Executing: " <> sedCmd
