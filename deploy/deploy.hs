#! /usr/bin/env nix-shell
#! nix-shell -i runghc -p 'ghc.withPackages (self: with self; [ shelly ])'

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Shelly
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import Prelude hiding (FilePath)
import Data.Monoid ((<>))
import Control.Concurrent.Async
default (T.Text)

main = shelly $ verbosely $ escaping False $ do
  let
    beRes = "backend-result"
    feRes = "frontend-result"

  beBuild <- asyncSh $ run "nix-build" ["-o", toTextArg beRes, "-A", "ghc.backend"]
  run "nix-build" ["-o", toTextArg feRes, "-A", "ghcjs.frontend"]
  liftIO $ wait (beBuild)

  bePath <- canonic beRes
  fePath <- canonic feRes

  withTmpDir $ \tmp -> do
    makeStaticDirContents bePath fePath tmp
    doDeploy bePath tmp

  return ()

destHost = "tenjinreader.com"

getHashFromPath :: FilePath -> Text
getHashFromPath bePath = beHash
  where
    (Just beHash) = T.stripSuffix "-server-0.1.0"
      =<< T.stripPrefix "/nix/store/" (toTextArg bePath)

makeStaticDirContents :: FilePath -> FilePath -> FilePath -> Sh ()
makeStaticDirContents bePath fePath deployDir = do
  let
    beHash = getHashFromPath bePath
    wkjs = "deploy" </> "wanakana.min.js"

    outJsFile = (beHash) <.> "js"

    makeFeCode (s,d) = do
      let dest = dirP </> d
          src = fePath </> s
          alljs = src </> "all.js"
          allextern = src </> "all.js.externs"
          runmainjs = src </> "runmain.js"
          outF = dest </> outJsFile
          ccCmdArgs :: [Text]
          ccCmdArgs =
            [ " --js_output_file " <> toTextArg outF
            , " --compilation_level ADVANCED "
            , " --externs " <> toTextArg allextern
            , " --js " <> toTextArg alljs]

      mkdir dest
      run_ closureCompiler ccCmdArgs
      cp indexHtml dest
      cp wkjs dest
      cp runmainjs dest

    dirP = deployDir </> "static" </> "app"
    indexHtml = (deployDir </> "index.html")

  mkdir_p dirP
  makeIndexHtml outJsFile indexHtml
  mapM_ makeFeCode feDestPaths

  run_ "ln" ["-s", toTextArg bePath, toTextArg (deployDir </> "server")]
  run_ "git" ["archive", "-o", toTextArg (deployDir </> "source.tar.gz"), "HEAD"]

doDeploy :: FilePath -> FilePath -> Sh ()
doDeploy bePath deployDir = do
  let
    beHash = getHashFromPath bePath
    targetDir = destHost <> ":~/" <> beHash

  run_ "nix-copy-closure" ["--sign", "--include-outputs", destHost, toTextArg bePath]
  run_ "rsync" ["-az", toTextArg deployDir, targetDir]

closureCompiler :: FilePath
closureCompiler = "/nix/store/n7h36mmdgd9y5g59qasznjvsl3psg0g2-closure-compiler-20170218/bin/closure-compiler"

feDestPaths :: [(FilePath, FilePath)]
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
  let
    r = "s@all_cc_js_name@" <> (toTextArg jsName) <> "@"
    inpFile = "deploy/index.html.template"
  cp inpFile outF
  run_ "sed" ["-i", r, toTextArg outF]
