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
default (T.Text)

main = shelly $ verbosely $ escaping False $ do
  -- let
  --   beBuildCmd = "nix-build -o backend-result -A ghc.backend"
  --   feBuildCmd = "nix-build -o frontend-result -A ghcjs.frontend"

  -- (be,fe) <- concurrently (shell beBuildCmd empty)
  --   (shell feBuildCmd empty)

  -- if not (be == ExitSuccess && fe == ExitSuccess)
  --   then putStrLn "Problem with nix-build"
  --   else do
  --     bePath <- realpath "backend-result"
  --     fePath <- realpath "frontend-result"
  --     putStrLn bePath
  --     putStrLn fePath

  withTmpDir $ \tmp -> do
    makeStaticDirContents exampleBePath exampleFePath tmp
    doDeploy exampleBePath tmp

  return ()

destHost = "tenjinreader.com"

exampleBePath :: FilePath
exampleBePath = "/nix/store/kwvhlj6a0bvapm3rhgkhd737j5jxmy60-server-0.1.0"
exampleFePath :: FilePath
exampleFePath = "/nix/store/l91myjy2xq3054hb5sgrbml4xf7j8911-frontend-0.1.0.0"

getHashFromPath :: FilePath -> Text
getHashFromPath bePath = beHash
  where
    (Just beHash) = T.stripSuffix "-server-0.1.0"
      =<< T.stripPrefix "/nix/store/" (toTextArg bePath)

makeStaticDirContents :: FilePath -> FilePath -> FilePath -> Sh ()
makeStaticDirContents bePath fePath deployDir = do
  let
    beHash = getHashFromPath bePath

    outJsFile = (beHash) <.> "js"

    makeFeCode (s,d) = do
      let dest = dirP </> d
          src = fePath </> s
          alljs = src </> "all.js"
          allextern = src </> "all.js.externs"
          outF = dest </> outJsFile
          ccCmdArgs :: [Text]
          ccCmdArgs =
            [ " --compilation_level ADVANCED "
            , " --js_output_file " <> toTextArg outF
            , " --externs " <> toTextArg allextern
            , " --js " <> toTextArg alljs]

      mkdir dest
      run_ closureCompiler ccCmdArgs
      cp indexHtml dest

    dirP = deployDir </> "static" </> "app"
    indexHtml = (deployDir </> "index.html")

  mkdir_p dirP
  makeIndexHtml outJsFile indexHtml
  mapM_ makeFeCode feDestPaths

  run_ "ln" ["-s", toTextArg bePath, toTextArg (deployDir </> "server")]

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
    inpFile = "index.html.template"
  cp inpFile outF
  run_ "sed" ["-i", r, toTextArg outF]
