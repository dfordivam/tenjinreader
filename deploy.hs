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

main = shelly $ do
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

  makeStaticDirContents exampleBePath exampleFePath
  doDeploy exampleBePath

  return ()

destHost = "tenjinreader.com"
deployDir = "deploy-outputs"
indexHtml = deployDir </> "index.html"

exampleBePath :: FilePath
exampleBePath = "/nix/store/kwvhlj6a0bvapm3rhgkhd737j5jxmy60-server-0.1.0"
exampleFePath :: FilePath
exampleFePath = "/nix/store/l91myjy2xq3054hb5sgrbml4xf7j8911-frontend-0.1.0.0"

getHashFromPath :: FilePath -> Text
getHashFromPath bePath = beHash
  where
    (Just beHash) = T.stripSuffix "-server-0.1.0"
      =<< T.stripPrefix "/nix/store/" (toTextArg bePath)

makeStaticDirContents :: FilePath -> FilePath -> Sh ()
makeStaticDirContents bePath fePath = do
  let
    beHash = getHashFromPath bePath

    outJsFile = (beHash) <.> "js"

    makeFeCode (s,d) = do
      let dest = appDir </> d
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

      -- mkdir dest
      trace $ "Creating : '" <> toTextArg outF <> "', from '" <> toTextArg src <> "'."

      trace $ mconcat ccCmdArgs
      -- shell ccCmd empty

      -- cp indexHtml dest

    stDir = (deployDir  <> "/static")
    appDir = stDir <> "/app"

  makeIndexHtml outJsFile indexHtml
  -- mkdir deployDir
  -- mkdir stDir
  -- mkdir appDir
  mapM_ makeFeCode feDestPaths

  let
    lnCmd = "ln -s " <> toTextArg bePath <> " " <> toTextArg deployDir <> "/server"
  trace lnCmd
  -- shell lnCmd empty

doDeploy :: FilePath -> Sh ()
doDeploy bePath = do
  let
    beHash = getHashFromPath bePath
    ccCmd = "nix-copy-closure --sign --include-outputs " <> toTextArg destHost <> " " <> toTextArg bePath
    targetDir = destHost <> ":~/" <> beHash
    scpCmd = "scp -r " <> toTextArg deployDir <> " " <> toTextArg targetDir
  trace ccCmd
  -- shell ccCmd empty

  trace scpCmd
  -- shell scpCmd empty

closureCompilerPath :: FilePath
closureCompilerPath = "/nix/store/n7h36mmdgd9y5g59qasznjvsl3psg0g2-closure-compiler-20170218/bin/closure-compiler"

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
  let sedCmd = "sed \"s@all_cc_js_name@" <>
        (toTextArg jsName) <> "@\" index.html.template > " <> toTextArg outF
  trace $ "Executing: " <> sedCmd
  -- shell sedCmd empty
