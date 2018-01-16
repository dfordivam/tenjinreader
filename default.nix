{}:

(import ./reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    common = ./common;
    backend = ./backend;
    kanjidbreader = ./backend/kanjiDbReader;
    jmdict-xml-parser = ./backend/jmdict-parser;
    jmdict-ast = ./common/jmdict-ast;
    frontend = ./frontend;
    hs-nlp-jp-utils = ./common/hs-nlp-jp-utils;
  };

  android.frontend = {
    executableName = "frontend";
    applicationId = "org.example.frontend";
    displayName = "Example Android App";
  };

  shells = {
    ghc = ["common" "backend" "frontend" "kanjidbreader" "jmdict-xml-parser"];
    ghcjs = ["common" "frontend"];
  };

  overrides = self: super:
      let
        r-all = import (pkgs.fetchFromGitHub {
          owner = "dfordivam";
          repo = "reflex-websocket-interface";
          rev = "779b7b296fef89b2a686aa4342b93a294871d351";
          sha256 = "0s6dd3ls9wniqwvpz62ia0iqnl48543l8h1fxqxn4x4pjhm709kn";
        }) self;
        beam-src = pkgs.fetchFromGitHub {
            owner = "tathougies";
            repo = "beam";
            rev = "abe473a5dc8456c611f84016c53a493f57442484";
            sha256 = "0w7rhw7pgl4vvlfh8n7v4sdia1ncbljlawigs76gabc1mmlnhbx7";
          };
      in rec {
        yesod-auth-oauth2 = pkgs.haskell.lib.doJailbreak
          (self.callCabal2nix "yesod-auth-oauth2" (pkgs.fetchFromGitHub {
          owner = "thoughtbot";
          repo = "yesod-auth-oauth2";
          rev = "937ad572a31dcbb2d5fe75b91f689c7a2f6ecfca";
          sha256 = "0ai4axdh5x87qrkzn6a9i0a2srs6gkjbbiywihrv7ydmh2caqjya";
        }) {});

        mecab = pkgs.haskell.lib.doJailbreak super.mecab;
        reflex-websocket-interface = r-all.reflex-websocket-interface;

        reflex-websocket-interface-shared = r-all.reflex-websocket-interface-shared;

        reflex-websocket-interface-server = r-all.reflex-websocket-interface-server;

        beam-core = self.callCabal2nix "beam-core" "${beam-src}/beam-core" {};
        beam-sqlite = self.callCabal2nix "beam-core" "${beam-src}/beam-sqlite" {};
        beam-migrate = self.callCabal2nix "beam-core" "${beam-src}/beam-migrate" {};

        frontend = pkgs.haskell.lib.dontHaddock super.frontend;
        backend = pkgs.haskell.lib.dontHaddock super.backend;
    };

})
