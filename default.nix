{}:

(import ./reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    common = ./common;
    backend = ./backend;
    kanjidbreader = ./backend/kanjiDbReader;
    frontend = ./frontend;
    frontend-android = ./android;
  };

  android.frontend-android = {
    executableName = "frontend-android";
    applicationId = "org.example.frontend";
    displayName = "Example Android App";
  };

  shells = {
    ghc = ["common" "backend" "frontend" "frontend-android" "kanjidbreader"];
    ghcjs = ["common" "frontend"];
  };

  overrides = self: super:
      let
        r-all =
          import (pkgs.fetchFromGitHub {
            owner = "dfordivam";
            repo = "reflex-websocket-interface";
            rev = "160c2da048b63f5b38343b8a06fc0d426796f66e";
            sha256 = "1gf7rjqgkzvs28gjmg4ssp9149kyyq9rsx9f2z0qwpsjkg35pqgz";
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
        yesod-auth-oauth = pkgs.haskell.lib.doJailbreak super.yesod-auth-oauth;
        reflex-websocket-interface = r-all.reflex-websocket-interface;

        reflex-websocket-interface-shared = r-all.reflex-websocket-interface-shared;

        reflex-websocket-interface-server = r-all.reflex-websocket-interface-server;

        beam-core = self.callCabal2nix "beam-core" "${beam-src}/beam-core" {};
        beam-sqlite = self.callCabal2nix "beam-core" "${beam-src}/beam-sqlite" {};
        beam-migrate = self.callCabal2nix "beam-core" "${beam-src}/beam-migrate" {};

        haskey = self.callCabal2nix "haskey" 
          (pkgs.fetchFromGitHub {
            owner = "haskell-haskey";
            repo = "haskey";
            rev = "00c96fc30d45aa0e3c218aa4f6780a463cec8c89";
            sha256 = "1xbghqqrgkk454vmw24pxijhj7k3fnv6hnf928ba7i75fhxkqpfn";
          }) {};

        haskey-mtl = self.callCabal2nix "haskey" 
          (pkgs.fetchFromGitHub {
            owner = "haskell-haskey";
            repo = "haskey-mtl";
            rev = "d7ccc93e4763f7c3f131fd92bba5dbbbb9279099";
            sha256 = "04ax88rzjbjap8hl4q8y4rhhd4bfa5mz2jlfj6sps1jjp0d7dd9z";
          }) {inherit haskey haskey-btree;};

        haskey-btree = self.callCabal2nix "haskey" 
          (pkgs.fetchFromGitHub {
            owner = "haskell-haskey";
            repo = "haskey-btree";
            rev = "4cca2f924ab4ea0b6f576aa5ef1124f3330b3d3e";
            sha256 = "01qmwjzhjdwnqwfwk07irfzfnx75rmf351kvjnvjx16j083xji4b";
          }) {};

        frontend = pkgs.haskell.lib.dontHaddock super.frontend;
        backend = pkgs.haskell.lib.dontHaddock super.backend;
        common = pkgs.haskell.lib.dontHaddock super.common;
        kanjidbreader = pkgs.haskell.lib.dontHaddock super.kanjidbreader;

        jmdict-ast = self.callCabal2nix "jmdict-ast"
          (pkgs.fetchFromGitHub {
            owner = "dfordivam";
            repo = "jmdict-ast";
            rev = "60af06562040f014b38fbbd180f87243ebacdfd1";
            sha256 = "18gb7knjh7var5l418mzb8bd66mh5ji6bxlbczcghz0k4jh61zhh";
          }) {};

        jmdict-xml-parser = self.callCabal2nix "jmdict-xml-parser"
          (pkgs.fetchFromGitHub {
            owner = "dfordivam";
            repo = "jmdict-parser";
            rev = "fc1d700fd97dc3be053c87cfa5625111c788835f";
            sha256 = "19m8ykvwg8nl5a327lwg7axd7dp9988f0y3lli0a322w2g7q5d25";
          }) {};

        hs-nlp-jp-utils = self.callCabal2nix "hs-nlp-jp-utils"
          (pkgs.fetchFromGitHub {
            owner = "dfordivam";
            repo = "hs-nlp-jp-utils";
            rev = "92c3a1ee168f121935ad8090f495c777df6a464c";
            sha256 = "1k754dkzns90bznrl6yp0vnjvd707bai3v6gi7vik2dynvc2g7z0";
          }) {};
    };

})
