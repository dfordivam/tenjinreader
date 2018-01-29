{}:

(import ./reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    common = ./common;
    backend = ./backend;
    kanjidbreader = ./backend/kanjiDbReader;
    frontend = ./frontend;
  };

  android.frontend = {
    executableName = "frontend";
    applicationId = "org.example.frontend";
    displayName = "Example Android App";
  };

  shells = {
    ghc = ["common" "backend" "frontend" "kanjidbreader"];
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
            rev = "3cbcf5e780d1dd9ba5040a9cc9eb1f2f149cb5e7";
            sha256 = "1fdagcf1gjs0asq7p0nbyv2ar6js9z86crkxrhd28r7mdg33v2kq";
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
