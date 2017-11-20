{}:

(import ./reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    common = ./common;
    backend = ./backend;
    frontend = ./frontend;
  };

  android.frontend = {
    executableName = "frontend";
    applicationId = "org.example.frontend";
    displayName = "Example Android App";
  };

  shells = {
    ghc = ["common" "backend" "frontend"];
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
    };

})
