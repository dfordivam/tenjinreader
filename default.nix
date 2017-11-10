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

  overrides = self: super: {
    yesod-auth-oauth2 = pkgs.haskell.lib.doJailbreak (self.callCabal2nix "yesod-auth-oauth2" (pkgs.fetchFromGitHub {
      owner = "thoughtbot";
      repo = "yesod-auth-oauth2";
      rev = "937ad572a31dcbb2d5fe75b91f689c7a2f6ecfca";
      sha256 = "0ai4axdh5x87qrkzn6a9i0a2srs6gkjbbiywihrv7ydmh2caqjya";
    }) {});
  };
})
