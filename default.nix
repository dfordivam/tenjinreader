{ obelisk ? import ./.obelisk/impl {
    system = builtins.currentSystem;
    iosSdkVersion = "10.2";
    config.android_sdk.accept_license = true;
  }
}:
with obelisk;
project ./. ({ pkgs, hackGet, ... }:
let
  inherit (pkgs.haskell.lib) enableCabalFlag doJailbreak dontCheck appendPatch;

in {
  staticFiles = pkgs.callPackage ./static { pkgs = obelisk.nixpkgs; };
  overrides = self: super: {
    servant-reflex = self.callCabal2nix "servant-reflex" (hackGet ./dep/servant-reflex) {};
    # is marked broken
    servant-snap = (dontCheck (self.callCabal2nix "servant-snap" (hackGet ./dep/servant-snap) {}));
  };
})