{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "10.2"
, withHoogle ? false # to spin up localhost:8080 hoogle use: nix-shell --arg withHoogle true -A shells.ghc --command "hoogle server -p 8080 --local"
}:
let
  obelisk = import .obelisk/impl { inherit system iosSdkVersion; };
in obelisk.project ./. ({ pkgs, ... }:
  let

  in {
    inherit withHoogle;
    staticFiles = pkgs.callPackage ./static { pkgs = obelisk.nixpkgs; };
    android.applicationId = "systems.obsidian.obelisk.examples.minimal";
    android.displayName = "Obelisk Minimal Example";
    ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
    ios.bundleName = "Obelisk Minimal Example";
    overrides = self: super: let
      servant-reflex = pkgs.fetchFromGitHub {
        owner = "imalsogreg";
        repo = "servant-reflex";
        rev = "ba8d4f8a269d785ed7e62f11eddb392d9f582e19";
        sha256 = "0zppzl1ii01bzjrfj5x71vff5ivpcngrs0njvjawx6hf985x2zbk";
      };
      servant-snap = pkgs.fetchFromGitHub {
        owner = "antislava";
        repo = "servant-snap";
        rev = "fc9658e8f52ebce9e4f304ea0c6705d697d4fa84";
        sha256 = "0zlipmx1fb73mhpnndwmdmigxxrsdnsnb1157pgsrxpx89l9pjig";
      };
    in
      {
        servant-reflex = pkgs.haskell.lib.doJailbreak (self.callCabal2nix "servant-reflex" servant-reflex {});
        servant-snap = (pkgs.haskell.lib.dontCheck (self.callCabal2nix "servant-snap" servant-snap {}));
      };
  })
