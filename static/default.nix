{pkgs ? (import ../.obelisk/impl {}).reflex-platform.nixpkgs} :
let
    css = pkgs.callPackage ./css {};
in pkgs.stdenv.mkDerivation {
    name ="tenjin-reader-staticFiles";
    src = ./.;
    builder = pkgs.writeScript "builder.sh" ''
      source "$stdenv/setup"
      mkdir -p $out/css
      # cp -r $src/fonts $src/icons $src/images $out
      cp -r $src/images $out
      cp -r ${css}/* $out/css/
    '';
  }
