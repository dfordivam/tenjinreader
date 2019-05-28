{pkgs ? (import ../../.obelisk/impl {}).reflex-platform.nixpkgs} :

let
  bulma-src = pkgs.fetchFromGitHub {
    owner = "jgthms";
    repo = "bulma";
    rev = "7cf10a786a187d6ce278a64c1bdd708384386420";
    sha256 = "0njivx5w72j4r71h8rqw7b0wh2salg9m2qiwpv8lbdhnwj2jpam7";
  };

  bulma-tooltip-src = pkgs.fetchFromGitHub {
    owner = "Wikiki";
    repo = "bulma-tooltip";
    rev = "716c9bf00cd4c59e3e18779c082f3aad12d0233c";
    sha256 = "1vnxymh3kvspp248mhlf6v6bnwd56wq95118hyrg4zfxa8ybsf5x";
  };

in pkgs.stdenv.mkDerivation rec {
  name = "tenjin-reader-css";

  src = ./.;

  nativeBuildInputs = [ pkgs.sass bulma-src bulma-tooltip-src ];

  buildPhase = ''
    ln -s ${bulma-src} sass/bulma
    ln -s ${bulma-tooltip-src}/dist/css sass/bulma-tooltip
    sass --sourcemap=none sass/tenjin-reader.scss:tenjin-reader.css
  '';

  installPhase = ''
    mkdir -p $out
    cp *.css $out/
  '';
}