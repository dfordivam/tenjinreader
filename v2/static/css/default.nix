{pkgs ? (import ../../.obelisk/impl {}).reflex-platform.nixpkgs} :

let
  bulma-src = pkgs.fetchFromGitHub {
    owner = "jgthms";
    repo = "bulma";
    rev = "7cf10a786a187d6ce278a64c1bdd708384386420";
    sha256 = "0njivx5w72j4r71h8rqw7b0wh2salg9m2qiwpv8lbdhnwj2jpam7";
  };

in pkgs.stdenv.mkDerivation rec {
  version = "1.0";
  name = "tenjin-reader-css-${version}";

  src = ./.;

  nativeBuildInputs = [ pkgs.sass bulma-src ];

  buildPhase = ''
    ln -s ${bulma-src} sass/bulma
    sass --sourcemap=none sass/tenjin-reader.scss:tenjin-reader.css
  '';

  installPhase = ''
    mkdir -p $out
    cp tenjin-reader.css $out/
  '';
}