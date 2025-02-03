{ pkgs }:

pkgs.stdenvNoCC.mkDerivation {
  pname = "pot-utils";
  version = "0.0.1";
  src = ../home-files;
  nativeBuildInputs = with pkgs; [];
  dontPatchShebangs = true;

  installPhase = ''
    mkdir -p bin
    install --mode=555 $src/bin/* bin/
  '';
}
