{ stdenvNoCC, lib }:

stdenvNoCC.mkDerivation {
  pname = "pot-utils";
  version = "0.0.1";
  src = ../home-files;
  dontPatchShebangs = true;

  installPhase = ''
    mkdir -p $out/bin
    install --mode=555 $src/bin/* $out/bin/
  '' + (lib.optionalString stdenvNoCC.isDarwin ''
    install --mode=555 $src/bin-darwin/* $out/bin/
  '');
}
