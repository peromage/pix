{ stdenvNoCC }:

stdenvNoCC.mkDerivation {
  pname = "rime-default-config";
  version = "0.0.1";

  dontUnpack = true;
  dontPatch = true;
  dontConfigure = true;

  installPhase = ''
    runHook preInstall

    dest=$out/share/rime-data
    mkdir -p $dest
    touch $dest/default.yaml

    runHook postInstall
  '';
}
