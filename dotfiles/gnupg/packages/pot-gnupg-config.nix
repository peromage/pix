{ stdenvNoCC, homeDir ? "/home/fang" }:

stdenvNoCC.mkDerivation {
  pname = "pot-gnupg-config";
  version = "0.0.1";
  src = ../home-files/.gnupg;
  sourceRoot = ".gnupg";
  dontPatchShebangs = true;

  buildPhase = ''
    sed -i'''''' 's#/home/fang/#${homeDir}/#' gpg-agent.conf
  '';

  installPhase = ''
    DIR="$out/etc/pot-gnupg-config"
    chmod u+x pinentry-auto.sh
    mkdir -p "$DIR"
    cp -a * "$DIR"
  '';
}
