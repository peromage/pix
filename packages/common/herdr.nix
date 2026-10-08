# This package is used as a workaround as herdr is not available in 26.05 currently
{
  stdenvNoCC,
  system,
  lib,
  fetchurl,
}: let
  # Copied from GitHub release page
  version = "v0.9.3";
  artifact =
    {
      "x86_64-linux" = {
        file = "herdr-linux-x86_64";
        sha256 = "sha256:18a8dc65f1c2fa485884344356dea1cfd911c6f06cf46fa78e193f4087f4dba7";
      };
      "aarch64-linux" = {
        file = "herdr-linux-aarch64";
        sha256 = "sha256:4de7aa3e25678812e92960de64f7c2aaa1bca1f0f80a3c5e559837e231e1f5c0";
      };
      "x86_64-darwin" = {
        file = "herdr-macos-x86_64";
        sha256 = "sha256:db62d548ff3e832b087a96b1894a08d26be3905f1830309cd556783f215d4054";
      };
      "aarch64-darwin" = {
        file = "herdr-macos-aarch64";
        sha256 = "sha256:5173a3e0ae42d5d1ab7ebfa5d5e6329f7c3d23f8e1a3677c7ce3231da2884157";
      };
    }.${
      system
    };

  url = {
    url = "https://github.com/herdrdev/herdr/releases/download/${version}/${artifact.file}";
    sha256 = lib.removePrefix "sha256:" artifact.sha256;
  };
in
  stdenvNoCC.mkDerivation (finlaAttrs: {
    pname = "herdr";
    version = version;
    src = fetchurl url;
    dontUnpack = true;
    installPhase = ''
      mkdir -p $out/bin
      install -Dm755 $src $out/bin/herdr
    '';
    meta = {
      description = "Herdr prebuilt binary";
      homepage = "https://github.com/herdrdev/herdr";
    };
  })
