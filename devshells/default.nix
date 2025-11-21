{ pix, pkgs }:

let
  pkgsCommon = {
    build-essential-env = ./common/build-essential-env.nix;
    python-env = ./common/python-env.nix;
  };

  pkgsPlatformSpecialized = {};

in pkgs.callPackageAttrs {} (pkgsCommon // (pkgsPlatformSpecialized.${pkgs.system} or {}))
