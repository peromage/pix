{ pix, pkgs, lib }:

let
  callPackages = lib.mapAttrs (_: file: pkgs.callPackage file {});

  pkgsCommon = {
    build-essential-env = ./common/build-essential-env.nix;
    python-env = ./common/python-env.nix;
  };

  pkgsPlatformSpecialized = {};

in callPackages (pkgsCommon // (pkgsPlatformSpecialized.${pkgs.system} or {}))
