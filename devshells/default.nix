{ pix, pkgs, ... }:

let
  importPackages = pkgAttrs: pkgs.lib.mapAttrs (name: file: pkgs.newScope { inherit pix; } file {}) pkgAttrs;

  pkgsCommon = importPackages {
    build-essential-env = ./common/build-essential-env.nix;
    python-env = ./common/python-env.nix;
  };

  pkgsPlatformSpecialized = {};

in pkgsCommon // (pkgsPlatformSpecialized.${pkgs.system} or {})
