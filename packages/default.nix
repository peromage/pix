{ pix, pkgs, lib }:

let
  callPackages = lib.mapAttrs (_: file: pkgs.callPackage file {});

  pkgsCommon = {
    build-essential = ./common/build-essential.nix;
    home-configurations = ./common/home-configurations.nix;
    home-manager = ./common/home-manager.nix;
  };

  pkgsPlatformSpecialized = {
    x86_64-darwin = {
      bclm = ./x86_64-darwin/bclm.nix;
      nix-darwin = ./x86_64-darwin/nix-darwin.nix;
    };
  };

in callPackages (pkgsCommon // (pkgsPlatformSpecialized.${pkgs.system} or {}))
