{ pix, system }:

let
  importPackages = pkgAttrs: pkgs.lib.mapAttrs (name: file: pkgs.newScope { inherit pix; } file {}) pkgAttrs;

  pkgsCommon = importPackages {
    build-essential = ./common/build-essential.nix;
    home-configurations = ./common/home-configurations.nix;
    home-manager = ./common/home-manager.nix;
  };

  pkgsPlatformSpecialized = {
    x86_64-darwin = importPackages {
      bclm = ./x86_64-darwin/bclm.nix;
      nix-darwin = ./x86_64-darwin/nix-darwin.nix;
    };
  };

in pkgsCommon // (pkgsPlatformSpecialized.${pkgs.system} or {})
