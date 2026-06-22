{ pix, pkgs }:

let
  common = {
    build-essential-env = ./common/build-essential-env.nix;
    python-env = ./common/python-env.nix;
  };

  platform = {};

in pkgs.callPackageAttrs {} (common // (platform.${pkgs.stdenv.hostPlatform.system} or {}))
