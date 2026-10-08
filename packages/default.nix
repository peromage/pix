{
  pix,
  system,
}: let
  pkgs = pix.lib.makePkgs system;

  common = {
    build-essential = ./common/build-essential.nix;
    home-manager = ./common/home-manager.nix;
    utils = ./common/utils.nix;
    emacs = ./common/emacs.nix;
    spelling = ./common/spelling.nix;
    nodejs = ./common/nodejs.nix;
    python = ./common/python.nix;
    rime-default-config = ./common/rime-default-config.nix;
  };

  platform = {
    x86_64-darwin = {
      bclm = ./darwin/bclm.nix;
      nix-darwin = ./darwin/nix-darwin.nix;
    };

    aarch64-darwin = {
      nix-darwin = ./darwin/nix-darwin.nix;
    };
  };
in
  pkgs.pixScope.callPackageAttrs {} (common // (platform.${pkgs.stdenv.hostPlatform.system} or {}))
