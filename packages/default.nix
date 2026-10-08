{
  pix,
  system,
}: let
  pkgs = pix.lib.makePkgs system;

  common = {
    build-essential = ./common/build-essential.nix;
    emacs = ./common/emacs.nix;
    herdr = ./common/herdr.nix;
    home-manager = ./common/home-manager.nix;
    myscripts = ./common/myscripts.nix;
    nodejs = ./common/nodejs.nix;
    python = ./common/python.nix;
    rime-default-config = ./common/rime-default-config.nix;
    spelling = ./common/spelling.nix;
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
