{ pix, pkgs }:

let
  pkgsCommon = {
    build-essential = ./common/build-essential.nix;
    home-manager = ./common/home-manager.nix;
    pot-utils = ../dotfiles/pot-utils/packages/pot-utils.nix;
    pot-emacs = ../dotfiles/emacs/packages/pot-emacs.nix;
    pot-emacs-config = ../dotfiles/emacs/packages/pot-emacs-config.nix;
    pot-spelling = ../dotfiles/emacs/packages/pot-spelling.nix;
    pot-nodejs = ../dotfiles/nodejs/packages/pot-nodejs.nix;
    pot-python = ../dotfiles/python/packages/pot-python.nix;
    rime-default-config = ../dotfiles/fcitx5/packages/rime-default-config.nix;
  };

  pkgsPlatformSpecialized = {
    x86_64-darwin = {
      bclm = ./x86_64-darwin/bclm.nix;
      nix-darwin = ./x86_64-darwin/nix-darwin.nix;
    };
  };

in pkgs.callPackageAttrs {} (pkgsCommon // (pkgsPlatformSpecialized.${pkgs.system} or {}))
