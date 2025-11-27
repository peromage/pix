{ pix, pkgs }:

let
  pkgsCommon = {
    build-essential = ./common/build-essential.nix;
    home-manager = ./common/home-manager.nix;
    pot-utils = ../dotfiles/pot-utils/pkgs/pot-utils.nix;
    emacs = ../dotfiles/emacs/pkgs/emacs.nix;
    spelling = ../dotfiles/emacs/pkgs/spelling.nix;
    nodejs = ../dotfiles/nodejs/pkgs/nodejs.nix;
    python = ../dotfiles/python/pkgs/python.nix;
    rime-default-config = ../dotfiles/fcitx5/pkgs/rime-default-config.nix;
    pot-emacs-config = ../dotfiles/emacs/pkgs/pot-emacs-config.nix;
  };

  pkgsPlatformSpecialized = {
    x86_64-darwin = {
      bclm = ./x86_64-darwin/bclm.nix;
      nix-darwin = ./x86_64-darwin/nix-darwin.nix;
    };
  };

in pkgs.callPackageAttrs {} (pkgsCommon // (pkgsPlatformSpecialized.${pkgs.system} or {}))
