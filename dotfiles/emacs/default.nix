{ config, pkgs, lib, ... }:

let
  cfg = config.pix.dotfiles.emacs;
  src = ./home-files/.emacs.d;
  myEmacs = pkgs.callPackage ./pkgs/emacs.nix {};

in {
  options.pix.dotfiles.emacs = {
    enable = lib.mkEnableOption "Pot Emacs";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ myEmacs ];

    home.file.".emacs.d" = {
      source = src;
      recursive = true;
    };
  };
}
