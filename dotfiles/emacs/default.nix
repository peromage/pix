{ config, pkgs, lib, ... }:

let
  cfg = config.pix.dotfiles.emacs;
  src = ./home/.emacs.d;
  myEmacs = pkgs.callPackage ./pkgs/emacs.nix {};

in {
  options.pix.dotfiles.emacs = {
    enable = lib.mkEnableOption "Pot Emacs";
  };

  config = lib.mkIf cfg.enable {
    home.package = [ myEmacs ];

    home.file.".emacs.d" = {
      source = src;
      recursive = true;
    };
  };
}
