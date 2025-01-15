{ config, pkgs, lib, ... }:

let
  cfg = config.pix.dotfiles.emacs;
  src = ./home/.emacs.d;
  myEmacs = pkgs.callPackage ./pkgs/emacs.nix {};

in with lib; {
  options.pix.dotfiles.emacs = {
    enable = mkEnableOption "Pot Emacs";
  };

  config = mkIf cfg.enable {
    home.package = [ myEmacs ];

    home.file.".emacs.d" = {
      source = src;
      recursive = true;
    };
  };
}
