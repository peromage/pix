{ config, lib, pkgs, ... }:

let
  cfg = config.pix.dotfiles.pot-utils;
  potUtils = pkgs.callPackage ./pkgs/pot-utils.nix {};

in with lib; {
  options.pix.dotfiles.pot-utils = {
    enable = mkEnableOption "Pot Utils";
  };

  config = mkIf cfg.enable {
    home.package = [ potUtils ];
  };
}
