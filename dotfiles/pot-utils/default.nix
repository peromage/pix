{ config, lib, pkgs, ... }:

let
  cfg = config.pix.dotfiles.pot-utils;
  potUtils = pkgs.callPackage ./pkgs/pot-utils.nix {};

in {
  options.pix.dotfiles.pot-utils = {
    enable = lib.mkEnableOption "Pot Utils";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ potUtils ];
  };
}
