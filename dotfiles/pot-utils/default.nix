{ config, lib, pkgs, ... }:

let
  cfg = config.pix.dotfiles.pot-utils;

in {
  options.pix.dotfiles.pot-utils = {
    enable = lib.mkEnableOption "Pot Utils";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.pixPkgs.pot-utils ];
  };
}
