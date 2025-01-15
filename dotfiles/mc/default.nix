{ config, lib, pkgs, ... }:

let
  cfg = config.pix.dotfiles.mc;
  src = ./home/.config/mc;

in with lib; {
  options.pix.dotfiles.mc = {
    enable = mkEnableOption "Pot Midnight Commander";
  };

  config = mkIf cfg.enable {
    home.packages = [ pkgs.mc ];

    xdg.configFile."mc" = {
      source = src;
      recursive = true;
    };
  };
}
