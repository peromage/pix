{ config, lib, pkgs, ... }:

let
  cfg = config.pix.dotfiles.mc;
  src = ./home-files/.config/mc;

in {
  options.pix.dotfiles.mc = {
    enable = lib.mkEnableOption "Pot Midnight Commander";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.mc ];

    xdg.configFile."mc" = {
      source = src;
      recursive = true;
    };
  };
}
