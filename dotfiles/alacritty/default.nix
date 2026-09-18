{ config, pkgs, lib, ... }:

let
  cfg = config.pix.dotfiles.alacritty;
  src = ./home-files/.config/alacritty;

in {
  options.pix.dotfiles.alacritty = {
    enable = lib.mkEnableOption "Pot Alacritty";
    passthru = lib.mkOption {};
  };

  config = lib.mkIf cfg.enable {
    programs.alacritty = {
      enable = true;
    } // cfg.passthru;

    xdg.configFile."alacritty" = {
      source = src;
      recursive = true;
    };
  };
}
