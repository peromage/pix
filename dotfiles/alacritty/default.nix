{ config, pkgs, lib, ... }:

let
  cfg = config.pix.dotfiles.alacritty;
  src = ./home-files/.config/alacritty;

in {
  options.pix.dotfiles.alacritty = {
    enable = lib.mkEnableOption "Pot Alacritty";
    package = lib.mkPackageOption pkgs "alacritty" {};
  };

  config = lib.mkIf cfg.enable {
    programs.alacritty = {
      enable = true;
      package = cfg.package;
    };

    xdg.configFile."alacritty" = {
      source = src;
      recursive = true;
    };
  };
}
