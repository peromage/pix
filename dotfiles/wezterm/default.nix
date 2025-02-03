{ config, lib, ... }:

let
  cfg = config.pix.dotfiles.wezterm;
  src = ./home-files/.config/wezterm;

in {
  options.pix.dotfiles.wezterm = {
    enable = lib.mkEnableOption "Pot Wez's Terminal";
  };

  config = lib.mkIf cfg.enable {
    programs.wezterm = {
      enable = true;
      # enableBashIntegration = true;
    };

    xdg.configFile."wezterm" = {
      source = src;
      recursive = true;
    };
  };
}
