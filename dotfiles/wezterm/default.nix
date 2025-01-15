{ config, lib, ... }:

let
  cfg = config.pix.dotfiles.wezterm;
  src = ./home/.config/wezterm;

in with lib; {
  options.pix.dotfiles.wezterm = {
    enable = mkEnableOption "Wez's Terminal";
  };

  config = mkIf cfg.enable {
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
