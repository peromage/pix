{
  config,
  pkgs,
  lib,
  ...
}: let
  cfg = config.pix.dotfiles.wezterm;
  src = ./home-files/.config/wezterm;
in {
  options.pix.dotfiles.wezterm = {
    enable = lib.mkEnableOption "Pot Wez's Terminal";
    passthru = lib.mkOption {};
  };

  config = lib.mkIf cfg.enable {
    programs.wezterm =
      {
        enable = true;
        # enableBashIntegration = true;
      }
      // cfg.passthru;

    xdg.configFile."wezterm" = {
      source = src;
      recursive = true;
    };
  };
}
