{
  config,
  pkgs,
  lib,
  ...
}: let
  cfg = config.pix.dotfiles.ghostty;
  src = ./home-files/.config/ghostty;
in {
  options.pix.dotfiles.ghostty = {
    enable = lib.mkEnableOption "Pot Ghostty";
    passthru = lib.mkOption {default = {};};
  };

  config = lib.mkIf cfg.enable {
    programs.ghostty =
      {
        enable = true;
      }
      // cfg.passthru;

    xdg.configFile."ghostty" = {
      source = src;
      recursive = true;
    };
  };
}
