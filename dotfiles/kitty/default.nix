{
  config,
  pkgs,
  lib,
  ...
}: let
  cfg = config.pix.dotfiles.kitty;
  src = ./home-files/.config/kitty;
in {
  options.pix.dotfiles.kitty = {
    enable = lib.mkEnableOption "Pot Kitty";
    passthru = lib.mkOption {default = {};};
  };

  config = lib.mkIf cfg.enable {
    programs.kitty =
      {
        enable = true;
      }
      // cfg.passthru;

    xdg.configFile."kitty" = {
      source = src;
      recursive = true;
    };
  };
}
