{ config, lib, ...}:

let
  cfg = config.pix.dotfiles.kitty;
  src = ./home-files/.config/kitty;

in {
  options.pix.dotfiles.kitty = {
    enable = lib.mkEnableOption "Kitty";
  };

  config = lib.mkIf cfg.enable {
    programs.kitty.enable = true;

    xdg.configFile."kitty" = {
      source = src;
      recursive = true;
    };
  };
}
