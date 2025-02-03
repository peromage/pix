{ config, lib, ... }:

let
  cfg = config.pix.dotfiles.alacritty;
  src = ./home-files/.config/alacritty;

in {
  options.pix.dotfiles.alacritty = {
    enable = lib.mkEnableOption "Pot Alacritty";
  };

  config = lib.mkIf cfg.enable {
    programs.alacritty.enable = true;

    xdg.configFile."alacritty" = {
      source = src;
      recursive = true;
    };
  };
}
