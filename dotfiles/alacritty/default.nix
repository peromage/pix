{ config, lib, ... }:

let
  cfg = config.pix.dotfiles.alacritty;
  src = ./home/.config/alacritty;

in with lib; {
  options.pix.dotfiles.alacritty = {
    enable = mkEnableOption "Pot Alacritty";
  };

  config = mkIf cfg.enable {
    programs.alacritty.enable = true;

    xdg.configFile."alacritty" = {
      source = src;
      recursive = true;
    };
  };
}
