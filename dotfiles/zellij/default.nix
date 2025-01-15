{ config, lib, ... }:

let
  cfg = config.pix.dotfiles.zellij;
  src = ./home/.config/zellij;

in with lib; {
  options.pix.dotfiles.zellij = {
    enable = mkEnableOption "Zellij";
  };

  config = mkIf cfg.enable {
    programs.zellij.enable = true;

    xdg.configFile."zellij" = {
      source = src;
      recursive = true;
    };
  };
}
