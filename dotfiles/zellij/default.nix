{ config, lib, ... }:

let
  cfg = config.pix.dotfiles.zellij;
  src = ./home-files/.config/zellij;

in {
  options.pix.dotfiles.zellij = {
    enable = lib.mkEnableOption "Pot Zellij";
  };

  config = lib.mkIf cfg.enable {
    programs.zellij.enable = true;

    xdg.configFile."zellij" = {
      source = src;
      recursive = true;
    };
  };
}
