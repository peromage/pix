{ config, lib, pkgs, ... }:

let
  cfg = config.pix.dotfiles.tigervnc;
  src = ./home/.config/systemd/user/x0vncserver.service;

in {
  options.pix.dotfiles.tigervnc = {
    enable = lib.mkEnableOption "Pot TigerVNC";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [ tigervnc ];
    xdg.configFile.x0vncserverService = {
      source = src;
      target = "systemd/user/x0vncserver.service";
    };
  };
}
