{ config, lib, pkgs, pix, ... }:

let
  cfg = config.pix.dotfiles.tigervnc;
  src = ./home/.config/systemd/user/x0vncserver.service;

in with lib; {
  options.pix.dotfiles.tigervnc = {
    enable = mkEnableOption "TigerVNC";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ tigervnc ];
    xdg.configFile.x0vncserverService = {
      source = src;
      target = "systemd/user/x0vncserver.service";
    };
  };
}
