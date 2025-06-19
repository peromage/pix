{ config, lib, pkgs, ... }:

let
  cfg = config.pix.dotfiles.fcitx5;
  srcFcitx5Config = ./home-files/.config/fcitx5;
  srcFcitx5PluginConfig = ./home-files/.local/share/fcitx5;

in {
  options.pix.dotfiles.fcitx5 = {
    enable = lib.mkEnableOption "Pot Fcitx5";
  };

  config = lib.mkIf cfg.enable {
    i18n.inputMethod = {
      enable = true;
      type = "fcitx5";
      fcitx5.addons = with pkgs; [
        fcitx5-rime
        fcitx5-configtool
        fcitx5-chinese-addons
        fcitx5-gtk
      ];
    };

    home.packages = with pkgs; [
      librime
      librime-lua
      librime-octagram
      rime-cli
      rime-data
    ];

    xdg.configFile."fcitx5" = {
      source = srcFcitx5Config;
      recursive = true;
    };
    xdg.dataFile."fcitx5" = {
      source = srcFcitx5PluginConfig;
      recursive = true;
    };
  };
}
