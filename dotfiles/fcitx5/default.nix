{ config, lib, pkgs, ... }:

let
  cfg = config.pix.dotfiles.fcitx5;
  srcFcitx5Config = ./home-files/.config/fcitx5;
  srcFcitx5PluginConfig = ./home-files/.local/share/fcitx5;

  srcRimeIce = pkgs.fetchFromGitHub {
    owner = "iDvel";
    repo = "rime-ice";
    rev = "2024.12.12";
    hash = "sha256-2QZdlLGZwWIesbjYTE/2yhM1hHGVVp7jR02bR0oqxV0=";
  };

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

    /*
       To rebuild, cd to this directory and execute:
         rime_deployer --build
    */
    xdg.dataFile."fcitx5/rime" = {
      source = srcRimeIce.outPath;
      recursive = true;
    };
  };
}
