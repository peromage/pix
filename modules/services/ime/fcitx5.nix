{ config, lib, pkgs, ... }:

let
  cfg = config.pix.services.ime.fcitx5;
  gnomeCfg = config.pix.desktops.env.gnome;

in {
  options.pix.services.ime.fcitx5 = {
    enable = lib.mkEnableOption "Fcitx5";

    layout = lib.mkOption {
      type = lib.types.str;
      default = "us";
      description = "X keyboard layout, or multiple keyboard layouts separated by commas.";
    };
  };

  config = lib.mkIf cfg.enable {
    services.xserver.xkb.layout = cfg.layout;

    i18n.inputMethod = {
      enable = true;
      type = "fcitx5";
      fcitx5.addons = with pkgs; [
        (fcitx5-rime.override { rimeDataPkgs = []; })
        fcitx5-configtool
        fcitx5-gtk
      ];
    };

    environment.systemPackages = with pkgs; [
      (librime.override { plugins = [ librime-lua librime-octagram ];})
      rime-cli
    ]
    ++ lib.optional gnomeCfg.enable gnomeExtensions.kimpanel;
  };
}
