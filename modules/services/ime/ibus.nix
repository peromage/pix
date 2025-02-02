{ config, lib, pkgs, ... }:

let
  cfg = config.pix.services.ime.ibus;

in {
  options.pix.services.ime.ibus = {
    enable = lib.mkEnableOption "Ibus";

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
      type = "ibus";
      ibus.engines = with pkgs.ibus-engines; [
        rime
        libpinyin
      ];
    };

    environment.systemPackages = with pkgs; [
      librime
      rime-cli
      rime-data
    ];
  };
}
