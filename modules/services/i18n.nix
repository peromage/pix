{ config, lib, ... }:

let
  inherit (lib)
    mkEnableOption
    mkOption
    mkIf
    types;

  cfg = config.pix.services.i18n;

in {
  options.pix.services.i18n = {
    enable = mkEnableOption "internationalization";

    locale = mkOption {
      type = types.str;
      default = "en_US.UTF-8";
      description = "Default locale settings.";
    };

    timeZone = mkOption {
      type = types.str;
      default = "America/Detroit";
      description = "Default time zone.";
    };
  };

  config = mkIf cfg.enable {
    i18n = {
      defaultLocale = cfg.locale;
      extraLocaleSettings = {
        LC_ADDRESS = cfg.locale;
        LC_IDENTIFICATION = cfg.locale;
        LC_MEASUREMENT = cfg.locale;
        LC_MONETARY = cfg.locale;
        LC_NAME = cfg.locale;
        LC_NUMERIC = cfg.locale;
        LC_PAPER = cfg.locale;
        LC_TELEPHONE = cfg.locale;
        LC_TIME = cfg.locale;
      };
    };
    time.timeZone = cfg.timeZone;
  };
}
