{ config, lib, ... }:

let
  cfg = config.pix.services.i18n;

in {
  options.pix.services.i18n = {
    enable = lib.mkEnableOption "internationalization";

    locale = lib.mkOption {
      type = lib.types.str;
      default = "en_US.UTF-8";
      description = "Default locale settings.";
    };

    timeZone = lib.mkOption {
      type = lib.types.str;
      default = "America/Detroit";
      description = "Default time zone.";
    };
  };

  config = lib.mkIf cfg.enable {
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
