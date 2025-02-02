{ config, lib, ... }:

let
  cfgOverall = config.pix.desktops;
  cfg = cfgOverall.env.xfce;

in {
  options.pix.desktops.env.xfce = {
    enable = lib.mkEnableOption "XFCE";
    enableLightDM = lib.mkEnableOption "LightDM display manager" // { default = true; };
  };

  config = lib.mkIf cfg.enable {
    services.xserver = {
      desktopManager.xfce.enable = true;
      displayManager.lightdm.enable = cfg.enableLightDM;
    };
  };
}
