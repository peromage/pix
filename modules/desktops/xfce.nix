{ config, lib, ... }:

let
  inherit (lib)
    mkEnableOption
    mkIf;

  cfgOverall = config.pix.desktops;
  cfg = cfgOverall.env.xfce;

in {
  options.pix.desktops.env.xfce = {
    enable = mkEnableOption "XFCE";
    enableLightDM = mkEnableOption "LightDM display manager" // { default = true; };
  };

  config = mkIf cfg.enable {
    services.xserver = {
      desktopManager.xfce.enable = true;
      displayManager.lightdm.enable = cfg.enableLightDM;
    };
  };
}
