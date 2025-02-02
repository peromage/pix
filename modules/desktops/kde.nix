{ config, lib, ... }:

let
  cfgOverall = config.pix.desktops;
  cfg = cfgOverall.env.kde;

in {
  options.pix.desktops.env.kde = {
    enable = lib.mkEnableOption "KDE";
    enableSDDM = lib.mkEnableOption "SDDM display manager" // { default = true; };
  };

  config = lib.mkIf cfg.enable {
    services.xserver = {
      desktopManager.plasma5.enable = true;
      displayManager.sddm.enable = cfg.enableSDDM;
      displayManager.sddm.wayland.enable = cfgOverall.enableWayland;
    };
  };
}
