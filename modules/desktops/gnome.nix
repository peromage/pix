{ config, lib, pkgs, ... }:

let
  cfgOverall = config.pix.desktops;
  cfg = cfgOverall.env.gnome;

in {
  options.pix.desktops.env.gnome = {
    enable = lib.mkEnableOption "Gnome";
    enableGDM = lib.mkEnableOption "GDM display manager" // { default = true; };
  };

  config = lib.mkIf cfg.enable (lib.mkMerge [
    ## For both Wayland and X11
    {
      services.xserver = {
        desktopManager.gnome.enable = true;
        displayManager.gdm.enable = cfg.enableGDM;
        displayManager.gdm.wayland = cfgOverall.enableWayland;
      };

      environment.systemPackages = with pkgs; [
        gnome-tweaks
        gnome-extension-manager
        dconf2nix
        gnome-terminal ## Provides more functionalities than default gnome-console
        gnomeExtensions.tray-icons-reloaded
      ];
    }

    ## X11 utilities
    (lib.mkIf (!cfgOverall.enableWayland) {
      environment.systemPackages = with pkgs; [
        gnomeExtensions.x11-gestures
      ];
      services.touchegg.enable = true;
    })
  ]);
}
