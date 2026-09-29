{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.pix.desktops.env.gnome;
in {
  options.pix.desktops.env.gnome = {
    enable = lib.mkEnableOption "Gnome";
    enableGDM = lib.mkEnableOption "GDM display manager" // {default = true;};
  };

  ## Gnome has dropped X11 support completely
  config =
    lib.mkIf cfg.enable
    {
      services = {
        desktopManager.gnome.enable = true;
        displayManager.gdm.enable = cfg.enableGDM;
      };

      environment.systemPackages =
        (with pkgs; [
          gnome-tweaks
          gnome-extension-manager
          dconf2nix
          gnome-terminal ## Provides more functionalities than default gnome-console
          pinentry-gnome3
        ])
        ++ (with pkgs.gnomeExtensions; [
          tray-icons-reloaded
          kimpanel
        ]);

      security.pam.services.gdm.enableGnomeKeyring = true;
    };
}
