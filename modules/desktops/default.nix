{ config, lib, pkgs, pix, ... }:

let
  cfg = config.pix.desktops;

in {
  imports = [
    ./gnome.nix
    ./kde.nix
    ./xfce.nix
  ];

  options.pix.desktops = {
    /*
       The display server is actually selected by the display manager.
       See: https://discourse.nixos.org/t/enabling-x11-still-results-in-wayland/25362/2
    */
    enableWayland = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = ''
        Use Wayland as default display server.
        This option has no effect if no DE is enabled in `pix.desktops.env'.
      '';
    };

    enableGraphicAcceleration = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = ''
        Graphic acceleration support.
        This option has no effect if no DE is enabled in `pix.desktops.env'.
      '';
    };

    ## Each DE can have their own options while `enable' is mandatory
    env = {};
  };

  config = lib.mkIf (pix.lib.anyAttrs (_: v: v.enable) cfg.env) {
    services = {
      xserver.enable = true;
      libinput.enable = true;
    };

    programs.xwayland.enable = cfg.enableWayland;

    hardware.graphics = {
      enable = cfg.enableGraphicAcceleration;
      enable32Bit = cfg.enableGraphicAcceleration;
    };

    environment.systemPackages = with pkgs; [
      desktop-file-utils
    ];
  };
}
