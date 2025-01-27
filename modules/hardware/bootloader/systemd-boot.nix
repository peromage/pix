{ config, lib, ... }:

let
  inherit (lib)
    mkEnableOption
    mkIf
    mkForce;

  cfg = config.pix.hardware.bootloader.systemd-boot;

in {
  options.pix.hardware.bootloader.systemd-boot = {
    enable = mkEnableOption "Systemd-boot bootloader";
  };

  config = mkIf cfg.enable {
    boot = {
      bootspec.enable = true;
      loader = {
        grub.enable = mkForce false;
        systemd-boot.enable = mkForce true;
        efi.canTouchEfiVariables = false;
      };
    };
  };
}
