{ config, lib, ... }:

let
  cfg = config.pix.hardware.bootloader.systemd-boot;

in {
  options.pix.hardware.bootloader.systemd-boot = {
    enable = lib.mkEnableOption "Systemd-boot bootloader";
  };

  config = lib.mkIf cfg.enable {
    boot = {
      bootspec.enable = true;
      loader = {
        grub.enable = lib.mkForce false;
        systemd-boot.enable = lib.mkForce true;
        efi.canTouchEfiVariables = false;
      };
    };
  };
}
