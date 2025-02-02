{ config, lib, ... }:

let
  cfg = config.pix.hardware.bootloader.grub;

in {
  options.pix.hardware.bootloader.grub = {
    enable = lib.mkEnableOption "Grub bootloader";

    device = lib.mkOption {
      type = lib.types.str;
      default = "";
      description = "Same option `boot.loader.grub.device'.";
    };

    probeOS = lib.mkEnableOption "Detect other OS";
  };

  config = lib.mkIf cfg.enable {
    boot = {
      bootspec.enable = true;
      loader = {
        grub = {
          enable = lib.mkForce true;
          efiSupport = true;
          device = cfg.grubDevice;
          useOSProber = cfg.probeOS;
        };
        systemd-boot.enable = lib.mkForce false;
        efi.canTouchEfiVariables = false;
      };
    };
  };
}
