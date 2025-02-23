{ config, lib, pkgs, ... }:

let
  cfg = config.pix.hardware.peripherals;

  mkDeviceConfig = device: config: lib.mkIf (lib.elem device cfg.devices) config;

in {
  options.pix.hardware.peripherals = {
    enable = lib.mkEnableOption "peripheral management";
    devices = lib.mkOption {
      type = with lib.types; listOf (enum [
        "printer"
        "zsa-keyboard"
        "xbox-controller"
      ]);
      default = [];
      description = "A list of devices to support.";
    };
  };

  config = lib.mkIf cfg.enable (lib.mkMerge [
    (mkDeviceConfig "printer" {
      services.printing.enable = true;
    })

    (mkDeviceConfig "zsa-keyboard" {
      hardware.keyboard.zsa.enable = true;
      environment.systemPackages = with pkgs; [
        keymapp
      ];
    })

    (mkDeviceConfig "xbox-controller" {
      hardware.xpadneo.enable = true;
    })
  ]);
}
