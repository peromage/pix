{ config, lib, pkgs, ... }:

let
  inherit (lib)
    mkEnableOption
    mkOption
    mkIf
    mkMerge
    types
    elem;

  cfg = config.pix.hardware.peripherals;

in {
  options.pix.hardware.peripherals = {
    enable = mkEnableOption "peripheral management";
    devices = mkOption {
      type = with types; listOf (enum [ "printer" "zsa-keyboard" ]);
      default = [];
      description = "A list of devices to support.";
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable && elem "printer" cfg.devices) {
      services.printing.enable = true;
    })

    (mkIf (cfg.enable && elem "zsa-keyboard" cfg.devices) {
      hardware.keyboard.zsa.enable = true;
      environment.systemPackages = with pkgs; [
        keymapp
      ];
    })
  ];
}
