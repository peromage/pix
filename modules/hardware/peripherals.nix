{ config, lib, pkgs, ... }:

let
  cfg = config.pix.hardware.peripherals;

in {
  options.pix.hardware.peripherals = {
    enable = lib.mkEnableOption "peripheral management";
    devices = lib.mkOption {
      type = with lib.types; listOf (enum [ "printer" "zsa-keyboard" ]);
      default = [];
      description = "A list of devices to support.";
    };
  };

  config = lib.mkMerge [
    (lib.mkIf (cfg.enable && lib.elem "printer" cfg.devices) {
      services.printing.enable = true;
    })

    (lib.mkIf (cfg.enable && lib.elem "zsa-keyboard" cfg.devices) {
      hardware.keyboard.zsa.enable = true;
      environment.systemPackages = with pkgs; [
        keymapp
      ];
    })
  ];
}
