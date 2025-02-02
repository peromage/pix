{ config, lib, ... }:

let
  cfg = config.pix.hardware.bluetooth;

in {
  options.pix.hardware.bluetooth = {
    enable = lib.mkEnableOption "Bluetooth management";
  };

  config = lib.mkIf cfg.enable {
    hardware.bluetooth = {
      enable = true;
      powerOnBoot = true;
    };
  };
}
