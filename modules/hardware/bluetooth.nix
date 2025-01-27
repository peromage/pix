{ config, lib, ... }:

let
  inherit (lib)
    mkEnableOption
    mkIf;

  cfg = config.pix.hardware.bluetooth;

in {
  options.pix.hardware.bluetooth = {
    enable = mkEnableOption "Bluetooth management";
  };

  config = mkIf cfg.enable {
    hardware.bluetooth = {
      enable = true;
      powerOnBoot = true;
    };
  };
}
