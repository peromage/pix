{ config, lib, ... }:

let
  inherit (lib)
    mkEnableOption
    mkIf;

  cfg = config.pix.hardware.firmware;

in {
  options.pix.hardware.firmware = {
    ## Don't forget `fwupdmgr update'
    enable = mkEnableOption "firmware management";
  };

  config = mkIf cfg.enable {
    hardware = {
      enableAllFirmware = true;
      enableRedistributableFirmware = true;
    };
    services.fwupd.enable = true;
  };
}
