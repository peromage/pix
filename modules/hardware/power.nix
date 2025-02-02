{ config, lib, ... }:

let
  cfg = config.pix.hardware.power;

in {
  options.pix.hardware.power = {
    enable = lib.mkEnableOption "power governor";

    profile = lib.mkOption {
      type = lib.types.enum [ "ondemand" "powersave" "performance" ];
      default = "ondemand";
      description = "Default power governor profile.";
    };
  };

  config = lib.mkIf cfg.enable {
    powerManagement.cpuFreqGovernor = cfg.profile;
  };
}
