{ config, lib, ... }:

let
  cfg = config.pix.hardware;

in with lib; {
  imports = [
    ./bootloader/
    ./audio.nix
    ./bluetooth.nix
    ./firmware.nix
    ./networking.nix
    ./peripherals.nix
    ./power.nix
  ];

  options.pix.hardware = {
    platform = mkOption {
      type = with types; nullOr str;
      default = null;
      description = ''
        Host platform architecture.
        For clarification, this needs to be specified explicitly.
      '';
    };
  };

  config = {
    nixpkgs.hostPlatform = cfg.platform;

    assertions = singleton {
      assertion = cfg.platform != null;
      message = "Platform must be explicitly specified.";
    };
  };
}
