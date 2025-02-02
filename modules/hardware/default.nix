{ config, lib, ... }:

let
  cfg = config.pix.hardware;

in {
  imports = [
    ./bootloader
    ./audio.nix
    ./bluetooth.nix
    ./firmware.nix
    ./networking.nix
    ./peripherals.nix
    ./power.nix
  ];

  options.pix.hardware = {
    platform = lib.mkOption {
      type = with lib.types; nullOr str;
      default = null;
      description = ''
        Host platform architecture.
        For clarification, this needs to be specified explicitly.
      '';
    };
  };

  config = {
    nixpkgs.hostPlatform = cfg.platform;

    assertions = lib.singleton {
      assertion = cfg.platform != null;
      message = "Platform must be explicitly specified.";
    };
  };
}
