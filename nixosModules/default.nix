{ config, lib, pix, ... }:

let
  systemCfg = config.pix.system;
  libpix = pix.lib;

in with lib; {
  # imports = with pix.lib; listDir (notPred isDefaultNix) ./.;
  imports = [
    ./desktops
    ./hardware
    ./services
    ./users
    ./hosts/PRMG.nix
  ];

  /* Interface */
  options.pix.system = {
    hostName = mkOption {
      type = with types; nullOr str;
      default = "PIX";
      description = "Host name for this machine.";
    };
  };

  config = {
    networking.hostName = systemCfg.hostName;
  };
}
