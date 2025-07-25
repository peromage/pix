{ config, lib, pkgs, ... }:

let
  cfg = config.pix.services.docker;

in {
  options.pix.services.docker = {
    enable = lib.mkEnableOption "Docker";
    enableOnBoot = lib.mkEnableOption "Start Docker on system boot" // { default = true; };
  };

  config = lib.mkIf cfg.enable {
    virtualisation.docker = {
      enable = cfg.enable;
      enableOnBoot = cfg.enableOnBoot;
    };
  };
}
