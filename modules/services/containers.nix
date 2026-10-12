{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.pix.services.containers;
in {
  options.pix.services.containers = {
    enable = lib.mkEnableOption "Container services";
  };

  config = lib.mkIf cfg.enable {
    virtualisation.podman = {
      enable = true;
      dockerCompat = true;
    };

    environment.systemPackages = with pkgs; [
      docker-compose
    ];
  };
}
