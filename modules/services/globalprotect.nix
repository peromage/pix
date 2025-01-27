{ config, lib, pkgs, ... }:

let
  inherit (lib)
    mkEnableOption
    mkIf;

  cfg = config.pix.services.globalprotect;

in {
  options.pix.services.globalprotect = {
    enable = mkEnableOption "GlobalProtect VPN client";
  };

  config = mkIf cfg.enable {
    services.globalprotect.enable = true;
    environment.systemPackages = with pkgs; [
      globalprotect-openconnect
    ];
  };
}
