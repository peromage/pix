{ config, lib, pkgs, ... }:

let
  inherit (lib)
    mkEnableOption
    mkIf;

  cfg = config.pix.services.flatpak;

in {
  options.pix.services.flatpak = {
    enable = mkEnableOption "Flatpak";
  };

  config = mkIf cfg.enable {
    services.flatpak.enable = true;

    ## Add default remote for all users
    systemd.services.flatpak-repo = {
      wantedBy = [ "multi-user.target" ];
      path = [ pkgs.flatpak ];
      script = ''
        flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
      '';
    };
  };
}
