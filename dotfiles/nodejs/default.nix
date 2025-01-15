{ config, pkgs, lib, ... }:

let
  cfg = config.pix.dotfiles.nodejs;
  nodejis = pkgs.callPackage ./pkgs/nodejs.nix { userNpmDir = "${config.xdg.dataHome}/${n.userNpmDir}"; };

in with lib; {
  options.pix.dotfiles.nodejs = {
    enable = mkEnableOption "Pot NodeJS";
  };

  config = mkIf cfg.enable {
    home.sessionPath = [
      nodejs.userPath
    ];

    home.packages = [ nodejs ];

    home.file.".npmrc".text = ''
      prefix=${userNpmDir}
    '';
  };
}
