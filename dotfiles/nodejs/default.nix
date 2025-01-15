{ config, pkgs, lib, ... }:

let
  cfg = config.pix.dotfiles.nodejs;
  nodejs = let n = pkgs.pixPkgs.nodejs; in n.override {
    userNpmDir = "${config.xdg.dataHome}/${n.userNpmDir}";
  };

in with lib; {
  options.pix.dotfiles.nodejs = {
    enable = mkEnableOption "NodeJS";
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
