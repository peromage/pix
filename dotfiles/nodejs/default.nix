{ config, pkgs, lib, ... }:

let
  cfg = config.pix.dotfiles.nodejs;
  myNodejs = let nodejs = pkgs.pixPkgs.nodejs; in nodejs.override {
    userNpmDir = "${config.xdg.dataHome}/${nodejs.userNpmDir}";
  };

in {
  options.pix.dotfiles.nodejs = {
    enable = lib.mkEnableOption "Pot NodeJS";
  };

  config = lib.mkIf cfg.enable {
    home.sessionPath = [
      myNodejs.userPath
    ];

    home.packages = [ myNodejs ];

    home.file.".npmrc".text = ''
      prefix=${myNodejs.userNpmDir}
    '';
  };
}
