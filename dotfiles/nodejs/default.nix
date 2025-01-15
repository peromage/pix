{ config, pkgs, lib, ... }:

let
  cfg = config.pix.dotfiles.nodejs;
  myNodejs = let nodejs = pkgs.callPackage ./pkgs/nodejs.nix {}; in nodejs.override {
    userNpmDir = "${config.xdg.dataHome}/${nodejs.userNpmDir}";
  };

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
      prefix=${myNodejs.userNpmDir}
    '';
  };
}
