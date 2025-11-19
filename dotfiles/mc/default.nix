{ config, lib, pkgs, ... }:

let
  cfg = config.pix.dotfiles.mc;
  src = ./home-files/.config/mc;

in {
  options.pix.dotfiles.mc = {
    enable = lib.mkEnableOption "Pot Midnight Commander";
    package = lib.mkPackageOption pkgs "mc" {};
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ cfg.package ];

    xdg.configFile."mc" = {
      source = src;
      recursive = true;
    };
  };
}
