{ config, lib, pkgs, ... }:
let
  cfg = config.pix.dotfiles.herdr;
  src = ./home-files/.config/herdr;
in {
  options.pix.dotfiles.herdr = {
    enable = lib.mkEnableOption "My Herdr";
  };

  config = lib.mkIf cfg.enable {
    # Doesn't exist in 26.05, need a workaround
    home.packages = [ pkgs.pixPkgs.herdr ];

    xdg.configFile."herdr" = {
      source = src;
      recursive = true;
    }
  };
}
