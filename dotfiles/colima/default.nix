{ config, pkgs, lib, ...}:

let
  cfg = config.pix.dotfiles.colima;
  src = ./home-files/.config/colima;

in {
  options.pix.dotfiles.colima = {
    enable = lib.mkEnableOption "Pot Colima";
  };

  config = lib.mkIf cfg.enable {
    services.colima.enable = true;

    xdg.configFile."colima" = {
      source = src;
      recursive = true;
    };
  };
}
