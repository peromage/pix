{ config, pkgs, lib, ...}:

let
  cfg = config.pix.dotfiles.kitty;
  src = ./home-files/.config/kitty;

in {
  options.pix.dotfiles.kitty = {
    enable = lib.mkEnableOption "Kitty";
    package = lib.mkPackageOption pkgs "kitty" {};
  };

  config = lib.mkIf cfg.enable {
    programs.kitty = {
      enable = true;
      package = cfg.package;
    };

    xdg.configFile."kitty" = {
      source = src;
      recursive = true;
    };
  };
}
