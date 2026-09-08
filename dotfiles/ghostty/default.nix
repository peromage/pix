{ config, pkgs, lib, ...}:

let
  cfg = config.pix.dotfiles.ghostty;
  src = ./home-files/.config/ghostty;

in {
  options.pix.dotfiles.ghostty = {
    enable = lib.mkEnableOption "Pot Ghostty";
    package = lib.mkPackageOption pkgs "ghostty" {};
  };

  config = lib.mkIf cfg.enable {
    programs.ghostty = {
      enable = true;
      package = cfg.package;
    };

    xdg.configFile."ghostty" = {
      source = src;
      recursive = true;
    };
  };
}
