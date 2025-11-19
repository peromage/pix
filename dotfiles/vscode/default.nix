{ config, pkgs, lib, ... }:

let
  cfg = config.pix.dotfiles.vscode;
  src = ./home-files/.config/Code;

in {
  options.pix.dotfiles.vscode = {
    enable = lib.mkEnableOption "Pot Visual Studio Code";
    package = lib.mkPackageOption pkgs "vscode" {};
  };

  config = lib.mkIf cfg.enable {
    programs.vscode = {
      enable = true;
      package = cfg.package;
    };

    xdg.configFile."Code" = {
      source = src;
      recursive = true;
    };
  };
}
