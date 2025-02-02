{ config, lib, pkgs, ... }:

let
  cfg = config.pix.dotfiles.pwsh;
  src = ./home/.config/powershell;

in {
  options.pix.dotfiles.pwsh = {
    enable = lib.mkEnableOption "Pot PowerShell";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.powershell ];

    xdg.configFile."powershell" = {
      source = src;
      recursive = true;
    };
  };
}
