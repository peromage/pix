{ config, lib, pkgs, ... }:

let
  cfg = config.pix.dotfiles.pwsh;
  src = ./home/.config/powershell;

in with lib; {
  options.pix.dotfiles.pwsh = {
    enable = mkEnableOption "Pot PowerShell";
  };

  config = mkIf cfg.enable {
    home.packages = [ pkgs.powershell ];

    xdg.configFile."powershell" = {
      source = src;
      recursive = true;
    };
  };
}
