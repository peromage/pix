{ config, lib, ... }:

let
  cfg = config.pix.dotfiles.vscode;
  src = ./home/.config/Code;

in with lib; {
  options.pix.dotfiles.vscode = {
    enable = mkEnableOption "Pot Visual Studio Code";
  };

  config = mkIf cfg.enable {
    programs.vscode.enable = true;

    xdg.configFile."Code" = {
      source = src;
      recursive = true;
    };
  };
}
