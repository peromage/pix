{ config, lib, ... }:

let
  cfg = config.pix.dotfiles.vscode;
  src = ./home/.config/Code;

in {
  options.pix.dotfiles.vscode = {
    enable = lib.mkEnableOption "Pot Visual Studio Code";
  };

  config = lib.mkIf cfg.enable {
    programs.vscode.enable = true;

    xdg.configFile."Code" = {
      source = src;
      recursive = true;
    };
  };
}
