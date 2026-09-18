{
  config,
  pkgs,
  lib,
  ...
}: let
  cfg = config.pix.dotfiles.vscode;
  src = ./home-files/.config/Code;
in {
  options.pix.dotfiles.vscode = {
    enable = lib.mkEnableOption "Pot Visual Studio Code";
    passthru = lib.mkOption {};
  };

  config = lib.mkIf cfg.enable {
    programs.vscode =
      {
        enable = true;
      }
      // cfg.passthru;

    xdg.configFile."Code" = {
      source = src;
      recursive = true;
    };
  };
}
