{ config, lib, pix, ... }:

let
  cfg = config.pix.dotfiles.fish;
  src = ./home/.config/fish;

in with lib; {
  options.pix.dotfiles.fish = {
    enable = mkEnableOption "Fish";
  };

  config = mkIf cfg.enable {
    programs.fish = {
      enable = true;
      shellInit = "";
      loginShellInit = "";
      interactiveShellInit = ''
        source ${src}/config.fish
      '';
    };

    xdg.configFile = {
      "fish/functions" = {
        source = "${src}/functions";
        recursive = true;
      };
    };
  };
}
