{ config, lib, ... }:

let
  cfg = config.pix.dotfiles.fish;
  src = ./home-files/.config/fish;

in {
  options.pix.dotfiles.fish = {
    enable = lib.mkEnableOption "Pot Fish";
  };

  config = lib.mkIf cfg.enable {
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
