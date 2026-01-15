{ config, pkgs, lib, ... }:

let
  cfg = config.pix.dotfiles.fish;
  src = ./home-files/.config/fish;

in {
  options.pix.dotfiles.fish = {
    enable = lib.mkEnableOption "Pot Fish";
    package = lib.mkPackageOption pkgs "fish" {};

    init = lib.mkOption {
      type = lib.types.str;
      default = "";
      description = "Additional interactive shell init code.";
    };
  };

  config = lib.mkIf cfg.enable {
    programs.fish = {
      enable = true;
      package = cfg.package;
      shellInit = "";
      loginShellInit = "";
      interactiveShellInit = ''
        source ${src}/config.fish
      '' + cfg.init;
    };

    xdg.configFile = {
      "fish/functions" = {
        source = "${src}/functions";
        recursive = true;
      };
    };
  };
}
