{
  config,
  pkgs,
  lib,
  ...
}: let
  cfg = config.pix.dotfiles.fish;
  src = ./home-files/.config/fish;
in {
  options.pix.dotfiles.fish = {
    enable = lib.mkEnableOption "Pot Fish";

    init = lib.mkOption {
      type = lib.types.str;
      default = "";
      description = "Additional interactive shell init code.";
    };

    passthru = lib.mkOption {};
  };

  config = lib.mkIf cfg.enable {
    programs.fish =
      {
        enable = true;
        shellInit = "";
        loginShellInit = "";
        interactiveShellInit =
          ''
            source ${src}/config.fish
          ''
          + cfg.init;
      }
      // cfg.passthru;

    xdg.configFile = {
      "fish/functions" = {
        source = "${src}/functions";
        recursive = true;
      };
    };
  };
}
