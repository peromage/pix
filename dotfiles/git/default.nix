{ config, lib, ... }:

let
  cfg = config.pix.dotfiles.git;
  src = ./home/.config/git;

in {
  options.pix.dotfiles.git = {
    enable = lib.mkEnableOption "Pot Git";

    extraIncludes = lib.mkOption {
      type = with lib.types; listOf attrs;
      default = [];
      description = ''
        Extra inlcudes of git config.
        This is equivalent to `programs.git.includes';
      '';
    };
    # extraIncludes = options.programs.git.includes;
  };

  config = lib.mkIf cfg.enable {
    programs.git = {
      enable = true;
      lfs.enable = true;
      includes = [
        { path = "${src}/config"; }
      ] ++ cfg.extraIncludes;
    };
  };
}
