{ config, lib, pix, ... }:

let
  cfg = config.pix.dotfiles.git;
  src = ./home/.config/git;

in with lib; {
  options.pix.dotfiles.git = {
    enable = mkEnableOption "Git";

    extraIncludes = mkOption {
      type = with types; listOf attrs;
      default = [];
      description = ''
        Extra inlcudes of git config.
        This is equivalent to `programs.git.includes';
      '';
    };
    # extraIncludes = options.programs.git.includes;
  };

  config = mkIf cfg.enable {
    programs.git = {
      enable = true;
      lfs.enable = true;
      includes = [
        { path = "${src}/config"; }
      ] ++ cfg.extraIncludes;
    };
  };
}
