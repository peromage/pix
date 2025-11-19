{ config, lib, ... }:

let
  cfg = config.pix.dotfiles.git;
  src = ./home-files/.config/git;

in {
  options.pix.dotfiles.git = {
    enable = lib.mkEnableOption "Pot Git";

    includes = lib.mkOption {
      type = with lib.types; listOf (oneOf [attrs str path]);
      default = [];
      description = ''
        Extra inlcudes of git config.

        Accepts a plain paths or strings that points to the config files, or
        attribute sets that are equivalent to the definition in `programs.git.includes';
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    programs.git = {
      enable = true;
      lfs.enable = true;
      includes = [
        { path = "${src}/config"; }
      ] ++ (map (p: if lib.isAttrs p then p else { path = p; }) cfg.includes);
    };
  };
}
