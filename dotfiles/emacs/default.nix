{
  config,
  pkgs,
  lib,
  ...
}: let
  cfg = config.pix.dotfiles.emacs;

  configSrc = ./home-files/.emacs.d;
  configPreset = let
    loadLines = lib.concatMapStringsSep "\n"
      (file: ''(load "${file}")'')
      cfg.extraLoadEl;
  in
    pkgs.runCommand "pot-emacs-config-preset" {} ''
      # Prepare files
      PRE_CUSTOM_EL_ANCHOR=";; ANCHOR-PRE-CUSTOM-EL"

      mkdir -p "$out"
      cp -r "${configSrc}"/. "$out/"

      # Patch init.el
      substituteInPlace "$out/init.el" \
        --replace-fail "$PRE_CUSTOM_EL_ANCHOR" "$PRE_CUSTOM_EL_ANCHOR
      ${loadLines}"
    '';
  configExtra = pkgs.linkFarm "pot-emacs-config-extra" cfg.extraFiles;
  emacsConfig = pkgs.symlinkJoin {
    name = "pot-emacs-config";
    paths = [
      configPreset
      configExtra
    ];
  };
in {
  options.pix.dotfiles = {
    emacs = {
      enable = lib.mkEnableOption "Pot Emacs";

      extraLoadEl = lib.mkOption {
        type = lib.types.listOf lib.types.path;
        description = "Extra .el files to be loaded in init.el";
        default = [];
      };

      extraFiles = lib.mkOption {
        type = lib.types.attrsOf lib.types.path;
        description = "Extra files to be copied into the config. Names are relative paths to the toplevel config";
        default = {};
        example = lib.literalExpression ''
          {
            "lisp/foo.el" = /somewhere/foo.el;
            "snippets" = /path/to/dir;
          }
        '';
      };
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.pixPkgs.pot-emacs
      pkgs.pixPkgs.pot-spelling
    ];

    home.file.".emacs.d" = {
      source = emacsConfig;
      recursive = true;
    };
  };
}
