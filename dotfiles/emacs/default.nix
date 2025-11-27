{ config, pkgs, lib, ... }:

let
  emacsCfg = config.pix.dotfiles.emacs;
  spellingCfg = config.pix.dotfiles.spelling;

  extraSrc = pkgs.stdenvNoCC.mkDerivation {
    pname = "pot-emacs-extra-load-files";
    version = "0.0.1";
    src = emacsCfg.loadFiles;
    sourceRoot = ".";

    installPhase = ''
      DIR=$out/extra-load-files
      DEFAULT=$DIR/default.el

      mkdir -p $DIR
      install -m 644 $src/* $DIR/
      touch $DEFAULT

      for f in $DIR/*; do
        echo "(load \"$f\")" >$DEFAULT
      done
    '';
  };

  src = pkgs.stdenvNoCC.mkDerivation {
    name = "pot-emacs-config";
    version = "0.0.1";
    src = [ ./home-files/.emacs.d extraSrc ];
    sourceRoot = ".";

    installPhase = ''
      mkdir $out

    '';
  };

in {
  options.pix.dotfiles = {
    emacs = {
      enable = lib.mkEnableOption "Pot Emacs";
      package = lib.mkPackageOption pkgs.pixPkgs "emacs" {};

      loadFiles = lib.mkOption {
        type = with lib.types; listOf (either path str);
        default = [];
        description = ''
          A list of additional files to load on top of shipped configuration.
        '';
      };
    };

    spelling = {
      enable = lib.mkEnableOption "Pot Spelling";
      package = lib.mkPackageOption pkgs.pixPkgs "spelling" {};
    };
  };

  config = lib.mkMerge [
    (lib.mkIf emacsCfg.enable {
      home.packages = [ emacsCfg.package ];

      home.file.".emacs.d" = {
        source = src;
        recursive = true;
      };
    })

    (lib.mkIf spellingCfg.enable {
      home.packages = [ spellingCfg.package ];
    })
  ];

  assertions = [
    {
      assertion = with builtins; all (f: readFileType f == "regular") emacsCfg.loadFiles;
      message = "All paths must be regular files.";
    }
  ];
}
