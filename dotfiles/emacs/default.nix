{ config, pkgs, lib, ... }:

let
  emacsCfg = config.pix.dotfiles.emacs;
  spellingCfg = config.pix.dotfiles.spelling;

in {
  options.pix.dotfiles = {
    emacs = {
      enable = lib.mkEnableOption "Pot Emacs";
      package = lib.mkPackageOption pkgs.pixPkgs "pot-emacs" {};
      configPackage = lib.mkPackageOption pkgs.pixPkgs "pot-emacs-config" {};
    };

    spelling = {
      enable = lib.mkEnableOption "Pot Spelling";
      package = lib.mkPackageOption pkgs.pixPkgs "spelling" {};
    };
  };

  config = lib.mkMerge [
    (lib.mkIf emacsCfg.enable {
      home.packages = [ emacsCfg.package  emacsCfg.configPackage ];

      home.file.".emacs.d" = {
        source = "${emacsCfg.configPackage}/dot-emacs-d";
        recursive = true;
      };
    })

    (lib.mkIf spellingCfg.enable {
      home.packages = [ spellingCfg.package ];
    })
  ];
}
