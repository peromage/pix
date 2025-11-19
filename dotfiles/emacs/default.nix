{ config, pkgs, lib, ... }:

let
  emacsCfg = config.pix.dotfiles.emacs;
  spellingCfg = config.pix.dotfiles.spelling;
  src = ./home-files/.emacs.d;

in {
  options.pix.dotfiles = {
    emacs = {
      enable = lib.mkEnableOption "Pot Emacs";
      package = lib.mkPackageOption pkgs.pixPkgs "emacs" {};
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
}
