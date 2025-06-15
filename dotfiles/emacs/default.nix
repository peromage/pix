{ config, pkgs, lib, ... }:

let
  emacsCfg = config.pix.dotfiles.emacs;
  spellingCfg = config.pix.dotfiles.spelling;
  src = ./home-files/.emacs.d;
  myEmacs = pkgs.callPackage ./pkgs/emacs.nix {};
  mySpelling = pkgs.callPackage ./pkgs/spelling.nix {};

in {
  options.pix.dotfiles = {
    emacs.enable = lib.mkEnableOption "Pot Emacs";
    spelling.enable = lib.mkEnableOption "Pot Spelling";
  };

  config = lib.mkMerge [
    (lib.mkIf emacsCfg.enable {
      home.packages = [ myEmacs ];

      home.file.".emacs.d" = {
        source = src;
        recursive = true;
      };
    })

    (lib.mkIf spellingCfg.enable {
      home.packages = [ mySpelling ];
    })
  ];
}
