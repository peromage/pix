{ config, lib, pkgs, ... }:

let
  cfg = config.pix.dotfiles.pot-utils;
  mkIfDarwin = cond: lib.mkIf (pkgs.stdenv.isDarwin && cond);

in {
  options.pix.dotfiles.pot-utils = {
    enable = lib.mkEnableOption "Pot Utils";

    # Options have no effect outside of MacOS
    darwin = {
      fixHomeManagerApps = lib.mkEnableOption "Fix Home Manager App shortcuts.";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.pixPkgs.pot-utils ];

    home.activation.fixMacOSApps = mkIfDarwin cfg.darwin.fixHomeManagerApps (lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      run ${pkgs.pixPkgs.pot-utils}/bin/macos-fix-homemanager-apps.sh
    '');
  };
}
