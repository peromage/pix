{ config, lib, pkgs, ... }:

let
  cfg = config.pix.dotfiles.pot-utils;

in {
  options.pix.dotfiles.pot-utils = {
    enable = lib.mkEnableOption "Pot Utils";
    fixMacOSApps = lib.mkEnableOption "Fix App shortcuts on MacOS";
  };

  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      home.packages = [ pkgs.pixPkgs.pot-utils ];
    })

    (lib.mkIf cfg.fixMacOSApps {
      home.activation.fixMacOSApps = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        run ${pkgs.pixPkgs.pot-utils}/bin/fix-macos-hm-apps.sh
      '';

      assertions = [
        {
          assertion = with builtins; (match ".+-darwin" pkgs.stdenv.hostPlatform.system) != null;
          message = "pix.pot-utils.fixMacOSApps must be used on MacOS!";
        }
      ];
    })
  ];
}
