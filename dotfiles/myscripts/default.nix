{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.pix.dotfiles.myscripts;
  mkIfDarwin = cond: lib.mkIf (pkgs.stdenv.isDarwin && cond);
in {
  options.pix.dotfiles.myscripts = {
    enable = lib.mkEnableOption "My Scripts";

    # Options have no effect outside of MacOS
    darwin = {
      fixHomeManagerApps = lib.mkEnableOption "Fix Home Manager App shortcuts.";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [pkgs.pixPkgs.myscripts];

    home.activation.fixMacOSApps = mkIfDarwin cfg.darwin.fixHomeManagerApps (lib.hm.dag.entryAfter ["writeBoundary"] ''
      run ${pkgs.pixPkgs.myscripts}/bin/darwin-fix-homemanager-apps.sh
    '');

    assertions = [
      {
        assertion = !pkgs.stdenv.isDarwin -> lib.all (name: !cfg.darwin.${name}) (lib.attrNames cfg.darwin);
        message = "myscripts.darwin.* options can only be used on Darwin!";
      }
    ];
  };
}
