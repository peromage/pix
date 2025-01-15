{ config, pkgs, lib, ... }:

let
  cfg = config.pix.dotfiles.python;
  python = let p = pkgs.pixPkgs.python; in p.override {
    userPyenvDir = "${config.xdg.dataHome}/${p.userPyenvDir}";
  };

in with lib; {
  options.pix.dotfiles.python = {
    enable = mkEnableOption "Python3";
  };

  config = mkIf cfg.enable {
    home.sessionVariables = {
      PIP_PREFIX = python.userPyenvDir;
      PYTHONPATH = "${python.userPythonPath}:$PYTHONPATH";
    };
    home.sessionPath = [
      python.userPath
    ];
    home.packages = [ python ];
  };
}
