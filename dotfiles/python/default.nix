{ config, pkgs, lib, ... }:

let
  cfg = config.pix.dotfiles.python;
  myPython = pkgs.callPackage ./pkgs/python.nix { userPyenvDir = "${config.xdg.dataHome}/${p.userPyenvDir}"; };

in with lib; {
  options.pix.dotfiles.python = {
    enable = mkEnableOption "Pot Python3";
  };

  config = mkIf cfg.enable {
    home.sessionVariables = {
      PIP_PREFIX = myPython.userPyenvDir;
      PYTHONPATH = "${myPython.userPythonPath}:$PYTHONPATH";
    };

    home.sessionPath = [
      myPython.userPath
    ];

    home.packages = [ myPython ];
  };
}
