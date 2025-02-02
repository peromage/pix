{ config, pkgs, lib, ... }:

let
  cfg = config.pix.dotfiles.python;
  myPython = let python = pkgs.callPackage ./pkgs/python.nix {}; in python.override {
    userPyenvDir = "${config.xdg.dataHome}/${python.userPyenvDir}";
  };

in {
  options.pix.dotfiles.python = {
    enable = lib.mkEnableOption "Pot Python3";
  };

  config = lib.mkIf cfg.enable {
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
