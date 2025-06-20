{ config, lib, ... }:

let
  cfg = config.pix.services.ime;
  enabledIME = lib.filterAttrs (_: v: v.enable) cfg;

in {
  imports = [
    ./fcitx5.nix
    ./ibus.nix
  ];

  options.pix.services.ime = {};

  config = {
    assertions = [
      {
        ## One or none
        assertion = lib.length (lib.attrNames enabledIME) < 2;
        message = "Only one IME can be activated at a time.";
      }
    ];
  };
}
