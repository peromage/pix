{ config, lib, pix, ... }:

let
  cfg = config.pix.services.ime;
  enabledIME = lib.filterAttrs (_: v: v.enable) cfg;

in with lib; {
  imports = [
    ./fcitx.nix
    ./ibus.nix
  ];

  options.pix.services.ime = {};

  config = {
    assertions = [
      {
        ## One or none
        assertion = length (attrNames enabledIME) < 2;
        message = "Only one IME can be activated at a time.";
      }
    ];
  };
}
