{ config, lib, pix, ... }:

let
  cfg = config.pix.services.ime;
  libpix = pix.lib;

in with lib; {
  imports = with libpix; listDir isNotDefaultNix ./.;

  options.pix.services.ime = {};

  config = let
    enabledIME = filterAttrs (_: config: config.enable) cfg;

  in {
    assertions = [
      {
        ## One or none
        assertion = length (attrNames enabledIME) < 2;
        message = "Only one IME can be activated at a time.";
      }
    ];
  };
}
