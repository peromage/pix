{ config, lib, ... }:

let
  cfg = config.pix.hardware.bootloader;

in with lib; {
  imports = [
    ./grub.nix
    ./lanzaboote.nix
    ./systemd-boot.nix
  ];

  options.pix.hardware.bootloader = {};

  config = let
    enabledBootloaders = lib.filterAttrs (_: v: v.enable) cfg;

  in {
    assertions = [
      {
        ## One or none
        assertion = length (attrNames enabledBootloaders) < 2;
        message = "Only one bootloader can be activated at a time.";
      }
    ];
  };
}
