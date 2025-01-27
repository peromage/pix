{ config, lib, ... }:

let
  inherit (lib)
    length
    attrNames
    filterAttrs;

  cfg = config.pix.hardware.bootloader;

in {
  imports = [
    ./grub.nix
    ./lanzaboote.nix
    ./systemd-boot.nix
  ];

  options.pix.hardware.bootloader = {};

  config = let
    enabledBootloaders = filterAttrs (_: v: v.enable) cfg;

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
