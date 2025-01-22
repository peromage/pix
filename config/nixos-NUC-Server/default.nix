{ lib, pix, ... }:

{
  imports = [
    pix.nixosModules.default
    ./hardware.nix
    ../presets/system-PROX
    ../presets/user-wangguan
    ../presets/user-root
  ];

  system.stateVersion = "24.05";
}
