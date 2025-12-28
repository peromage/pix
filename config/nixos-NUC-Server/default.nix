{ lib, pix, ... }:

{
  imports = [
    ./hardware.nix
    ../presets/system-PROX
    ../presets/user-wangguan
    ../presets/user-root
  ];
}
