### Instance for my 13-inch 12th-gen-Intel Framework laptop

{ lib, pix, ... }:

{
  imports = [
    ./hardware.nix
    ./mounts.nix
    ../presets/user-fang
    ../presets/user-root
  ];

  system.stateVersion = "24.05";
}
