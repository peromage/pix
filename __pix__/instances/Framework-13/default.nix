### Instance for my 13-inch 12th-gen-Intel Framework laptop

{ lib, pix, ... }:

{
  imports = [
    ./hardware.nix
    ./mounts.nix
    pix.nixosModules.default
  ];

  system.stateVersion = "24.05";

  pix = {
    hosts.profiles.PRMG.enable = true;
    users.profiles.fang.enable = true;
    users.profiles.root.enable = false;
  };
}
