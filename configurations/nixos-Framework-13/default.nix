### Instance for my 13-inch 12th-gen-Intel Framework laptop

{ lib, pix, ... }:

{
  imports = [
    ./hardware.nix
    ./mounts.nix
    ./stateVersion.nix
    ../../presets/user-fang
    ../../presets/user-root
    pix.nixosModules.default
  ];

  pix = {
    # users.root.nologin = lib.mkForce false;
    # hosts.profiles.PRMG.enable = true;
    # users.profiles.fang.enable = true;
    # users.profiles.root.enable = false;
  };
}
