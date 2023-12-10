### Instance for my 13-inch 12th-gen-Intel Framework laptop

{ lib, rice, ... }:

let
  librice = rice.lib;
  arch = "x86_64-linux";

in {
  imports = [
    ./boot.nix
    ./mounts.nix
    librice.moduleTopLevel
  ] ++ librice.getModules [
    "hosts/potpie"
  ];

  nixpkgs.hostPlatform = arch;

  rice = {
    hosts.hostName = "Framepie";

    users.users.fang.enable = true;

    desktops.env.gnome.enable = true;

    hardware = {
      secureBoot.enable = true;

      powerGovernor = {
        enable = true;
        profile = "powersave";
      };

      firmware.enable = true;
      peripherals.printing = true;
    };

    services = {
      ssh.enable = true;
    };
  };
}
