{ lib, ... }:

let
  withDefaultSubvolOptions =
    { vol
    , optionOverrides ? []
    }: {
      device = "/dev/disk/by-uuid/35154f6e-27aa-49f8-b1b6-6472127cb524";
      fsType = "btrfs";
      options = if [] != optionOverrides then optionOverrides else [
        "subvol=${vol}"
        "ssd"
        "noatime"
        "autodefrag"
        "compress=zstd:3"
      ];
    };

in {
  fileSystems."/" = withDefaultSubvolOptions { vol = "@nixos"; };
  fileSystems."/nix" = withDefaultSubvolOptions { vol = "@nix"; };
  fileSystems."/home" = withDefaultSubvolOptions { vol = "@home"; };
  fileSystems."/vol/swap" = withDefaultSubvolOptions { vol = "@swap"; };
  fileSystems."/vol/vm" = withDefaultSubvolOptions { vol = "@vm"; };
  fileSystems."/vol/snapshot" = withDefaultSubvolOptions { vol = "@snapshot"; };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/8F86-D998";
    fsType = "vfat";
  };

  swapDevices = [
    {
      device = "/vol/swap/32gb.img";
    }
  ];
}
