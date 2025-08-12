{ lib, ... }:

/*
   Note for the BTRFS mount options like `nodatacow', only the first mounted
   volume takes effect.
   However, for options like `noatime' can be set separately per volume.
   See: https://btrfs.readthedocs.io/en/latest/Administration.html

   For directories that wish no CoW, use file attributes.
*/

{
  boot.initrd.luks.devices."nixos_root".device = "/dev/disk/by-uuid/ad552a04-8962-42bd-90b5-a7eb09d9862c";

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/6f50d190-f695-4019-83e6-950226839e9f";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/6D48-AED8";
    fsType = "vfat";
    options = [ "fmask=0022" "dmask=0022" ];
  };

  swapDevices = [
    {
      device = "/.swapfile";
    }
  ];

  boot.tmp = {
    useTmpfs = true;
    tmpfsSize = "50%"; ## 16GB
    tmpfsHugeMemoryPages = "never"; ## Huge pages may waste memory
  };
}
