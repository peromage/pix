{ lib, ... }:

{
  config.pix.users.wangguan = {
    description = "Wangguan";
    id = 1100;
    groups = [ "wheel" "users" "networkmanager"];
    enableNixManagement = true;
    password = lib.mkDefault "wangguan123"; # Hardened offline
  };
}
