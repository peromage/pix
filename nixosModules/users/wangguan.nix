{ lib, ... }:

{
  config.pix.users.profiles.wangguan = with lib; {
    description = "Wangguan";
    id = 1100;
    groups = [ "wheel" "users" "networkmanager"];
    enableNixManagement = true;
    password = mkDefault "wangguan123"; # Hardened offline
  };
}
