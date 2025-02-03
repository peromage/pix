{ lib, ... }:

{
  config.pix.users.fang = {
    description = "Fang The Handsome";
    id = 1001;
    groups = [ "wheel" "users" "audio" "video" "cdrom" "networkmanager" ];
    enableNixManagement = true;
    password = lib.mkDefault "P@55w0rd";
  };
}
