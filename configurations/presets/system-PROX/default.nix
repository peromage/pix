{ config, lib, pkgs, ... }:

with lib; {
  /* Pix options */
  pix.system.hostName = "PROX";
  pix.services = {
    i18n.enable = true;
    firewall.enable = true;
    vconsole.enable = true;
    sshd.enable = true;
    nix.enable = true;
  };

  pix.desktops.env.xfce.enable = true;

  environment.systemPackages = with pkgs; [
    brave
    vim
    tmux
    git
    curl
    wget
    rsync
    tree
    pixPkgs.emacs

    ## Filesystems
    ntfs3g
    exfat
    exfatprogs
    e2fsprogs
    fuse
    fuse3
  ];
}
