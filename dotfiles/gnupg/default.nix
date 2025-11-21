{ config, lib, pkgs, stdenvNoCC, ... }:

let
  cfg = config.pix.dotfiles.gpg;
  src = ./home-files/.gnupg;
  homeDir = config.home.homeDirectory;

in {
  options.pix.dotfiles.gpg = {
    enable = lib.mkEnableOption "Pot GNUPG";
    pinentryPackage = lib.mkPackageOption pkgs "pinentry-gtk2" { nullable = true; };
  };

  config = lib.mkIf cfg.enable {
    programs.gpg = {
      enable = true;
      scdaemonSettings = {};
    };

    services.gpg-agent = {
      enable = true;
      enableScDaemon = true;
      enableSshSupport = true;
      enableBashIntegration = true;
      enableFishIntegration = true;
      pinentry.package = cfg.pinentryPackage;
    };

    home.packages = lib.optional (cfg.pinentryPackage != null) cfg.pinentryPackage;

    ## Workaround to prevent SSH_AUTH_SOCK being set with wrong value
    ## Ref: https://wiki.archlinux.org/title/GNOME/Keyring#Disabling
    xdg.configFile."autostart/gnome-keyring-ssh.desktop".text = ''
      [Desktop Entry]
      Name=SSH Key Agent
      Type=Application
      Hidden=true
    '';

    ## Override with my own settings
    home.file.".gnupg" = {
      source = pkgs.stdenvNoCC.mkDerivation {
        pname = "my-gnupg-config";
        version = "0.0.1";
        src = src;
        sourceRoot = ".";
        dontPatchShebangs = true;
        installPhase = ''
          cd .gnupg
          sed -i'''''' -e 's#/home/fang#${homeDir}#' gpg-agent.conf
          chmod u+x pinentry-auto.sh
          mkdir $out
          cp * $out/
        '';
      };
      recursive = true;
    };
  };
}
