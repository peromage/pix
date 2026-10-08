{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.pix.dotfiles.gpg;
  configSrc = ./home-files/.gnupg;
  gnupgConfig = pkgs.runCommand "pot-gnupg-config" {} ''
    mkdir -p "$out"
    cp -r "${configSrc}/." "$out"
    sed -i"" "s#/home/fang/#${config.home.homeDirectory}/#" "$out/gpg-agent.conf"
    chmod 600 "$out"/*
    chmod u+x "$out/pinentry-auto.sh"
  '';
in {
  options.pix.dotfiles.gpg = {
    enable = lib.mkEnableOption "Pot GNUPG";
    pinentryPackage = lib.mkPackageOption pkgs "pinentry-gtk2" {nullable = true;};
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
      source = gnupgConfig;
      recursive = true;
    };
  };
}
