{ config, lib, pkgs, ... }:

let
  cfg = config.pix.services.steam;

  customSteam = pkgs.steam.override {
    extraPkgs = spkgs: with spkgs; [
      xorg.libXcursor
      xorg.libXi
      xorg.libXinerama
      xorg.libXScrnSaver
      libpng
      libpulseaudio
      libvorbis
      stdenv.cc.cc.lib
      libkrb5
      keyutils
      steamPackages.steamcmd
      steamPackages.steam-runtime
      wqy_zenhei
      wqy_microhei
    ];
  };

in {
  options.pix.services.steam = {
    enable = lib.mkEnableOption "Steam";
    openFirewall = {
      remotePlay = lib.mkEnableOption "Steam remote play ports";
      sourceServer = lib.mkEnableOption "Steam Source server ports";
    };
  };

  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      hardware.steam-hardware.enable = true;

      programs.steam = {
        enable = true;
        gamescopeSession.enable = true;
        package = customSteam;
      };

      environment.systemPackages = with pkgs; [
        steam-run
        protonup-qt
      ];
    })

    (lib.mkIf (cfg.enable && cfg.openFirewall.remotePlay) {
      programs.steam.remotePlay.openFirewall = true;
    })

    (lib.mkIf (cfg.enable && cfg.openFirewall.sourceServer) {
      programs.steam.dedicatedServer.openFirewall = true;
    })
  ];
}
