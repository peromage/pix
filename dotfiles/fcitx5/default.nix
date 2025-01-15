{ config, lib, pkgs, pix, ... }:

let
  cfg = config.pix.dotfiles.fcitx5;
  src = ./home;

  srcRimeIce = pkgs.fetchFromGitHub {
    owner = "iDvel";
    repo = "rime-ice";
    rev = "2024.12.12";
    hash = "sha256-2QZdlLGZwWIesbjYTE/2yhM1hHGVVp7jR02bR0oqxV0=";
  };

in with lib; {
  options.pix.dotfiles.fcitx5 = {
    enable = mkEnableOption "Fcitx5";
  };

  config = mkIf cfg.enable {
    /* Temporarily disabled since this is problematic in user space.  Instead, the
     IME module should be enabled in NixOS config.
    */
    # i18n.inputMethod = {
    #   enabled = "fcitx5";
    #   fcitx5.addons = with pkgs; [
    #     fcitx5-rime
    #     fcitx5-configtool
    #     fcitx5-chinese-addons
    #     fcitx5-gtk
    #   ];
    # };

    # home.packages = with pkgs; [
    #   librime
    #   rime-cli
    #   rime-data
    # ];

    xdg.configFile."fcitx5" = {
      source = "${src}/.config/fcitx5";
      recursive = true;
    };

    /* To rebuild, cd to this directory and execute:
         rime_deployer --build
    */
    xdg.dataFile."fcitx5/rime" = {
      source = "${srcRimeIce.outPath}";
      recursive = true;
    };
  };
}
