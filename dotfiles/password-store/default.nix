{ config, lib, pkgs, ... }:

let
  cfg = config.pix.dotfiles.password-store;

in {
  options.pix.dotfiles.password-store = {
    enable = lib.mkEnableOption "Pot Password Store";
  };

  config = lib.mkIf cfg.enable {
    programs.password-store = {
      enable = true;

      settings = {
        PASSWORD_STORE_DIR = "${config.home.homeDirectory}/.password-store";
        PASSWORD_STORE_CLIP_TIME = "30";
      };

      package = pkgs.pass.withExtensions (exts: with exts; [
        pass-otp
        pass-genphrase
      ]);
    };
  };
}
