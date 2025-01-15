{ config, lib, ... }:

let
  cfg = config.pix.dotfiles.vim;
  src = ./home/.vim;

in with lib; {
  options.pix.dotfiles.vim = {
    enable = mkEnableOption "Vim";
  };

  config = mkIf cfg.enable {
    programs.vim = {
      enable = true;
      extraConfig = ''
        source ${src}/vimrc
      '';
    };
  };
}
