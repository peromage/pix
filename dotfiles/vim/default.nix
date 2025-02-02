{ config, lib, ... }:

let
  cfg = config.pix.dotfiles.vim;
  src = ./home/.vim;

in {
  options.pix.dotfiles.vim = {
    enable = lib.mkEnableOption "Pot Vim";
  };

  config = lib.mkIf cfg.enable {
    programs.vim = {
      enable = true;
      extraConfig = ''
        source ${src}/vimrc
      '';
    };
  };
}
