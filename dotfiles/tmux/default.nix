{ config, lib, ... }:

let
  cfg = config.pix.dotfiles.tmux;
  src = ./home/.config/tmux;

in {
  options.pix.dotfiles.tmux = {
    enable = lib.mkEnableOption "Pot Tmux";
  };

  config = lib.mkIf cfg.enable {
    programs.tmux.enable = true;

    xdg.configFile."tmux" = {
      source = src;
      recursive = true;
    };
  };
}
