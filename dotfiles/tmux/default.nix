{ config, lib, ... }:

let
  cfg = config.pix.dotfiles.tmux;
  src = ./home/.config/tmux;

in with lib; {
  options.pix.dotfiles.tmux = {
    enable = mkEnableOption "Pot Tmux";
  };

  config = mkIf cfg.enable {
    programs.tmux.enable = true;

    xdg.configFile."tmux" = {
      source = src;
      recursive = true;
    };
  };
}
