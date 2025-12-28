{ pix, ... }:
{
  imports = [
    ./alacritty
    ./bash
    ./emacs
    ./fcitx5
    ./fish
    ./git
    ./gnome
    ./gnupg
    ./kitty
    ./mc
    ./nodejs
    ./password-store
    ./pot-utils
    ./pwsh
    ./python
    ./tigervnc
    ./tmux
    ./vim
    ./vscode
    ./wezterm
    ./zellij
  ];

  /*
     Managed by Home Manager
  */
  programs.home-manager.enable = true;


  /*
     Mapped path from Home Manager's variables:

     `~/.config': config.xdg.configHome
     `~/.local/share': config.xdg.dataHome

     Shorthands for creating files under directories:

     `~': home.file.<name>
     Ref: https://nix-community.github.io/home-manager/options.html#opt-home.file

     `~/.config': xdg.configFile.<name>
     Ref: https://nix-community.github.io/home-manager/options.html#opt-xdg.configFile

     `~/.local/share': xdg.dataFile.<name>
     Ref: https://nix-community.github.io/home-manager/options.html#opt-xdg.dataFile
  */
  xdg.enable = true;
}
