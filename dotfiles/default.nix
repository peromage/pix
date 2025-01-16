{ pix, ... }@args:
{
  imports = map (m: import m args) [
    ./alacritty
    ./bash
    ./emacs
    ./fcitx5
    ./fish
    ./gnome
    ./gnupg
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
}
