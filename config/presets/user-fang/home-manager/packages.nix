{ pix, pkgs, ... }:

{
  /*
     Pre-configured packages
  */
  pix.dotfiles = {
    bash.enable = true;
    emacs.enable = true;
    spelling.enable = true;
    fcitx5.enable = true;
    fish.enable = true;
    git = {
      enable = true;
      extraIncludes = [
        { path = "${pix.outPath}/dotfiles/git/home-files/.config/git/user-fang"; }
      ];
    };
    gpg.enable = true;
    password-store.enable = true;
    pwsh.enable = true;
    tmux.enable = true;
    vim.enable = true;
    wezterm.enable = true;
    gnome = {
      enableKeyboardShortcuts = true;
      enableGnomeTerminalConfig = true;
    };
    python.enable = true;
    nodejs.enable = true;
  };

  home.packages = with pkgs; [
    ## Daily
    pixPkgs.pot-utils
    ripgrep
    stow

    ## Data transfer
    wget
    curl
    aria2
    rsync

    ## Fancy stuff
    neofetch
    btop # Replace `htop'
    eza # Replace `ls'
    fzf
    jq # Json parser

    ## Development
    dotnet-sdk_8
    lua
    clang-tools
    shellcheck
    nixd
    pixPkgs.build-essential
    unrestrictedPkgs.android-studio
    qtscrcpy
    scrcpy

    ## Devices
    android-tools

    ## Productivity
    graphviz
    hugo
    libreoffice-fresh
    gimp
    kdePackages.kdenlive
    flameshot
    diceware
    yt-dlp
    zbar
    ffmpeg
    exiftool
    imagemagick

    ## Drawing
    libwacom
    krita

    ## GUI
    ## Some apps like browser and steam prefer to be installed via flatpak since
    ## they're updated often while nixpkgs (flake) is not
    remmina
    zeal
    mpv
    vlc

    ## Wayland
    wl-clipboard

    ## Entertainment
    prismlauncher
  ];
}
