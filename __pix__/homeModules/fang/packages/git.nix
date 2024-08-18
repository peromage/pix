{ pix, ... }:

let
  src = "${pix.path.dotfiles}/git/.config/git";

in {
  programs.git = {
    enable = true;
    lfs.enable = true;
    includes = [
      { path = "${src}/config"; }
      { path = "${src}/user-fang"; }
    ];
  };
}
