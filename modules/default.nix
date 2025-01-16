{ pix, ... }@args:
{
  imports = map (m: import m args) [
    ./options.nix
    ./desktops/
    ./hardware/
    ./services/
  ];
}
