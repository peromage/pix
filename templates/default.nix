{ ... }:

rec {
  default = pix-flake;
  pix-flake = import ./pix-flake;
}
