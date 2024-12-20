{ pix, pkgs, ... }:

let
  inherit (pix.inputs) nixpkgs;

in {
  imports = with pix.lib; listDir isNotDefaultNix ./.;

  nixpkgs = {
    hostPlatform = "x86_64-darwin";
    config = {
      allowUnfree = true;
      allowBroken = true;
    };
  };

  nix = {
    settings.experimental-features = [ "nix-command" "flakes" ];
    package = pkgs.nixVersions.stable;
    registry.nixpkgs.flake = nixpkgs;
    nixPath = [
      "nixpkgs=${nixpkgs}"
      "/nix/var/nix/profiles/per-user/root/channels"
    ];
  };

  ## Not using `nix.useDaemon' here as it removes daemon access from the user
  services.nix-daemon.enable = true;

  system.stateVersion = 4;
}
