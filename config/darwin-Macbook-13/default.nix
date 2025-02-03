{ pix, pkgs, ... }:

{
  imports = [
    ./homebrew.nix
    ./packages.nix
  ];

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
      "nixpkgs=${pix.inputs.nixpkgs}"
      "/nix/var/nix/profiles/per-user/root/channels"
    ];
  };

  ## Not using `nix.useDaemon' here as it removes daemon access from the user
  services.nix-daemon.enable = true;

  system.stateVersion = 4;
}
