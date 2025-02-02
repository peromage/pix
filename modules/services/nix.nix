{ config, lib, pkgs, pix, ... }:

let
  cfg = config.pix.services.nix;

in {
  options.pix.services.nix = {
    enable = lib.mkEnableOption "Nix settings";
    enableOptimization = lib.mkEnableOption "Nix optimization" // { default = true; };
  };

  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      nixpkgs = {
        hostPlatform = lib.mkDefault "x86_64-linux";
        config = {
          allowUnfree = true;
          allowBroken = true;
        };
        ## Make sure <nixpkgs> is consistent with the one from flake
        flake = {
          setNixPath = true;
          setFlakeRegistry = true;
        };
      };

      nix = {
        enable = true;
        channel.enable = true;

        settings = {
          sandbox = true;
          experimental-features = [ "nix-command" "flakes" ];
          trusted-users = [
            "@wheel"
          ];
          allowed-users = [
            "@wheel"
            "@users"
          ];
        };

        /* Synonyms
             pkgs.nixVersions.stable -> pkgs.nix, pkgs.nixFlakes, pkgs.nixStable
             pkgs.nixVersions.unstable -> pkgs.nixUnstable
             See: https://github.com/NixOS/nixpkgs/blob/master/pkgs/top-level/aliases.nix
          */
        package = pkgs.nixVersions.stable;

        nixPath = [ "nixpkgs=${pix.inputs.nixpkgs}" ];
      };
    })

    (lib.mkIf (cfg.enable && cfg.enableOptimization) {
      nix = {
        settings.auto-optimise-store = true; ## Use hard links to save space

        optimise = {
          automatic = true;
          dates = [ "weekly" ];
        };

        gc = {
          automatic = true;
          persistent = true;
          dates = "weekly";
          options = ""; ## Use "nix-collect-garbage --delete-older-than 30d" to purge old system profiles
          randomizedDelaySec = "0";
        };
      };
    })
  ];
}
