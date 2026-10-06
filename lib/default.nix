{libnixpkgs}: let
  lib = self:
    libnixpkgs.foldl' (acc: x: acc // (libnixpkgs.callPackageWith {inherit libnixpkgs self;} x {})) {} [
      ./filesystem.nix
      ./modules.nix
      ./trivial.nix
    ];
in
  libnixpkgs.makeExtensible lib
