{ libnix }:

let
  lib = self: libnix.foldl' (acc: x: acc // (libnix.callPackageWith { inherit libnix self; } x {})) {} [
    ./filesystem.nix
    ./modules.nix
    ./trivial.nix
  ];

in libnix.makeExtensible lib
