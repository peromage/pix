{ nixpkgs }:

let
  lib = self: with nixpkgs.lib; foldl' (acc: x: acc // (callPackageWith { inherit nixpkgs self; } x {})) {} [
    ./filesystem.nix
    ./modules.nix
    ./trivial.nix
  ];

in nixpkgs.lib.makeExtensible lib
