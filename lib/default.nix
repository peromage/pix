{ nixpkgs }:

let
  self = with nixpkgs.lib; foldl' (acc: x: acc // (callPackageWith { inherit nixpkgs self; } x {})) {} [
    ./modules.nix
    ./prelude.nix
    ./trivial.nix
  ];

in self
