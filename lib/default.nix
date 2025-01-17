{ nixpkgs }:

let
  lib = nixpkgs.lib;
  self = with lib; foldl' (acc: x: acc // (import x { inherit nixpkgs self; })) {} [
    ./modules.nix
    ./prelude.nix
    ./trivial.nix
  ];

in self
