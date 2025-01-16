{ nixpkgs, ... }@args:

let
  lib = nixpkgs.lib;
  self = with lib; foldl' (acc: x: acc // (import x (args // { inherit self; }))) {} [
    ./modules.nix
    ./prelude.nix
    ./trivial.nix
  ];

in self
