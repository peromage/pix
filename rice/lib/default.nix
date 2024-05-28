### Top level of the common code

## This lib only requires nixpkgs with an exception of other flakes from the top
## level flake inputs, e.g. home-manager, which should be include in specialArgs.
{ nixpkgs, specialArgs ? {}, ... }:

let
  inherit (nixpkgs.lib) foldl';

  prelude = let
    f = import ./prelude.nix;
    x = f { inherit nixpkgs; self = x; };
  in x;

  call = args: node: with prelude; foldl'
    (a: i: a // i)
    {}
    (callAllWithArgs args
      (listDir (n: t: isNotDefaultNix n t && isImportable n t) node));

  ## Import all nix files within this folder
  librice = let
    args = specialArgs // {
      ## Note: This nixpkgs will be used if it is different from the one in specialArgs
      inherit nixpkgs specialArgs;
      self = librice;
    };
  in call args ./.;

in
librice
