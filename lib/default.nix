{nixpkgs, ...}@inputs:
let
  lib = nixpkgs.lib;

  prelude = final: {
    overlays = [];
    supportedSystems = [
      "x86_64-linux"
      "aarch64-linux"
      "x86_64-darwin"
      "aarch64-darwin"
    ];

    forEachSupportedSystems = lib.genAttrs final.supportedSystems;

    # A wrapper function that returns an attrset of flake inputs with OS
    # specific flakes substituted.
    # Passing an empty string returns an unfiltered input attrset
    getInputs = system:
    if system == "" then
      inputs
    else
      let
        # e.g. x86_64-linux -> __linux
        osSuffix = "__${lib.elemAt (lib.match "[[:alnum:]-_]+-([[:alpha:]]+)" system) 0}";
        hasSuffix = lib.hasSuffix osSuffix;
        removeSuffix = lib.removeSuffix osSuffix;
        # e.g. nixpkgs__darwin
        hasInfix = lib.hasInfix "__";
        commonInputs = lib.filterAttrs (name: _: ! hasInfix name) inputs;
        osInputs = lib.mapAttrs'
          (name: value: lib.nameValuePair (removeSuffix name) value)
          (lib.filterAttrs (name: _: hasSuffix name) inputs);
      in
        commonInputs // osInputs;
  };

  libpix = self:
    lib.foldl (acc: x: acc // (import x self)) (lib.fix prelude) [
      ./filesystem.nix
      ./modules.nix
      ./trivial.nix
    ];
in
  lib.makeExtensible libpix
