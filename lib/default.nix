{nixpkgs, ...} @ inputs: let
  lib = nixpkgs.lib;

  prelude = final: {
    inherit inputs;
    overlays = [];
    supportedSystems = [
      "x86_64-linux"
      "aarch64-linux"
      "x86_64-darwin"
      "aarch64-darwin"
    ];

    forEachSupportedSystems = lib.genAttrs final.supportedSystems;

    /*
    A wrapper function that returns an attrset of flake inputs with OS
    specific flakes substituted.
    Passing an empty string returns an unfiltered input attrset

    Example input flake names:
      nixpkgs                 -> Common input
      nixpkgs__darwin         -> OS input
      nixpkgs__aarch64-darwin -> System input

    Precedence: Common < OS < System
    */
    getInputs = system:
      if system == ""
      then final.inputs
      else let
        # e.g. x86_64-linux -> __linux
        osSuffix = "__${lib.elemAt (lib.match "[[:alnum:]-_]+-([[:alpha:]]+)" system) 0}";
        sysSuffix = "__${system}";
        # e.g. nixpkgs__darwin
        hasInfix = lib.hasInfix "__";
        # Remove the suffix of OS or system inputs
        normalizeInput = suffix:
        lib.mapAttrs'
          (name: value: lib.nameValuePair (lib.removeSuffix suffix name) value)
          (lib.filterAttrs (name: _: lib.hasSuffix suffix name) final.inputs);

        commonInputs = lib.filterAttrs (name: _: ! hasInfix name) final.inputs;
        osInputs = normalizeInput osSuffix;
        sysInputs = normalizeInput sysSuffix;
      in
        # common < OS < system
        commonInputs // osInputs // sysInputs;
  };

  libpix = self:
    lib.foldl (acc: x: acc // (import x self)) (lib.fix prelude) [
      ./filesystem.nix
      ./modules.nix
      ./trivial.nix
    ];
in
  lib.makeExtensible libpix
