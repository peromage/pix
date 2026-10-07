self: let
  lib = (self.getInputs "").nixpkgs.lib;
  pix = (self.getInputs "").pix;
in {
  /*
  A thin wrapper for configuration.
  This function provides ability to override the original configuration by
  calling the underlying `extend' function.

  `f' is a configuration generation function like `nixosSystem',
  `darwinSystem' or `homeManagerConfiguration'.

  `fp' is a fixed-point function that produces the result consumed by `f'
  function.

  Type:
    makeConfiguration :: (a -> a) -> (a -> a) -> AttrSet
  */
  makeConfiguration = f: fp:
    f (lib.fix fp)
    // {
      extend = overlay: self.makeConfiguration f (lib.extends overlay fp);
    };

  makePkgs = system:
    import (self.getInputs system).nixpkgs {
      inherit system;
      overlays = self.overlays;
    };

  /*
  Note that the `system' attribute is not explicitly set (default to null)
  to allow modules to set it themselves.  This allows a hermetic configuration
  that doesn't depend on the system architecture when it is imported.
  See: https://github.com/NixOS/nixpkgs/pull/177012
  */
  makeNixOS = fn:
    self.makeConfiguration lib.nixosSystem (_: {
      specialArgs = {inherit pix;};
      modules = [
        pix.outputs.nixosModules.default
        {
          nixpkgs.overlays = self.overlays;
          system.stateVersion = pix.meta.stateVersion;
        }
        fn
      ];
    });

  makeDarwin = fn:
    self.makeConfiguration (self.getInputs "").nix-darwin.lib.darwinSystem (_: {
      specialArgs = {inherit pix;};
      modules = [
        {
          system.stateVersion = pix.meta.darwinStateVersion;
        }
        fn
      ];
    });

  makeHome = system: fn:
    self.makeConfiguration (self.getInputs system).home-manager.lib.homeManagerConfiguration (_: {
      pkgs = self.makePkgs system;
      extraSpecialArgs = {inherit pix;};
      modules = [
        pix.outputs.homeModules.default
        {
          home.stateVersion = pix.meta.stateVersion;
        }
        fn
      ];
    });

  /*
  Merge two package sets.

  The package set should look like:

  {
    x86_64-linux = { ... };
    aarch64-darwin = { ... };
    ...
  }

  The second package set merges into the same keys from the first one.

  Type:
    mergePackages :: AttrSet -> AttrSet -> AttrSet
  */
  mergePackages = base: override: base // (lib.genAttrs (lib.attrNames override) (platform: (base.${platform} or {}) // override.${platform}));
}
