{self, libnixpkgs}: {
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
    f (libnixpkgs.fix fp)
    // {
      extend = overlay: makeConfiguration f (libnixpkgs.extends overlay fp);
    };
}
