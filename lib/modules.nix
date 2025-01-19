{ self, nixpkgs, ... }:

let
  lib = nixpkgs.lib;

in with self; {
  /* A thin wrapper for configuration.
     This function provides ability to override the original configuration by
     calling the underlying `extend' function.

     `conf' is a configuration generation function like `nixosSystem',
     `darwinSystem' or `homeManagerConfiguration'.

     `f' is a fixed-point function that produces the result consumed by `conf'
     function.

     Type:
       mkConfiguration :: (a -> a) -> (a -> a)
  */
  mkConfiguration = conf: f: with lib; conf (fix f) // {
    extend = overlay: mkConfiguration conf (extends overlay f);
  };

  /* Merge a list of attribute sets from config top level.

     NOTE: This is a workaround to solve the infinite recursion issue when trying
     merge configs from top level.  The first level of attribute names must be
     specified explicitly.

     See: https://gist.github.com/udf/4d9301bdc02ab38439fd64fbda06ea43

     Type:
       mkMergeTopLevel :: [String] -> [AttrSet] -> AttrSet
  */
  mkMergeTopLevel = firstLevelNames: listOfAttrs:
    lib.getAttrs firstLevelNames
      (lib.mapAttrs
        (n: v: lib.mkMerge v)
        (lib.foldAttrs (n: a: [n] ++ a) [] listOfAttrs));

  /* Merge multiple module block conditonally.

     To leverage lazyness and avoid infinit recursion when some module blocks
     need to be evaluated conditionally.

     Type:
       mkMergeIf :: [{ cond :: Bool, as :: AttrSet }] -> AttrSet
  */
  mkMergeIf = listOfAttrs: lib.mkMerge (map (x: lib.mkIf x.cond x.as) listOfAttrs);

  /* Apply predicate `f' on each attribute and return true if at least one is true.
     Otherwise, return false.

     Type:
       anyAttrs :: (String -> a -> Bool) -> Bool
  */
  anyAttrs = f: attrs: with lib; any (name: f name attrs.${name}) (attrNames attrs);
}
