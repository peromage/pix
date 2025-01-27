{ self, nixpkgs }:

let
  inherit (nixpkgs.lib)
    fix
    extends
    getAttrs
    mapAttrs
    mkMerge
    mkIf
    foldAttrs
    any
    attrNames;

in with self; {
  /* A thin wrapper for configuration.
     This function provides ability to override the original configuration by
     calling the underlying `extend' function.

     `f' is a configuration generation function like `nixosSystem',
     `darwinSystem' or `homeManagerConfiguration'.

     `fp' is a fixed-point function that produces the result consumed by `f'
     function.

     Type:
       makeConfiguration :: (a -> a) -> (a -> a)
  */
  makeConfiguration = f: fp: f (fix fp) // {
    extend = overlay: makeConfiguration f (extends overlay fp);
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
    getAttrs firstLevelNames
      (mapAttrs
        (n: v: mkMerge v)
        (foldAttrs (n: a: [n] ++ a) [] listOfAttrs));

  /* Merge multiple module block conditonally.

     To leverage lazyness and avoid infinit recursion when some module blocks
     need to be evaluated conditionally.

     Type:
       mkMergeIf :: [{ cond :: Bool, as :: AttrSet }] -> AttrSet
  */
  mkMergeIf = listOfAttrs: mkMerge (map (x: mkIf x.cond x.as) listOfAttrs);

  /* Apply predicate `f' on each attribute and return true if at least one is true.
     Otherwise, return false.

     Type:
       anyAttrs :: (String -> a -> Bool) -> Bool
  */
  anyAttrs = f: attrs: any (name: f name attrs.${name}) (attrNames attrs);
}
