{
  self,
  libnixpkgs,
}:
with self; {
  /*
  Merge a list of attribute sets from config top level.

  NOTE: This is a workaround to solve the infinite recursion issue when trying
  merge configs from top level.  The first level of attribute names must be
  specified explicitly.

  See: https://gist.github.com/udf/4d9301bdc02ab38439fd64fbda06ea43

  Type:
    mkMergeTopLevel :: [String] -> [AttrSet] -> AttrSet
  */
  mkMergeTopLevel = firstLevelNames: listOfAttrs:
    libnixpkgs.getAttrs firstLevelNames
    (libnixpkgs.mapAttrs
      (n: v: libnixpkgs.mkMerge v)
      (libnixpkgs.foldAttrs (n: a: [n] ++ a) [] listOfAttrs));

  /*
  Merge multiple module block conditonally.

  To leverage lazyness and avoid infinit recursion when some module blocks
  need to be evaluated conditionally.

  Type:
    mkMergeIf :: [{ cond :: Bool, as :: AttrSet }] -> AttrSet
  */
  mkMergeIf = listOfAttrs: libnixpkgs.mkMerge (map (x: libnixpkgs.mkIf x.cond x.as) listOfAttrs);

  /*
  Shorthand to declare options with some presets.

  Type:
    mkEnableOption :: String -> AttrSet -> AttrSet
  */
  mkPresetEnableOption = name: options:
    {
      enable = libnixpkgs.mkEnableOption name;
      passthru = libnixpkgs.mkOption {};
    }
    // options;

  /*
  Merge two package sets from flakes.

  The package set should be like:

  {
    x86_64-linux = { ... };
    aarch64-darwin = { ... };
    ...
  }

  The second package set overwrites the same keys from the first one.

  Type:
    mergePackages :: AttrSet -> AttrSet -> AttrSet
  */
  mergePackages = pa: pb: builtins.mapAttrs (name: value: value // (pb.${name} or {})) pa;
}
