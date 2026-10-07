self: let
  lib = (self.getInputs "").nixpkgs.lib;
in {
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
    lib.getAttrs firstLevelNames
    (lib.mapAttrs
      (n: v: lib.mkMerge v)
      (lib.foldAttrs (n: a: [n] ++ a) [] listOfAttrs));

  /*
  Merge multiple module block conditonally.

  To leverage lazyness and avoid infinit recursion when some module blocks
  need to be evaluated conditionally.

  Type:
    mkMergeIf :: [{ cond :: Bool, as :: AttrSet }] -> AttrSet
  */
  mkMergeIf = listOfAttrs: lib.mkMerge (map (x: lib.mkIf x.cond x.as) listOfAttrs);

  /*
  Shorthand to declare options with some presets.

  Type:
    mkEnableOption :: String -> AttrSet -> AttrSet
  */
  mkPresetEnableOption = name: options:
    {
      enable = lib.mkEnableOption name;
      passthru = lib.mkOption {};
    }
    // options;
}
