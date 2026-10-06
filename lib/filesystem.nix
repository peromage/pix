{
  self,
  libnixpkgs,
}:
with self; {
  /*
  Simplified version of `libnixpkgs.callPackageWith'.
  This function doesn't add override attribute to the result.

  Type:
    autoCall :: AttrSet -> (AttrSet -> a) -> AttrSet -> a
  */
  autoCall = autoArgs: fn: args: let
    f =
      if libnixpkgs.isFunction fn
      then fn
      else import fn;
    passedArgs = libnixpkgs.intersectAttrs (libnixpkgs.functionArgs f) autoArgs // args;
  in
    f passedArgs;

  /*
  A generic function that filters all the files/directories under the given
  directory.  Return a list of names prepended with the given directory.

  Type:
    listDir :: (String -> String -> Bool) -> Path -> [String]
  */
  listDir = pred: dir:
    libnixpkgs.attrNames
    (libnixpkgs.filterAttrs pred (libnixpkgs.mapAttrs'
      (name: type: libnixpkgs.nameValuePair (libnixpkgs.toString (dir + "/${name}")) type)
      (libnixpkgs.readDir dir)));

  /*
  Predications used for `listDir'.
  */
  notPred = pred: name: type: ! pred name type;
  andPred = predA: predB: name: type: predA name type && predB name type;
  orPred = predA: predB: name: type: predA name type || predB name type;

  isDirectoryType = name: type: type == "directory";
  isRegularType = name: type: type == "regular";
  isSymbolicType = name: type: type == "symlink";
  isDefaultNix = name: type: (libnixpkgs.baseNameOf name) == "default.nix";
  isNixFile = andPred isRegularType (name: type: libnixpkgs.match ".+\\.nix$" name != null);
  isDisabled = name: type: libnixpkgs.match "^DISABLED_.*" name != null;
  hasDefaultNix = andPred isDirectoryType (name: type: libnixpkgs.hasAttr "default.nix" (libnixpkgs.readDir name));

  /*
  Return the basename without .nix extension

  Type:
    baseNameNoNixExt :: String -> String
  */
  baseNameNoNixExt = name: libnixpkgs.removeSuffix ".nix" (libnixpkgs.baseNameOf name);
}
