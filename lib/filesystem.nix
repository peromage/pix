{ self, libnix }:

with self; {
  /*
     Simplified version of `libnix.callPackageWith'.
     This function doesn't add override attribute to the result.

     Type:
       autoCall :: AttrSet -> (AttrSet -> a) -> AttrSet -> a
  */
  autoCall = autoArgs: fn: args:
    let
      f = if libnix.isFunction fn then fn else import fn;
      passedArgs = libnix.intersectAttrs (libnix.functionArgs f) autoArgs // args;
    in
      f passedArgs;

  /*
     A generic function that filters all the files/directories under the given
     directory.  Return a list of names prepended with the given directory.

     Type:
       listDir :: (String -> String -> Bool) -> Path -> [String]
  */
  listDir = pred: dir: libnix.attrNames
    (libnix.filterAttrs pred (libnix.mapAttrs'
      (name: type: libnix.nameValuePair (libnix.toString (dir + "/${name}")) type)
      (libnix.readDir dir)));

  /*
     Predications used for `listDir'.
  */
  notPred = pred: name: type: ! pred name type;
  andPred = predA: predB: name: type: predA name type && predB name type;
  orPred = predA: predB: name: type: predA name type || predB name type;

  isDirectoryType = name: type: type == "directory";
  isRegularType = name: type: type == "regular";
  isSymbolicType = name: type: type == "symlink";
  isDefaultNix = name: type: (libnix.baseNameOf name) == "default.nix";
  isNixFile = andPred isRegularType (name: type: libnix.match ".+\\.nix$" name != null);
  isDisabled = name: type: libnix.match "^DISABLED_.*" name != null;
  hasDefaultNix = andPred isDirectoryType (name: type: libnix.hasAttr "default.nix"  (libnix.readDir name));

  /*
     Return the basename without .nix extension

     Type:
       baseNameNoNixExt :: String -> String
  */
  baseNameNoNixExt = name: libnix.removeSuffix ".nix" (libnix.baseNameOf name);
}
