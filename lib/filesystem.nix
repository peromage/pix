{ self, nixpkgs }:

let
  inherit (nixpkgs.lib)
    isFunction
    functionArgs
    intersectAttrs
    attrNames
    toString
    readDir
    baseNameOf
    match
    hasAttr
    removeSuffix;

in with self; {
  /* Simplified version of `nixpkgs.lib.callPackageWith'.
     This function doesn't add override attribute to the result.

     Type:
       autoCall :: AttrSet -> (AttrSet -> a) -> AttrSet -> a
  */
  autoCall = autoArgs: fn: args:
    let
      f = isFunction fn then fn else import fn;
      passedArgs = intersectAttrs (functionArgs f) autoArgs // args;
    in
      f passedArgs;

  /* A generic function that filters all the files/directories under the given
     directory.  Return a list of names prepended with the given directory.

     Type:
       listDir :: (String -> String -> Bool) -> Path -> [String]
  */
  listDir = pred: dir: attrNames
    (filterAttrs pred (mapAttrs'
      (name: type: nameValuePair (toString (dir + "/${name}")) type)
      (readDir dir)));

  /* Predications used for `listDir'. */
  notPred = pred: name: type: ! pred name type;
  andPred = predA: predB: name: type: predA name type && predB name type;
  orPred = predA: predB: name: type: predA name type || predB name type;

  isDirectoryType = name: type: type == "directory";
  isRegularType = name: type: type == "regular";
  isSymbolicType = name: type: type == "symlink";
  isDefaultNix = name: type: (baseNameOf name) == "default.nix";
  isNixFile = andPred isRegularType (name: type: match ".+\\.nix$" name != null);
  isDisabled = name: type: match "^DISABLED_.*" name != null;
  hasDefaultNix = andPred isDirectoryType (name: type: hasAttr "default.nix"  (readDir name));

  /* Return the basename without .nix extension

     Type:
       baseNameNoNixExt :: String -> String
  */
  baseNameNoNixExt = name: removeSuffix ".nix" (baseNameOf name);
}
