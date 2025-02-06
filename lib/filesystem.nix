{ self, nixpkgs }:

let
  lib = nixpkgs.lib;

in with self; {
  /*
     Simplified version of `nixpkgs.lib.callPackageWith'.
     This function doesn't add override attribute to the result.

     Type:
       autoCall :: AttrSet -> (AttrSet -> a) -> AttrSet -> a
  */
  autoCall = autoArgs: fn: args:
    let
      f = if lib.isFunction fn then fn else import fn;
      passedArgs = lib.intersectAttrs (lib.functionArgs f) autoArgs // args;
    in
      f passedArgs;

  /*
     A generic function that filters all the files/directories under the given
     directory.  Return a list of names prepended with the given directory.

     Type:
       listDir :: (String -> String -> Bool) -> Path -> [String]
  */
  listDir = pred: dir: lib.attrNames
    (lib.filterAttrs pred (lib.mapAttrs'
      (name: type: lib.nameValuePair (lib.toString (dir + "/${name}")) type)
      (lib.readDir dir)));

  /*
     Predications used for `listDir'.
  */
  notPred = pred: name: type: ! pred name type;
  andPred = predA: predB: name: type: predA name type && predB name type;
  orPred = predA: predB: name: type: predA name type || predB name type;

  isDirectoryType = name: type: type == "directory";
  isRegularType = name: type: type == "regular";
  isSymbolicType = name: type: type == "symlink";
  isDefaultNix = name: type: (lib.baseNameOf name) == "default.nix";
  isNixFile = andPred isRegularType (name: type: lib.match ".+\\.nix$" name != null);
  isDisabled = name: type: lib.match "^DISABLED_.*" name != null;
  hasDefaultNix = andPred isDirectoryType (name: type: lib.hasAttr "default.nix"  (lib.readDir name));

  /*
     Return the basename without .nix extension

     Type:
       baseNameNoNixExt :: String -> String
  */
  baseNameNoNixExt = name: lib.removeSuffix ".nix" (lib.baseNameOf name);
}
