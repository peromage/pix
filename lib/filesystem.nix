{ self, nixpkgs }:

let
  lib = nixpkgs.lib;

in with self; {
  /* A generic function that filters all the files/directories under the given
     directory.  Return a list of names prepended with the given directory.

     Type:
       listDir :: (String -> String -> Bool) -> Path -> [String]
  */
  listDir = pred: dir: with lib; builtins.attrNames
    (filterAttrs pred (mapAttrs'
      (name: type: nameValuePair (builtins.toString (dir + "/${name}")) type)
      (builtins.readDir dir)));

  /* Predications used for `listDir'. */
  notPred = pred: name: type: ! pred name type;
  andPred = predA: predB: name: type: predA name type && predB name type;
  orPred = predA: predB: name: type: predA name type || predB name type;

  isDirectoryType = name: type: type == "directory";
  isRegularType = name: type: type == "regular";
  isSymbolicType = name: type: type == "symlink";
  isDefaultNix = name: type: (builtins.baseNameOf name) == "default.nix";
  isNixFile = andPred isRegularType (name: type: builtins.match ".+\\.nix$" name != null);
  isDisabled = name: type: builtins.match "^DISABLED_.*" name != null;
  hasDefaultNix = andPred isDirectoryType (name: type: with builtins; hasAttr "default.nix"  (readDir name));

  /* Return the basename without .nix extension

     Type:
       baseNameNoNixExt :: String -> String
  */
  baseNameNoNixExt = name: with lib; removeSuffix ".nix" (baseNameOf name);
}
