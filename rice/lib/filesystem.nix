{ self, nixpkgs, ... }:

let
  inherit (nixpkgs.lib) isFunction genAttrs mapAttrsToList filterAttrs elemAt
    pathExists optionalAttrs optional;
  inherit (builtins) readDir baseNameOf match;

in with self; {
  /* Generate an attribute set for supported platforms.
       More values can be checked from `nixpkgs.lib.systems.flakeExposed'.

     Type:
       forSupportedSystems :: (String -> Any) -> AttrSet
  */
  forSupportedSystems = genAttrs [
    "x86_64-linux"
    "x86_64-darwin"
    "aarch64-linux"
    "aarch64-darwin"
  ];

  /* Supported system attribute constant. */
  supportedSystems = forSupportedSystems id;

  /* Check if the given system is in the supported list.

     Type:
       isSupportedSystem :: String -> Bool
  */
  isSupportedSystem = system: hasAttr system supportedSystems;


  /* Import the given path with predefined arguments.

     Type:
       callWithArgs :: AttrSet -> ((AttrSet -> Any) | Path) -> Any
  */
  callWithArgs = args: fn: (if isFunction fn then fn else import fn) args;

  /* Import each module from the list with given argument.

     Type:
       callListWithArgs :: AttrSet -> [(AttrSet -> a) | Path] -> [Any]
  */
  callListWithArgs = args: map (callWithArgs args);

  /* Import paths from the given list.

     Type:
       importList :: [Path] -> [Any]
  */
  importList = map import;

  /* Like `importList' but instead of returning a list this returns an attrset
     with file names as the attributes.

     Type:
       importListAsAttrs :: [Path] -> AttrSet
  */
  importListAsAttrs = list: genAttrs list import;

  /* A generic function that filters all the files/directories under the given
     directory.  Return a list of names prepended with the given directory.

     Type:
       filterDir :: (String -> String -> Bool) -> Path -> [String]
  */
  filterDir = pred: dir: mapAttrsToList
    (n: t: dir + "/${n}")
    (filterAttrs pred (readDir dir));

  /* Predications used for `filterDir'. */
  isDirType = name: type: type == "directory";
  isFileType = name: type: type == "regular";
  isSymbolicType = name: type: type == "symlink";
  isDefaultNix = name: type: name == "default.nix";
  isNixFile = name: type: isNotDirType name type && match ".+\\.nix$" name != null;
  isBaseNameSupportedSystem = name: type: isSupportedSystem (baseNameNoExt name);
  isImportable = name: type: isDirType name type || isNixFile name type;

  isNotDirType = name: type: ! isDirType name type;
  isNotFileType = name: type: ! isFileType name type;
  isNotSymbolicType = name: type: ! isSymbolicType name type;
  isNotDefaultNix = name: type: ! isDefaultNix name type;
  isNotNixFile = name: type: ! isNixFile name type;
  isNotBaseNameSupportSystem = name: type: ! isBaseNameSupportedSystem name type;
  isNotImportable = name: type: ! isImportable name type;

  /* Return the basename without extension.

     Type:
       baseNameNoExt :: String -> String
  */
  baseNameNoExt = name:
    let
      b = baseNameOf name;
      m = match "(.+)(\\.[^.]+)$" b;
    in if null == m then b else elemAt m 0;
}
