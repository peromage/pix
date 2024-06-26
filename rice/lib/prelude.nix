{ self, nixpkgs, ... }:

let
  inherit (nixpkgs.lib) isFunction genAttrs mapAttrsToList filterAttrs elemAt
    pathExists optional mapAttrs' nameValuePair id;
  inherit (builtins) readDir baseNameOf match hasAttr toString;

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
       callAllWithArgs :: AttrSet -> [(AttrSet -> Any) | Path] -> [Any]
  */
  callAllWithArgs = args: map (callWithArgs args);

  /* Returns an attrset with file names as the attributes and imported content
     as the values.

     Type:
       importAll :: [String] -> AttrSet
  */
  importAll = list: genAttrs list import;

  /* Similar with `importAll' but transforms the attribute names with the given
     function.
     Note that if there are duplicated results of attribute names, only the first
     one takes effect.

     Type:
       importAllNameMapped :: (String -> String) -> [String] -> AttrSet
  */
  importAllNameMapped = func: list: mapAttrs'
    (n: v: nameValuePair (func n) v)
    (importAll list);

  /* A generic function that filters all the files/directories under the given
     directory.  Return a list of names prepended with the given directory.

     Type:
       listDir :: (String -> String -> Bool) -> Path -> [String]
  */
  listDir = pred: dir: mapAttrsToList
    (n: t: toString (dir + "/${n}"))
    (filterAttrs pred (readDir dir));

  /* Predications used for `listDir'. */
  isDirType = name: type: type == "directory";
  isFileType = name: type: type == "regular";
  isSymbolicType = name: type: type == "symlink";
  isDefaultNix = name: type: name == "default.nix";
  isNixFile = name: type: isNotDirType name type && match ".+\\.nix$" name != null;
  isImportable = name: type: isDirType name type || isNixFile name type;
  isSupportedSystemDir = name: type: isSupportedSystem name && isDirType name type;
  isDisabled = name: type: builtins.match "^DISABLED-.*" name != null;

  isNotDirType = name: type: ! isDirType name type;
  isNotFileType = name: type: ! isFileType name type;
  isNotSymbolicType = name: type: ! isSymbolicType name type;
  isNotDefaultNix = name: type: ! isDefaultNix name type;
  isNotNixFile = name: type: ! isNixFile name type;
  isNotImportable = name: type: ! isImportable name type;
  isNotSupportedSystemDir = name: type: ! isSupportedSystemDir name type;
  isNotDisabled = name: type: ! isDisabled name type;

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
