{ self, nixpkgs, ... }:

let
  lib = nixpkgs.lib;

in with self; {
  /* Join a list of strings/paths with separaters.

     Type:
       joinStrs :: String -> [Any] -> String
  */
  join = sep: list: with lib; foldl' (a: i: a + "${sep}${i}") (head list) (tail list);

  /* Apply a list of arguments to the function.

     Type:
       apply :: (Any -> Any) -> [Any] -> Any
  */
  apply = lib.foldl' (f: x: f x);

  /* Filter the return value of the original function.

     Note that n (the number of arguments) must be greater than 0 since a
     function should at least have one argument.  This is required because for
     curried functions the number of arguments can not be known beforehand.  The
     caller must tell this function where to end.

     Type:
       filterReturn :: (Any -> ... -> Any) -> Number -> (Any -> Any) -> Any
  */
  filterReturn = f: narg: filter:
    let virtualFilter = f: narg: arg:
      if narg == 1
      then filter (f arg)
      else virtualFilter (f arg) (narg - 1);
    in assert narg > 0; virtualFilter f narg;

  /* Filter the arguments of the original function.

     Note that n (the number of arguments) must be greater than 0 since a
     function should at least have one argument.  This is required because for
     curried functions the number of arguments can not be known beforehand.  The
     caller must tell the wrapper function where to end.

     The wrapper function must have the same signature of the original function
     and return a list of altered arguments.

     Type:
       filterArgs :: (Any -> ... -> Any) -> Number -> (Any -> ... -> [Any]) -> Any
  */
  filterArgs = f: narg: filter:
    let virtualFilter = filter: narg: arg:
      if narg == 1
      then apply f (filter arg)
      else virtualFilter (filter arg) (narg - 1);
    in assert narg > 0; virtualFilter filter narg;

  /* Fix point and override pattern.
     See: http://r6.ca/blog/20140422T142911Z.html
  */
  fixOverride = f: let x = f x; in x // {
    override = overrides: fixOverride (self: f self // (
      if builtins.isFunction overrides
      then overrides self
      else overrides
    ));
  };
}
