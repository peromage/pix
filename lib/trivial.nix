self: let
  lib = (self.getInputs "").nixpkgs.lib;
in {
  /*
  Join a list of strings/paths with separaters.

  Type:
    join :: String -> [Any] -> String
  */
  join = sep: list: lib.foldl (a: i: a + "${sep}${i}") (lib.head list) (lib.tail list);

  /*
  Apply a list of arguments to the function.

  Type:
    apply :: (Any -> Any) -> [Any] -> Any
  */
  apply = lib.foldl (f: x: f x);

  /*
  Filter the return value of the original function.

  Note that n (the number of arguments) must be greater than 0 since a
  function should at least have one argument.  This is required because for
  curried functions the number of arguments can not be known beforehand.  The
  caller must tell this function where to end.

  Type:
    filterReturn :: (Any -> ... -> Any) -> Number -> (Any -> Any) -> Any
  */
  filterReturn = f: narg: filter: let
    virtualFilter = f: narg: arg:
      if narg == 1
      then filter (f arg)
      else virtualFilter (f arg) (narg - 1);
  in
    assert narg > 0; virtualFilter f narg;

  /*
  Filter the arguments of the original function.

  Note that n (the number of arguments) must be greater than 0 since a
  function should at least have one argument.  This is required because for
  curried functions the number of arguments can not be known beforehand.  The
  caller must tell the wrapper function where to end.

  The wrapper function must have the same signature of the original function
  and return a list of altered arguments.

  Type:
    filterArgs :: (Any -> ... -> Any) -> Number -> (Any -> ... -> [Any]) -> Any
  */
  filterArgs = f: narg: filter: let
    virtualFilter = filter: narg: arg:
      if narg == 1
      then self.apply f (filter arg)
      else virtualFilter (filter arg) (narg - 1);
  in
    assert narg > 0; virtualFilter filter narg;

  /*
  Fix point and override pattern.
  See: http://r6.ca/blog/20140422T142911Z.html
  See also: `lib.makeExtensible'.  Better use `lib.makeExtensible' instead of
  this as this may encounter infinite recursion since it doesn't provide
  access to prev (only final).
  */
  fixOverridable = f: let
    x = f x;
  in
    x
    // {
      fixOverride = g:
        self.fixOverridable (self:
          f self
          // (
            if lib.isFunction g
            then g self
            else g
          ));
    };

  /*
  Apply predicate `f' on each attribute and return true if at least one is true.
  Otherwise, return false.

  Type:
    anyAttrs :: (String -> a -> Bool) -> AttrSet -> Bool
  */
  anyAttrs = f: attrs: lib.any (name: f name attrs.${name}) (lib.attrNames attrs);
}
