# Library of Nix functions used for cardano-wallet

# nixpkgs.lib  library to import from.
lib
:

rec {
  # Imports from nixpkgs.lib
  inherit (lib) filterAttrsRecursive recursiveUpdate collect 
                optionalAttrs mapAttrs isDerivation;

  /* Recursively set all attributes whose path satisfies
     a given condition to the empty set {}.
  */
  setEmptyAttrsWithCondition =
    # cond :: [string] -> bool
    cond:
    lib.mapAttrsRecursiveCond
      (value: !(lib.isDerivation value)) # do not modify attributes of derivations
      (path: value: if cond path then {} else value);

  /* Keep all attributes whose path contains a name starting
     with "integration".
  */
  keepIntegrationChecks =
    setEmptyAttrsWithCondition
      (path: !lib.any (lib.hasPrefix "integration") path);

  /* Keep all attributes whose path contain a name that is "unit" or "test".
  */
  keepUnitChecks =
    setEmptyAttrsWithCondition
      (path: !lib.any (name: name == "unit" || name == "test") path);

  /* Recursively remove all attributes named `recurseForDerivations`.
  */
  removeRecurse =
    lib.filterAttrsRecursive (n: _: n != "recurseForDerivations");

  /* Recursively traces an attrset as it's evaluated.
     This is helpful for debugging large attribute sets.
  */
  traceNames =
    let
      go = prefix: builtins.mapAttrs (n: v:
        if builtins.isAttrs v
          then if v ? type && v.type == "derivation"
            then __trace (prefix + n) v
            else go (prefix + n + ".") v
          else v);
    in
      go;
}
