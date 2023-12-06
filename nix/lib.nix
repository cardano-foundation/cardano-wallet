# Library of Nix functions used for cardano-wallet

# nixpkgs.lib  library to import from.
lib
:

rec {
  # Imports from nixpkgs.lib
  inherit (lib) filterAttrsRecursive recursiveUpdate collect 
                optionalAttrs mapAttrs isDerivation;

  /* Convert versions string from Cabal (YYYY.M.D)
     to git tag format (vYYYY-MM-DD).
  */
  gitTagFromCabalVersion =
    # Version number in cabal format as string (YYYY.M.D)
    cabalName :
    let
      versionRegExp =
        "(^.*)([[:digit:]]{4})\.([[:digit:]]{1,2})\.([[:digit:]]{1,2})(.*$)";
      parts = builtins.match versionRegExp cabalName;
      name = lib.head parts;
      cabalVer = lib.take 3 (lib.drop 1 parts);
      rest = lib.drop 4 parts;
      leading0 = str: if lib.stringLength str == 1 then "0" + str else str;
      tag = "v" + lib.concatMapStringsSep "-" leading0 cabalVer;
    in
      assert lib.assertMsg (parts != null)
          "gitTagFromCabalVersion: \"${cabalName}\" does not have a version in YYYY.M.D format";
      (name + tag + lib.concatStrings rest);

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
