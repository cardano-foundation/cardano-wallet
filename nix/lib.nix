# Library of Nix functions used for cardano-wallet

# nixpkgs.lib  library to import from.
lib
:

{
  # Imports from nixpkgs.lib
  inherit (lib) filterAttrsRecursive recursiveUpdate collect 
                optionalAttrs mapAttrs isDerivation;

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
