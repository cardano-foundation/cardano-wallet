{ lib }:

{
  # lib.cleanSourceWith filter function which removes socket files
  # from a source tree. This files can be created by cardano-node and
  # cause errors when nix attempts to copy them into the store.
  removeSocketFilesFilter = _path: type:
    lib.elem type ["regular" "directory" "symlink" ];

  # Convert version strings from Cabal format (YYYY.M.D)
  # to git tag format (vYYYY-MM-DD).
  versionTag = cabalName: let
      versionRegExp = "(^.*)([[:digit:]]{4})\.([[:digit:]]{1,2})\.([[:digit:]]{1,2})(.*$)";
      parts = builtins.match versionRegExp cabalName;
      name = lib.head parts;
      cabalVer = lib.take 3 (lib.drop 1 parts);
      rest = lib.drop 4 parts;
      leading0 = str: if lib.stringLength str == 1 then "0" + str else str;
      tag = "v" + lib.concatMapStringsSep "-" leading0 cabalVer;
    in
      assert lib.assertMsg (parts != null)
        "versionTag: \"${cabalName}\" does not have a version in YYYY.M.D format";
      (name + tag + lib.concatStrings rest);

  # Recursively traces an attrset as it's evaluated.
  # This is helpful to use in the Hydra jobset so that we can more
  # easily locate evaluation problems.
  traceNames = let
    go = prefix: builtins.mapAttrs (n: v:
      if builtins.isAttrs v
        then if v ? type && v.type == "derivation"
          then __trace (prefix + n) v
          else go (prefix + n + ".") v
        else v);
  in
    go;
}
