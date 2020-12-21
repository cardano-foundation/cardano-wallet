{ lib }:

{
  # lib.cleanSourceWith filter function which removes socket files
  # from a source tree. This files can be created by cardano-node and
  # cause errors when nix attempts to copy them into the store.
  removeSocketFilesFilter = _path: type:
    lib.elem type ["regular" "directory" "symlink" ];

  # Convert version strings from Cabal format (YYYY.M.D)
  # to git tag format (vYYYY-MM-DD).
  versionTag = cabal: let
      parts = builtins.match "([[:digit:]]{4})\.([[:digit:]]{1,2})\.([[:digit:]]{1,2})" cabal;
      leading0 = str: if lib.stringLength str == 1 then "0" + str else str;
    in
      assert lib.assertMsg (parts != null)
        "versionTag: ${cabal} is not in YYYY.M.D format";
      "v" + lib.concatMapStringsSep "-" leading0 parts;

}
