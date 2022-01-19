self: super: let
  inherit (self) lib cardanoWalletHaskellProject;
  inherit (self.haskell-nix) haskellLib;
in {
  cardanoWalletLib = {

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

    # Get the index-state and ghc version out of a cabal.project file.
    cabalProjectIndexState = cabalProject: let
      parsed = import (self.runCommandNoCC "cabal-project-index-state.nix" {
        preferLocalBuild = true;
        allowSubstitutes = false;
      } ''
        awk 'BEGIN { print "{"; } END { print "}"; } /^(index-state|with-compiler)/ { gsub(/:/, "", $1); print"  " $1 " = \"" $2 "\";"; }' < ${cabalProject} > $out
      '');
    in {
      index-state = parsed.index-state or null;
      compiler-nix-name = if parsed ? with-compiler
        then lib.replaceStrings ["-" "."] ["" ""] parsed.with-compiler
        else null;
    };

    # Since it's not automatically discovered from stack-pkgs yet, we use homepage as discriminant
    # to retrieve local project packages:
    projectPackageList = lib.attrNames (lib.filterAttrs
        (_: p: p != null
          && haskellLib.isLocalPackage p.package
          && p.package.homepage == "https://github.com/input-output-hk/cardano-wallet")
        cardanoWalletHaskellProject.pkg-set.config.packages);
  };
}
