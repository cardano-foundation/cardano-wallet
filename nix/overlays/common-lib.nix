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

    # Get the index-state and ghc version out of a cabal.project file.
    #
    # TODO [ADP-2445] It would be better to re-use functions from haskell.nix like:
    # https://github.com/input-output-hk/haskell.nix/blob/a30665693a1991341d067df55bbf2bec1124bddd/lib/cabal-project-parser.nix
    cabalProjectIndexState = cabalProject: let
      awkProgram = builtins.replaceStrings ["\n"] [" "] ''
            BEGIN { print "{"; }
            END { print "}"; }
            { if (($1=="index-state:" || $1=="with-compiler:") && $2!="cardano-haskell-packages")
              { gsub(/:/, "", $1); print"  " $1 " = \"" $2 "\";"; }
            }
            '';

      parsed = import (self.runCommandNoCC "cabal-project-index-state.nix" {
        preferLocalBuild = true;
        allowSubstitutes = false;
      } (''
          awk '${awkProgram}' < ${cabalProject} > $out
        '' ));
    in {
      index-state = parsed.index-state or null;
      compiler-nix-name = if parsed ? with-compiler
        then lib.replaceStrings ["-" "."] ["" ""] parsed.with-compiler
        else null;
    };

    # Retrieve the list of local project packages by
    # filtering the set of *all* packages by their homepage.
    projectPackageList = lib.attrNames (lib.filterAttrs
        (_: p: p != null
          && haskellLib.isLocalPackage p.package
          && p.package.homepage == "https://github.com/cardano-foundation/cardano-wallet")
        cardanoWalletHaskellProject.pkg-set.config.packages);

    ############################################################################
    # Debugging tools

    # Recursively traces an attrset as it's evaluated.
    # This is helpful for debugging large attribute sets.
    traceNames = let
      go = prefix: builtins.mapAttrs (n: v:
        if builtins.isAttrs v
          then if v ? type && v.type == "derivation"
            then __trace (prefix + n) v
            else go (prefix + n + ".") v
          else v);
    in
      go;
  };
}
