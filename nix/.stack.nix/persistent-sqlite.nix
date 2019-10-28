let
  buildDepError = pkg:
    builtins.throw ''
      The Haskell package set does not contain the package: ${pkg} (build dependency).
      
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
  sysDepError = pkg:
    builtins.throw ''
      The Nixpkgs package set does not contain the package: ${pkg} (system dependency).
      
      You may need to augment the system package mapping in haskell.nix so that it can be found.
      '';
  pkgConfDepError = pkg:
    builtins.throw ''
      The pkg-conf packages does not contain the package: ${pkg} (pkg-conf dependency).
      
      You may need to augment the pkg-conf package mapping in haskell.nix so that it can be found.
      '';
  exeDepError = pkg:
    builtins.throw ''
      The local executable components do not include the component: ${pkg} (executable dependency).
      '';
  legacyExeDepError = pkg:
    builtins.throw ''
      The Haskell package set does not contain the package: ${pkg} (executable dependency).
      
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
  buildToolDepError = pkg:
    builtins.throw ''
      Neither the Haskell package set or the Nixpkgs package set contain the package: ${pkg} (build tool dependency).
      
      If this is a system dependency:
      You may need to augment the system package mapping in haskell.nix so that it can be found.
      
      If this is a Haskell dependency:
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
in { system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {
      systemlib = false;
      use-pkgconfig = false;
      build-sanity-exe = false;
      full-text-search = true;
      uri-filenames = true;
      have-usleep = true;
      json1 = true;
      use-stat3 = false;
      use-stat4 = true;
      };
    package = {
      specVersion = "1.10";
      identifier = { name = "persistent-sqlite"; version = "2.10.5"; };
      license = "MIT";
      copyright = "";
      maintainer = "Michael Snoyman <michael@snoyman.com>";
      author = "Michael Snoyman <michael@snoyman.com>";
      homepage = "http://www.yesodweb.com/book/persistent";
      url = "";
      synopsis = "Backend for the persistent library using sqlite3.";
      description = "This package includes a thin sqlite3 wrapper based on the direct-sqlite package, as well as the entire C library, so there are no system dependencies.";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."persistent" or (buildDepError "persistent"))
          (hsPkgs."aeson" or (buildDepError "aeson"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."conduit" or (buildDepError "conduit"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."microlens-th" or (buildDepError "microlens-th"))
          (hsPkgs."monad-logger" or (buildDepError "monad-logger"))
          (hsPkgs."resource-pool" or (buildDepError "resource-pool"))
          (hsPkgs."resourcet" or (buildDepError "resourcet"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."time" or (buildDepError "time"))
          (hsPkgs."transformers" or (buildDepError "transformers"))
          (hsPkgs."unliftio-core" or (buildDepError "unliftio-core"))
          (hsPkgs."unordered-containers" or (buildDepError "unordered-containers"))
          ];
        libs = (pkgs.lib).optionals (flags.systemlib) ((pkgs.lib).optional (!flags.use-pkgconfig) (pkgs."sqlite3" or (sysDepError "sqlite3"))) ++ (pkgs.lib).optional (!system.isWindows) (pkgs."pthread" or (sysDepError "pthread"));
        pkgconfig = (pkgs.lib).optionals (flags.systemlib) ((pkgs.lib).optional (flags.use-pkgconfig) (pkgconfPkgs."sqlite3" or (pkgConfDepError "sqlite3")));
        buildable = true;
        };
      exes = {
        "sanity" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."persistent-sqlite" or (buildDepError "persistent-sqlite"))
            (hsPkgs."monad-logger" or (buildDepError "monad-logger"))
            ];
          buildable = if flags.build-sanity-exe then true else false;
          };
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."persistent" or (buildDepError "persistent"))
            (hsPkgs."persistent-sqlite" or (buildDepError "persistent-sqlite"))
            (hsPkgs."persistent-template" or (buildDepError "persistent-template"))
            (hsPkgs."persistent-test" or (buildDepError "persistent-test"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."exceptions" or (buildDepError "exceptions"))
            (hsPkgs."fast-logger" or (buildDepError "fast-logger"))
            (hsPkgs."hspec" or (buildDepError "hspec"))
            (hsPkgs."HUnit" or (buildDepError "HUnit"))
            (hsPkgs."monad-logger" or (buildDepError "monad-logger"))
            (hsPkgs."QuickCheck" or (buildDepError "QuickCheck"))
            (hsPkgs."resourcet" or (buildDepError "resourcet"))
            (hsPkgs."system-fileio" or (buildDepError "system-fileio"))
            (hsPkgs."system-filepath" or (buildDepError "system-filepath"))
            (hsPkgs."temporary" or (buildDepError "temporary"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."time" or (buildDepError "time"))
            (hsPkgs."transformers" or (buildDepError "transformers"))
            (hsPkgs."time" or (buildDepError "time"))
            (hsPkgs."unliftio-core" or (buildDepError "unliftio-core"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/persistent";
      rev = "79f2ece07eafae005a703c8eda1bd2420b5e07b5";
      sha256 = "081bhdg52wn7vgxsgl4aimy73ccai05j64r24hwkdnjj4kz96lia";
      });
    postUnpack = "sourceRoot+=/persistent-sqlite; echo source root reset to \$sourceRoot";
    }