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
    flags = {};
    package = {
      specVersion = "2.2";
      identifier = { name = "cardano-shell"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2018 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "https://github.com/input-output-hk/cardano-shell#readme";
      url = "";
      synopsis = "";
      description = "Please see the README on GitHub at <https://github.com/githubuser/cardano-shell#readme>";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (buildDepError "aeson"))
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."binary" or (buildDepError "binary"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."Cabal" or (buildDepError "Cabal"))
          (hsPkgs."cardano-prelude" or (buildDepError "cardano-prelude"))
          (hsPkgs."concurrency" or (buildDepError "concurrency"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."formatting" or (buildDepError "formatting"))
          (hsPkgs."process" or (buildDepError "process"))
          (hsPkgs."QuickCheck" or (buildDepError "QuickCheck"))
          (hsPkgs."safe-exceptions" or (buildDepError "safe-exceptions"))
          (hsPkgs."async" or (buildDepError "async"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."transformers" or (buildDepError "transformers"))
          ] ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."Win32" or (buildDepError "Win32"));
        buildable = true;
        };
      exes = {
        "node-ipc" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."cardano-shell" or (buildDepError "cardano-shell"))
            (hsPkgs."cardano-prelude" or (buildDepError "cardano-prelude"))
            ];
          buildable = true;
          };
        };
      tests = {
        "cardano-shell-test" = {
          depends = [
            (hsPkgs."aeson" or (buildDepError "aeson"))
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."cardano-shell" or (buildDepError "cardano-shell"))
            (hsPkgs."cardano-prelude" or (buildDepError "cardano-prelude"))
            (hsPkgs."QuickCheck" or (buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-state-machine" or (buildDepError "quickcheck-state-machine"))
            (hsPkgs."tree-diff" or (buildDepError "tree-diff"))
            (hsPkgs."hspec" or (buildDepError "hspec"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-shell";
      rev = "bc3563c952d9f3635e1c76749b86b0a24f7e4b83";
      sha256 = "0c4f394h0wshcly6vcghvg5m6zd1i7s3a9kb1xlg7na4hn4y4a2v";
      });
    postUnpack = "sourceRoot+=/cardano-shell; echo source root reset to \$sourceRoot";
    }