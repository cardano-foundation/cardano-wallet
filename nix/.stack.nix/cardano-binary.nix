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
    flags = { development = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "cardano-binary"; version = "1.5.0"; };
      license = "Apache-2.0";
      copyright = "2019 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Binary serialization for Cardano";
      description = "This package includes the binary serialization format for Cardano";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."aeson" or (buildDepError "aeson"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."cardano-prelude" or (buildDepError "cardano-prelude"))
          (hsPkgs."cborg" or (buildDepError "cborg"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."digest" or (buildDepError "digest"))
          (hsPkgs."formatting" or (buildDepError "formatting"))
          (hsPkgs."recursion-schemes" or (buildDepError "recursion-schemes"))
          (hsPkgs."safe-exceptions" or (buildDepError "safe-exceptions"))
          (hsPkgs."tagged" or (buildDepError "tagged"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."time" or (buildDepError "time"))
          (hsPkgs."vector" or (buildDepError "vector"))
          ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."cardano-binary" or (buildDepError "cardano-binary"))
            (hsPkgs."cardano-prelude" or (buildDepError "cardano-prelude"))
            (hsPkgs."cardano-prelude-test" or (buildDepError "cardano-prelude-test"))
            (hsPkgs."cborg" or (buildDepError "cborg"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."formatting" or (buildDepError "formatting"))
            (hsPkgs."hedgehog" or (buildDepError "hedgehog"))
            (hsPkgs."hspec" or (buildDepError "hspec"))
            (hsPkgs."pretty-show" or (buildDepError "pretty-show"))
            (hsPkgs."QuickCheck" or (buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (buildDepError "quickcheck-instances"))
            (hsPkgs."tagged" or (buildDepError "tagged"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."vector" or (buildDepError "vector"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-base";
      rev = "f869bee9b08ba1044b1476737c9d65083e1c6c7f";
      sha256 = "0df3bdf13cwx3hd8n4q53g9hybb0w8mh837y64ydd88xhdfaf6a3";
      });
    postUnpack = "sourceRoot+=/binary; echo source root reset to \$sourceRoot";
    }