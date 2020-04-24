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
    flags = { asserts = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "ouroboros-consensus-byron"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2019 Input Output (Hong Kong) Ltd.";
      maintainer = "operations@iohk.io";
      author = "IOHK Engineering Team";
      homepage = "";
      url = "";
      synopsis = "Byron ledger integration in the Ouroboros consensus layer";
      description = "";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."binary" or (buildDepError "binary"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."cardano-binary" or (buildDepError "cardano-binary"))
          (hsPkgs."cardano-crypto-class" or (buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-crypto-wrapper" or (buildDepError "cardano-crypto-wrapper"))
          (hsPkgs."cardano-ledger" or (buildDepError "cardano-ledger"))
          (hsPkgs."cardano-prelude" or (buildDepError "cardano-prelude"))
          (hsPkgs."cardano-slotting" or (buildDepError "cardano-slotting"))
          (hsPkgs."cborg" or (buildDepError "cborg"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."cryptonite" or (buildDepError "cryptonite"))
          (hsPkgs."formatting" or (buildDepError "formatting"))
          (hsPkgs."memory" or (buildDepError "memory"))
          (hsPkgs."mtl" or (buildDepError "mtl"))
          (hsPkgs."serialise" or (buildDepError "serialise"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."ouroboros-network" or (buildDepError "ouroboros-network"))
          (hsPkgs."ouroboros-consensus" or (buildDepError "ouroboros-consensus"))
          ];
        buildable = true;
        };
      exes = {
        "db-converter" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."cardano-binary" or (buildDepError "cardano-binary"))
            (hsPkgs."cardano-crypto-wrapper" or (buildDepError "cardano-crypto-wrapper"))
            (hsPkgs."cardano-ledger" or (buildDepError "cardano-ledger"))
            (hsPkgs."cardano-slotting" or (buildDepError "cardano-slotting"))
            (hsPkgs."contra-tracer" or (buildDepError "contra-tracer"))
            (hsPkgs."directory" or (buildDepError "directory"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
            (hsPkgs."optparse-generic" or (buildDepError "optparse-generic"))
            (hsPkgs."path" or (buildDepError "path"))
            (hsPkgs."path-io" or (buildDepError "path-io"))
            (hsPkgs."resourcet" or (buildDepError "resourcet"))
            (hsPkgs."streaming" or (buildDepError "streaming"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."time" or (buildDepError "time"))
            (hsPkgs."ouroboros-consensus" or (buildDepError "ouroboros-consensus"))
            (hsPkgs."ouroboros-consensus-byron" or (buildDepError "ouroboros-consensus-byron"))
            ];
          buildable = true;
          };
        "db-analyser" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."cardano-binary" or (buildDepError "cardano-binary"))
            (hsPkgs."cardano-crypto-wrapper" or (buildDepError "cardano-crypto-wrapper"))
            (hsPkgs."cardano-ledger" or (buildDepError "cardano-ledger"))
            (hsPkgs."cardano-slotting" or (buildDepError "cardano-slotting"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
            (hsPkgs."ouroboros-consensus" or (buildDepError "ouroboros-consensus"))
            (hsPkgs."ouroboros-consensus-byron" or (buildDepError "ouroboros-consensus-byron"))
            (hsPkgs."ouroboros-network" or (buildDepError "ouroboros-network"))
            ];
          buildable = true;
          };
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."binary" or (buildDepError "binary"))
            (hsPkgs."binary-search" or (buildDepError "binary-search"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."cardano-binary" or (buildDepError "cardano-binary"))
            (hsPkgs."cardano-crypto-class" or (buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-crypto-test" or (buildDepError "cardano-crypto-test"))
            (hsPkgs."cardano-crypto-wrapper" or (buildDepError "cardano-crypto-wrapper"))
            (hsPkgs."cardano-ledger" or (buildDepError "cardano-ledger"))
            (hsPkgs."cardano-ledger-test" or (buildDepError "cardano-ledger-test"))
            (hsPkgs."cardano-prelude" or (buildDepError "cardano-prelude"))
            (hsPkgs."cardano-slotting" or (buildDepError "cardano-slotting"))
            (hsPkgs."cborg" or (buildDepError "cborg"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."cryptonite" or (buildDepError "cryptonite"))
            (hsPkgs."hedgehog-quickcheck" or (buildDepError "hedgehog-quickcheck"))
            (hsPkgs."mtl" or (buildDepError "mtl"))
            (hsPkgs."QuickCheck" or (buildDepError "QuickCheck"))
            (hsPkgs."tasty" or (buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (buildDepError "tasty-quickcheck"))
            (hsPkgs."time" or (buildDepError "time"))
            (hsPkgs."transformers" or (buildDepError "transformers"))
            (hsPkgs."ouroboros-network" or (buildDepError "ouroboros-network"))
            (hsPkgs."ouroboros-consensus" or (buildDepError "ouroboros-consensus"))
            (hsPkgs."ouroboros-consensus-test-infra" or (buildDepError "ouroboros-consensus-test-infra"))
            (hsPkgs."ouroboros-consensus-byron" or (buildDepError "ouroboros-consensus-byron"))
            (hsPkgs."ouroboros-consensus-byronspec" or (buildDepError "ouroboros-consensus-byronspec"))
            (hsPkgs."byron-spec-chain" or (buildDepError "byron-spec-chain"))
            (hsPkgs."byron-spec-ledger" or (buildDepError "byron-spec-ledger"))
            (hsPkgs."small-steps" or (buildDepError "small-steps"))
            (hsPkgs."hedgehog" or (buildDepError "hedgehog"))
            (hsPkgs."serialise" or (buildDepError "serialise"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/ouroboros-network";
      rev = "ec4f3af58c13a4c20116e9e262b048fb2d27bdc3";
      sha256 = "1p4z6br0vyvkd84f3a9ilb7sg7d6pgg3fx2aijlxx9x3v1lb1426";
      });
    postUnpack = "sourceRoot+=/ouroboros-consensus-byron; echo source root reset to \$sourceRoot";
    }