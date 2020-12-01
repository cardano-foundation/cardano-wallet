{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  ({
    flags = { release = false; };
    package = {
      specVersion = "2.0";
      identifier = { name = "cardano-transactions"; version = "1.0.0"; };
      license = "Apache-2.0";
      copyright = "2020 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "https://github.com/input-output-hk/cardano-transactions#readme";
      url = "";
      synopsis = "Library utilities for constructing and signing Cardano transactions.";
      description = "Please see the README on GitHub at <https://github.com/input-output-hk/cardano-transactions>";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bech32" or (errorHandler.buildDepError "bech32"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-api" or (errorHandler.buildDepError "cardano-api"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-crypto" or (errorHandler.buildDepError "cardano-crypto"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-crypto-wrapper" or (errorHandler.buildDepError "cardano-crypto-wrapper"))
          (hsPkgs."cardano-ledger" or (errorHandler.buildDepError "cardano-ledger"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
          (hsPkgs."ouroboros-consensus-shelley" or (errorHandler.buildDepError "ouroboros-consensus-shelley"))
          (hsPkgs."shelley-spec-ledger" or (errorHandler.buildDepError "shelley-spec-ledger"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
        buildable = true;
        };
      sublibs = {
        "cardano-transactions-shared" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base58-bytestring" or (errorHandler.buildDepError "base58-bytestring"))
            (hsPkgs."bech32" or (errorHandler.buildDepError "bech32"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          };
        };
      exes = {
        "cardano-tx" = {
          depends = [
            (hsPkgs."ansi-terminal" or (errorHandler.buildDepError "ansi-terminal"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-transactions" or (errorHandler.buildDepError "cardano-transactions"))
            (hsPkgs."cardano-transactions-shared" or (errorHandler.buildDepError "cardano-transactions-shared"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          };
        };
      tests = {
        "unit" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-crypto-wrapper" or (errorHandler.buildDepError "cardano-crypto-wrapper"))
            (hsPkgs."cardano-ledger" or (errorHandler.buildDepError "cardano-ledger"))
            (hsPkgs."cardano-ledger-test" or (errorHandler.buildDepError "cardano-ledger-test"))
            (hsPkgs."cardano-transactions" or (errorHandler.buildDepError "cardano-transactions"))
            (hsPkgs."cardano-transactions-shared" or (errorHandler.buildDepError "cardano-transactions-shared"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."hedgehog-quickcheck" or (errorHandler.buildDepError "hedgehog-quickcheck"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.cardano-tx or (pkgs.buildPackages.cardano-tx or (errorHandler.buildToolDepError "cardano-tx")))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-transactions";
      rev = "9fd020c934cd9ff5a0d82613ef8f85a1889f0c6a";
      sha256 = "036f27nhbmasx5whxd8v4d0bhvdhlmig6m7ncr71rig19379nskz";
      });
    }) // { cabal-generator = "hpack"; }