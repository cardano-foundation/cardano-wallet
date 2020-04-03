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
      identifier = { name = "cardano-wallet"; version = "2020.4.1"; };
      license = "Apache-2.0";
      copyright = "2020 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK Engineering Team";
      homepage = "https://github.com/input-output-hk/cardano-wallet";
      url = "";
      synopsis = "Wallet backend protocol-specific bits implemented using byron nodes";
      description = "Please see README.md";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."aeson" or (buildDepError "aeson"))
          (hsPkgs."async" or (buildDepError "async"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."cardano-binary" or (buildDepError "cardano-binary"))
          (hsPkgs."cardano-crypto" or (buildDepError "cardano-crypto"))
          (hsPkgs."cardano-crypto-wrapper" or (buildDepError "cardano-crypto-wrapper"))
          (hsPkgs."cardano-ledger" or (buildDepError "cardano-ledger"))
          (hsPkgs."cardano-wallet-core" or (buildDepError "cardano-wallet-core"))
          (hsPkgs."cardano-wallet-launcher" or (buildDepError "cardano-wallet-launcher"))
          (hsPkgs."cborg" or (buildDepError "cborg"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."contra-tracer" or (buildDepError "contra-tracer"))
          (hsPkgs."cryptonite" or (buildDepError "cryptonite"))
          (hsPkgs."deepseq" or (buildDepError "deepseq"))
          (hsPkgs."directory" or (buildDepError "directory"))
          (hsPkgs."either" or (buildDepError "either"))
          (hsPkgs."exceptions" or (buildDepError "exceptions"))
          (hsPkgs."filepath" or (buildDepError "filepath"))
          (hsPkgs."fmt" or (buildDepError "fmt"))
          (hsPkgs."io-sim-classes" or (buildDepError "io-sim-classes"))
          (hsPkgs."iohk-monitoring" or (buildDepError "iohk-monitoring"))
          (hsPkgs."memory" or (buildDepError "memory"))
          (hsPkgs."network" or (buildDepError "network"))
          (hsPkgs."network-mux" or (buildDepError "network-mux"))
          (hsPkgs."ouroboros-consensus" or (buildDepError "ouroboros-consensus"))
          (hsPkgs."ouroboros-consensus-byron" or (buildDepError "ouroboros-consensus-byron"))
          (hsPkgs."ouroboros-network" or (buildDepError "ouroboros-network"))
          (hsPkgs."ouroboros-network-framework" or (buildDepError "ouroboros-network-framework"))
          (hsPkgs."process" or (buildDepError "process"))
          (hsPkgs."retry" or (buildDepError "retry"))
          (hsPkgs."temporary" or (buildDepError "temporary"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."text-class" or (buildDepError "text-class"))
          (hsPkgs."time" or (buildDepError "time"))
          (hsPkgs."transformers" or (buildDepError "transformers"))
          (hsPkgs."typed-protocols-examples" or (buildDepError "typed-protocols-examples"))
          (hsPkgs."unordered-containers" or (buildDepError "unordered-containers"))
          (hsPkgs."warp" or (buildDepError "warp"))
          (hsPkgs."yaml" or (buildDepError "yaml"))
          (hsPkgs."Win32-network" or (buildDepError "Win32-network"))
          ];
        buildable = true;
        };
      exes = {
        "cardano-wallet" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."cardano-ledger" or (buildDepError "cardano-ledger"))
            (hsPkgs."cardano-wallet" or (buildDepError "cardano-wallet"))
            (hsPkgs."cardano-wallet-cli" or (buildDepError "cardano-wallet-cli"))
            (hsPkgs."cardano-wallet-core" or (buildDepError "cardano-wallet-core"))
            (hsPkgs."cardano-wallet-launcher" or (buildDepError "cardano-wallet-launcher"))
            (hsPkgs."contra-tracer" or (buildDepError "contra-tracer"))
            (hsPkgs."iohk-monitoring" or (buildDepError "iohk-monitoring"))
            (hsPkgs."network" or (buildDepError "network"))
            (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."text-class" or (buildDepError "text-class"))
            (hsPkgs."transformers" or (buildDepError "transformers"))
            ];
          buildable = true;
          };
        };
      tests = {
        "unit" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."cardano-crypto" or (buildDepError "cardano-crypto"))
            (hsPkgs."cardano-wallet-core" or (buildDepError "cardano-wallet-core"))
            (hsPkgs."cardano-wallet" or (buildDepError "cardano-wallet"))
            (hsPkgs."cborg" or (buildDepError "cborg"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."hspec" or (buildDepError "hspec"))
            (hsPkgs."fmt" or (buildDepError "fmt"))
            (hsPkgs."memory" or (buildDepError "memory"))
            (hsPkgs."QuickCheck" or (buildDepError "QuickCheck"))
            (hsPkgs."transformers" or (buildDepError "transformers"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover or (pkgs.buildPackages.hspec-discover or (buildToolDepError "hspec-discover")))
            ];
          buildable = true;
          };
        "integration" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."aeson" or (buildDepError "aeson"))
            (hsPkgs."async" or (buildDepError "async"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."cardano-wallet" or (buildDepError "cardano-wallet"))
            (hsPkgs."cardano-wallet-cli" or (buildDepError "cardano-wallet-cli"))
            (hsPkgs."cardano-wallet-core" or (buildDepError "cardano-wallet-core"))
            (hsPkgs."cardano-wallet-core-integration" or (buildDepError "cardano-wallet-core-integration"))
            (hsPkgs."cardano-wallet-launcher" or (buildDepError "cardano-wallet-launcher"))
            (hsPkgs."cardano-wallet-test-utils" or (buildDepError "cardano-wallet-test-utils"))
            (hsPkgs."command" or (buildDepError "command"))
            (hsPkgs."generic-lens" or (buildDepError "generic-lens"))
            (hsPkgs."hspec" or (buildDepError "hspec"))
            (hsPkgs."http-client" or (buildDepError "http-client"))
            (hsPkgs."http-types" or (buildDepError "http-types"))
            (hsPkgs."iohk-monitoring" or (buildDepError "iohk-monitoring"))
            (hsPkgs."temporary" or (buildDepError "temporary"))
            (hsPkgs."text" or (buildDepError "text"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.cardano-wallet or (pkgs.buildPackages.cardano-wallet or (buildToolDepError "cardano-wallet")))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "restore" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."cardano-wallet-core" or (buildDepError "cardano-wallet-core"))
            (hsPkgs."cardano-wallet" or (buildDepError "cardano-wallet"))
            (hsPkgs."cardano-wallet-launcher" or (buildDepError "cardano-wallet-launcher"))
            (hsPkgs."ouroboros-network" or (buildDepError "ouroboros-network"))
            (hsPkgs."contra-tracer" or (buildDepError "contra-tracer"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."criterion-measurement" or (buildDepError "criterion-measurement"))
            (hsPkgs."cryptonite" or (buildDepError "cryptonite"))
            (hsPkgs."deepseq" or (buildDepError "deepseq"))
            (hsPkgs."digest" or (buildDepError "digest"))
            (hsPkgs."filepath" or (buildDepError "filepath"))
            (hsPkgs."fmt" or (buildDepError "fmt"))
            (hsPkgs."iohk-monitoring" or (buildDepError "iohk-monitoring"))
            (hsPkgs."persistent" or (buildDepError "persistent"))
            (hsPkgs."persistent-template" or (buildDepError "persistent-template"))
            (hsPkgs."process" or (buildDepError "process"))
            (hsPkgs."say" or (buildDepError "say"))
            (hsPkgs."temporary" or (buildDepError "temporary"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."time" or (buildDepError "time"))
            (hsPkgs."transformers" or (buildDepError "transformers"))
            ];
          buildable = true;
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././lib/byron; }