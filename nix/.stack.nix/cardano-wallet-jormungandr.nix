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
      identifier = {
        name = "cardano-wallet-jormungandr";
        version = "2019.10.16";
        };
      license = "Apache-2.0";
      copyright = "2019 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK Engineering Team";
      homepage = "https://github.com/input-output-hk/cardano-wallet";
      url = "";
      synopsis = "Wallet backend protocol-specific bits implemented using Jörmungandr";
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
          (hsPkgs."base58-bytestring" or (buildDepError "base58-bytestring"))
          (hsPkgs."bech32" or (buildDepError "bech32"))
          (hsPkgs."binary" or (buildDepError "binary"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."cardano-crypto" or (buildDepError "cardano-crypto"))
          (hsPkgs."cardano-wallet-core" or (buildDepError "cardano-wallet-core"))
          (hsPkgs."cardano-wallet-cli" or (buildDepError "cardano-wallet-cli"))
          (hsPkgs."cardano-wallet-launcher" or (buildDepError "cardano-wallet-launcher"))
          (hsPkgs."cborg" or (buildDepError "cborg"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."cryptonite" or (buildDepError "cryptonite"))
          (hsPkgs."deepseq" or (buildDepError "deepseq"))
          (hsPkgs."directory" or (buildDepError "directory"))
          (hsPkgs."either" or (buildDepError "either"))
          (hsPkgs."exceptions" or (buildDepError "exceptions"))
          (hsPkgs."extra" or (buildDepError "extra"))
          (hsPkgs."filepath" or (buildDepError "filepath"))
          (hsPkgs."fmt" or (buildDepError "fmt"))
          (hsPkgs."http-client" or (buildDepError "http-client"))
          (hsPkgs."http-types" or (buildDepError "http-types"))
          (hsPkgs."iohk-monitoring" or (buildDepError "iohk-monitoring"))
          (hsPkgs."lifted-base" or (buildDepError "lifted-base"))
          (hsPkgs."monad-control" or (buildDepError "monad-control"))
          (hsPkgs."memory" or (buildDepError "memory"))
          (hsPkgs."network" or (buildDepError "network"))
          (hsPkgs."safe" or (buildDepError "safe"))
          (hsPkgs."servant" or (buildDepError "servant"))
          (hsPkgs."servant-client" or (buildDepError "servant-client"))
          (hsPkgs."servant-client-core" or (buildDepError "servant-client-core"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."text-class" or (buildDepError "text-class"))
          (hsPkgs."time" or (buildDepError "time"))
          (hsPkgs."transformers" or (buildDepError "transformers"))
          (hsPkgs."yaml" or (buildDepError "yaml"))
          (hsPkgs."warp" or (buildDepError "warp"))
          ];
        buildable = true;
        };
      exes = {
        "cardano-wallet-jormungandr" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."cardano-wallet-cli" or (buildDepError "cardano-wallet-cli"))
            (hsPkgs."cardano-wallet-core" or (buildDepError "cardano-wallet-core"))
            (hsPkgs."cardano-wallet-jormungandr" or (buildDepError "cardano-wallet-jormungandr"))
            (hsPkgs."cardano-wallet-launcher" or (buildDepError "cardano-wallet-launcher"))
            (hsPkgs."filepath" or (buildDepError "filepath"))
            (hsPkgs."iohk-monitoring" or (buildDepError "iohk-monitoring"))
            (hsPkgs."network" or (buildDepError "network"))
            (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
            (hsPkgs."process" or (buildDepError "process"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."text-class" or (buildDepError "text-class"))
            ];
          buildable = true;
          };
        };
      tests = {
        "unit" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."aeson" or (buildDepError "aeson"))
            (hsPkgs."aeson-qq" or (buildDepError "aeson-qq"))
            (hsPkgs."binary" or (buildDepError "binary"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."cardano-crypto" or (buildDepError "cardano-crypto"))
            (hsPkgs."cardano-wallet-core" or (buildDepError "cardano-wallet-core"))
            (hsPkgs."cardano-wallet-jormungandr" or (buildDepError "cardano-wallet-jormungandr"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."directory" or (buildDepError "directory"))
            (hsPkgs."fmt" or (buildDepError "fmt"))
            (hsPkgs."generic-arbitrary" or (buildDepError "generic-arbitrary"))
            (hsPkgs."generic-lens" or (buildDepError "generic-lens"))
            (hsPkgs."hspec" or (buildDepError "hspec"))
            (hsPkgs."lifted-base" or (buildDepError "lifted-base"))
            (hsPkgs."memory" or (buildDepError "memory"))
            (hsPkgs."monad-control" or (buildDepError "monad-control"))
            (hsPkgs."QuickCheck" or (buildDepError "QuickCheck"))
            (hsPkgs."safe" or (buildDepError "safe"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."text-class" or (buildDepError "text-class"))
            (hsPkgs."time" or (buildDepError "time"))
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
            (hsPkgs."QuickCheck" or (buildDepError "QuickCheck"))
            (hsPkgs."aeson" or (buildDepError "aeson"))
            (hsPkgs."aeson-qq" or (buildDepError "aeson-qq"))
            (hsPkgs."async" or (buildDepError "async"))
            (hsPkgs."binary" or (buildDepError "binary"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."cardano-wallet-cli" or (buildDepError "cardano-wallet-cli"))
            (hsPkgs."cardano-wallet-core" or (buildDepError "cardano-wallet-core"))
            (hsPkgs."cardano-wallet-core-integration" or (buildDepError "cardano-wallet-core-integration"))
            (hsPkgs."cardano-wallet-jormungandr" or (buildDepError "cardano-wallet-jormungandr"))
            (hsPkgs."cardano-wallet-launcher" or (buildDepError "cardano-wallet-launcher"))
            (hsPkgs."cardano-wallet-test-utils" or (buildDepError "cardano-wallet-test-utils"))
            (hsPkgs."command" or (buildDepError "command"))
            (hsPkgs."containers" or (buildDepError "containers"))
            (hsPkgs."cryptonite" or (buildDepError "cryptonite"))
            (hsPkgs."deepseq" or (buildDepError "deepseq"))
            (hsPkgs."directory" or (buildDepError "directory"))
            (hsPkgs."exceptions" or (buildDepError "exceptions"))
            (hsPkgs."extra" or (buildDepError "extra"))
            (hsPkgs."filepath" or (buildDepError "filepath"))
            (hsPkgs."generic-arbitrary" or (buildDepError "generic-arbitrary"))
            (hsPkgs."generic-lens" or (buildDepError "generic-lens"))
            (hsPkgs."hspec" or (buildDepError "hspec"))
            (hsPkgs."hspec-expectations-lifted" or (buildDepError "hspec-expectations-lifted"))
            (hsPkgs."http-api-data" or (buildDepError "http-api-data"))
            (hsPkgs."http-client" or (buildDepError "http-client"))
            (hsPkgs."http-types" or (buildDepError "http-types"))
            (hsPkgs."iohk-monitoring" or (buildDepError "iohk-monitoring"))
            (hsPkgs."lifted-base" or (buildDepError "lifted-base"))
            (hsPkgs."memory" or (buildDepError "memory"))
            (hsPkgs."monad-control" or (buildDepError "monad-control"))
            (hsPkgs."network" or (buildDepError "network"))
            (hsPkgs."persistent" or (buildDepError "persistent"))
            (hsPkgs."process" or (buildDepError "process"))
            (hsPkgs."retry" or (buildDepError "retry"))
            (hsPkgs."safe" or (buildDepError "safe"))
            (hsPkgs."servant" or (buildDepError "servant"))
            (hsPkgs."template-haskell" or (buildDepError "template-haskell"))
            (hsPkgs."temporary" or (buildDepError "temporary"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."text-class" or (buildDepError "text-class"))
            (hsPkgs."time" or (buildDepError "time"))
            (hsPkgs."transformers" or (buildDepError "transformers"))
            (hsPkgs."warp" or (buildDepError "warp"))
            (hsPkgs."yaml" or (buildDepError "yaml"))
            ];
          buildable = true;
          build-tools = [
            (hsPkgs.buildPackages.nodejs or (pkgs.buildPackages.nodejs))
            (hsPkgs.buildPackages.cardano-wallet-jormungandr or (pkgs.buildPackages.cardano-wallet-jormungandr))
            ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././lib/jormungandr; }