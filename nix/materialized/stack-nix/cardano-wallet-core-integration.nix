{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = { release = false; };
    package = {
      specVersion = "1.10";
      identifier = {
        name = "cardano-wallet-core-integration";
        version = "2022.1.18";
        };
      license = "Apache-2.0";
      copyright = "2018-2020 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK Engineering Team";
      homepage = "https://github.com/input-output-hk/cardano-wallet";
      url = "";
      synopsis = "Core integration test library.";
      description = "Shared core functionality for our integration test suites.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."aeson-qq" or (errorHandler.buildDepError "aeson-qq"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."base58-bytestring" or (errorHandler.buildDepError "base58-bytestring"))
          (hsPkgs."bech32" or (errorHandler.buildDepError "bech32"))
          (hsPkgs."bech32-th" or (errorHandler.buildDepError "bech32-th"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-api" or (errorHandler.buildDepError "cardano-api"))
          (hsPkgs."cardano-addresses" or (errorHandler.buildDepError "cardano-addresses"))
          (hsPkgs."cardano-api" or (errorHandler.buildDepError "cardano-api"))
          (hsPkgs."cardano-crypto" or (errorHandler.buildDepError "cardano-crypto"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
          (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
          (hsPkgs."cardano-wallet-cli" or (errorHandler.buildDepError "cardano-wallet-cli"))
          (hsPkgs."cardano-wallet-core" or (errorHandler.buildDepError "cardano-wallet-core"))
          (hsPkgs."cardano-wallet-launcher" or (errorHandler.buildDepError "cardano-wallet-launcher"))
          (hsPkgs."cardano-wallet-test-utils" or (errorHandler.buildDepError "cardano-wallet-test-utils"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."command" or (errorHandler.buildDepError "command"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."criterion-measurement" or (errorHandler.buildDepError "criterion-measurement"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."either" or (errorHandler.buildDepError "either"))
          (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
          (hsPkgs."fmt" or (errorHandler.buildDepError "fmt"))
          (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
          (hsPkgs."generic-lens-core" or (errorHandler.buildDepError "generic-lens-core"))
          (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
          (hsPkgs."hspec-expectations-lifted" or (errorHandler.buildDepError "hspec-expectations-lifted"))
          (hsPkgs."http-api-data" or (errorHandler.buildDepError "http-api-data"))
          (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
          (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
          (hsPkgs."iohk-monitoring" or (errorHandler.buildDepError "iohk-monitoring"))
          (hsPkgs."lens-aeson" or (errorHandler.buildDepError "lens-aeson"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
          (hsPkgs."microstache" or (errorHandler.buildDepError "microstache"))
          (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
          (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
          (hsPkgs."retry" or (errorHandler.buildDepError "retry"))
          (hsPkgs."say" or (errorHandler.buildDepError "say"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."string-interpolate" or (errorHandler.buildDepError "string-interpolate"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."text-class" or (errorHandler.buildDepError "text-class"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
          (hsPkgs."unliftio-core" or (errorHandler.buildDepError "unliftio-core"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          ];
        buildable = true;
        modules = [
          "Test/Integration/Faucet"
          "Test/Integration/Framework/Context"
          "Test/Integration/Framework/DSL"
          "Test/Integration/Framework/Request"
          "Test/Integration/Framework/TestData"
          "Test/Integration/Plutus"
          "Test/Integration/Scenario/API/Byron/Wallets"
          "Test/Integration/Scenario/API/Byron/HWWallets"
          "Test/Integration/Scenario/API/Byron/Addresses"
          "Test/Integration/Scenario/API/Byron/CoinSelections"
          "Test/Integration/Scenario/API/Byron/Transactions"
          "Test/Integration/Scenario/API/Byron/TransactionsNew"
          "Test/Integration/Scenario/API/Byron/Migrations"
          "Test/Integration/Scenario/API/Byron/Network"
          "Test/Integration/Scenario/API/Shelley/Addresses"
          "Test/Integration/Scenario/API/Shelley/CoinSelections"
          "Test/Integration/Scenario/API/Shelley/HWWallets"
          "Test/Integration/Scenario/API/Shelley/Network"
          "Test/Integration/Scenario/API/Shelley/Settings"
          "Test/Integration/Scenario/API/Shelley/StakePools"
          "Test/Integration/Scenario/API/Shelley/Transactions"
          "Test/Integration/Scenario/API/Shelley/TransactionsNew"
          "Test/Integration/Scenario/API/Shelley/Migrations"
          "Test/Integration/Scenario/API/Shelley/Wallets"
          "Test/Integration/Scenario/API/Shared/Wallets"
          "Test/Integration/Scenario/API/Shared/Addresses"
          "Test/Integration/Scenario/API/Network"
          "Test/Integration/Scenario/CLI/Byron/Wallets"
          "Test/Integration/Scenario/CLI/Byron/Addresses"
          "Test/Integration/Scenario/CLI/Shelley/Addresses"
          "Test/Integration/Scenario/CLI/Shelley/HWWallets"
          "Test/Integration/Scenario/CLI/Shelley/Transactions"
          "Test/Integration/Scenario/CLI/Shelley/Wallets"
          "Test/Integration/Scenario/CLI/Miscellaneous"
          "Test/Integration/Scenario/CLI/Network"
          "Test/Integration/Scenario/CLI/Port"
          "Cardano/Wallet/LatencyBenchShared"
          "Cardano/Wallet/BenchShared"
          ];
        hsSourceDirs = [ "src" ];
        };
      };
    } // rec {
    src = (pkgs.lib).mkDefault ./lib/core-integration;
    }