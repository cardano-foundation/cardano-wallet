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
      identifier = { name = "cardano-wallet"; version = "2022.1.18"; };
      license = "Apache-2.0";
      copyright = "2020 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK Engineering Team";
      homepage = "https://github.com/input-output-hk/cardano-wallet";
      url = "";
      synopsis = "Wallet backend protocol-specific bits implemented using Shelley nodes";
      description = "Please see README.md";
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
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."base58-bytestring" or (errorHandler.buildDepError "base58-bytestring"))
          (hsPkgs."bech32" or (errorHandler.buildDepError "bech32"))
          (hsPkgs."bech32-th" or (errorHandler.buildDepError "bech32-th"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-addresses" or (errorHandler.buildDepError "cardano-addresses"))
          (hsPkgs."cardano-api" or (errorHandler.buildDepError "cardano-api"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-crypto" or (errorHandler.buildDepError "cardano-crypto"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-crypto-wrapper" or (errorHandler.buildDepError "cardano-crypto-wrapper"))
          (hsPkgs."cardano-wallet-launcher" or (errorHandler.buildDepError "cardano-wallet-launcher"))
          (hsPkgs."cardano-ledger-byron" or (errorHandler.buildDepError "cardano-ledger-byron"))
          (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
          (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
          (hsPkgs."cardano-ledger-shelley-ma" or (errorHandler.buildDepError "cardano-ledger-shelley-ma"))
          (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."cardano-wallet-cli" or (errorHandler.buildDepError "cardano-wallet-cli"))
          (hsPkgs."cardano-wallet-core" or (errorHandler.buildDepError "cardano-wallet-core"))
          (hsPkgs."cardano-wallet-launcher" or (errorHandler.buildDepError "cardano-wallet-launcher"))
          (hsPkgs."cardano-wallet-test-utils" or (errorHandler.buildDepError "cardano-wallet-test-utils"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."fmt" or (errorHandler.buildDepError "fmt"))
          (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
          (hsPkgs."int-cast" or (errorHandler.buildDepError "int-cast"))
          (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          (hsPkgs."iohk-monitoring" or (errorHandler.buildDepError "iohk-monitoring"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."network-mux" or (errorHandler.buildDepError "network-mux"))
          (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
          (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
          (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
          (hsPkgs."ouroboros-consensus-byron" or (errorHandler.buildDepError "ouroboros-consensus-byron"))
          (hsPkgs."ouroboros-consensus-cardano" or (errorHandler.buildDepError "ouroboros-consensus-cardano"))
          (hsPkgs."ouroboros-consensus-shelley" or (errorHandler.buildDepError "ouroboros-consensus-shelley"))
          (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
          (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."retry" or (errorHandler.buildDepError "retry"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
          (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
          (hsPkgs."strict-non-empty-containers" or (errorHandler.buildDepError "strict-non-empty-containers"))
          (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."text-class" or (errorHandler.buildDepError "text-class"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."typed-process" or (errorHandler.buildDepError "typed-process"))
          (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
          (hsPkgs."unliftio-core" or (errorHandler.buildDepError "unliftio-core"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
          (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
          (hsPkgs."Win32-network" or (errorHandler.buildDepError "Win32-network"))
          ];
        buildable = true;
        modules = [
          "Cardano/Wallet/Byron/Compatibility"
          "Cardano/Wallet/Shelley"
          "Cardano/Wallet/Shelley/Api/Server"
          "Cardano/Wallet/Shelley/Compatibility"
          "Cardano/Wallet/Shelley/Compatibility/Ledger"
          "Cardano/Wallet/Shelley/Network"
          "Cardano/Wallet/Shelley/Transaction"
          "Cardano/Wallet/Shelley/Launch"
          "Cardano/Wallet/Shelley/Launch/Cluster"
          "Cardano/Wallet/Shelley/Pools"
          ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "cardano-wallet" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-wallet-cli" or (errorHandler.buildDepError "cardano-wallet-cli"))
            (hsPkgs."cardano-wallet-core" or (errorHandler.buildDepError "cardano-wallet-core"))
            (hsPkgs."cardano-wallet-launcher" or (errorHandler.buildDepError "cardano-wallet-launcher"))
            (hsPkgs."cardano-wallet" or (errorHandler.buildDepError "cardano-wallet"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."iohk-monitoring" or (errorHandler.buildDepError "iohk-monitoring"))
            (hsPkgs."lobemo-backend-ekg" or (errorHandler.buildDepError "lobemo-backend-ekg"))
            (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."text-class" or (errorHandler.buildDepError "text-class"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
            ];
          buildable = true;
          hsSourceDirs = [ "exe" ];
          mainPath = [
            "cardano-wallet.hs"
            ] ++ (pkgs.lib).optional (flags.release) "";
          };
        "local-cluster" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-wallet-cli" or (errorHandler.buildDepError "cardano-wallet-cli"))
            (hsPkgs."cardano-wallet-core" or (errorHandler.buildDepError "cardano-wallet-core"))
            (hsPkgs."cardano-wallet-core-integration" or (errorHandler.buildDepError "cardano-wallet-core-integration"))
            (hsPkgs."cardano-wallet-launcher" or (errorHandler.buildDepError "cardano-wallet-launcher"))
            (hsPkgs."cardano-wallet" or (errorHandler.buildDepError "cardano-wallet"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."iohk-monitoring" or (errorHandler.buildDepError "iohk-monitoring"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."lobemo-backend-ekg" or (errorHandler.buildDepError "lobemo-backend-ekg"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."text-class" or (errorHandler.buildDepError "text-class"))
            ];
          buildable = true;
          hsSourceDirs = [ "exe" ];
          mainPath = [
            "local-cluster.hs"
            ] ++ (pkgs.lib).optional (flags.release) "";
          };
        "mock-token-metadata-server" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."ansi-wl-pprint" or (errorHandler.buildDepError "ansi-wl-pprint"))
            (hsPkgs."cardano-wallet-core" or (errorHandler.buildDepError "cardano-wallet-core"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."wai-extra" or (errorHandler.buildDepError "wai-extra"))
            ];
          buildable = true;
          hsSourceDirs = [ "exe" ];
          mainPath = [
            "mock-token-metadata-server.hs"
            ] ++ (pkgs.lib).optional (flags.release) "";
          };
        };
      tests = {
        "unit" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base58-bytestring" or (errorHandler.buildDepError "base58-bytestring"))
            (hsPkgs."bech32" or (errorHandler.buildDepError "bech32"))
            (hsPkgs."bech32-th" or (errorHandler.buildDepError "bech32-th"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-addresses" or (errorHandler.buildDepError "cardano-addresses"))
            (hsPkgs."cardano-api" or (errorHandler.buildDepError "cardano-api"))
            (hsPkgs."cardano-crypto" or (errorHandler.buildDepError "cardano-crypto"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-crypto-wrapper" or (errorHandler.buildDepError "cardano-crypto-wrapper"))
            (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
            (hsPkgs."cardano-ledger-byron" or (errorHandler.buildDepError "cardano-ledger-byron"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-wallet" or (errorHandler.buildDepError "cardano-wallet"))
            (hsPkgs."cardano-wallet-core" or (errorHandler.buildDepError "cardano-wallet-core"))
            (hsPkgs."cardano-wallet-launcher" or (errorHandler.buildDepError "cardano-wallet-launcher"))
            (hsPkgs."cardano-wallet-test-utils" or (errorHandler.buildDepError "cardano-wallet-test-utils"))
            (hsPkgs."cardano-ledger-shelley-ma" or (errorHandler.buildDepError "cardano-ledger-shelley-ma"))
            (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
            (hsPkgs."pretty-simple" or (errorHandler.buildDepError "pretty-simple"))
            (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."fmt" or (errorHandler.buildDepError "fmt"))
            (hsPkgs."generic-arbitrary" or (errorHandler.buildDepError "generic-arbitrary"))
            (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
            (hsPkgs."hspec-core" or (errorHandler.buildDepError "hspec-core"))
            (hsPkgs."hspec-golden" or (errorHandler.buildDepError "hspec-golden"))
            (hsPkgs."iohk-monitoring" or (errorHandler.buildDepError "iohk-monitoring"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."hspec-core" or (errorHandler.buildDepError "hspec-core"))
            (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
            (hsPkgs."MonadRandom" or (errorHandler.buildDepError "MonadRandom"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."ouroboros-consensus-shelley" or (errorHandler.buildDepError "ouroboros-consensus-shelley"))
            (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
            (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."text-class" or (errorHandler.buildDepError "text-class"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover.components.exes.hspec-discover or (pkgs.buildPackages.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
            ];
          buildable = true;
          modules = [
            "Cardano/Wallet/Shelley/CompatibilitySpec"
            "Cardano/Wallet/Shelley/Compatibility/LedgerSpec"
            "Cardano/Wallet/Shelley/LaunchSpec"
            "Cardano/Wallet/Shelley/NetworkSpec"
            "Cardano/Wallet/Shelley/TransactionSpec"
            "Spec"
            ];
          hsSourceDirs = [ "test/unit" "test/data" ];
          mainPath = [ "shelley-unit-test.hs" ];
          };
        "integration" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cardano-wallet-cli" or (errorHandler.buildDepError "cardano-wallet-cli"))
            (hsPkgs."cardano-wallet-core" or (errorHandler.buildDepError "cardano-wallet-core"))
            (hsPkgs."cardano-wallet-core-integration" or (errorHandler.buildDepError "cardano-wallet-core-integration"))
            (hsPkgs."cardano-wallet-launcher" or (errorHandler.buildDepError "cardano-wallet-launcher"))
            (hsPkgs."cardano-wallet" or (errorHandler.buildDepError "cardano-wallet"))
            (hsPkgs."cardano-wallet-test-utils" or (errorHandler.buildDepError "cardano-wallet-test-utils"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."either" or (errorHandler.buildDepError "either"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."hspec-core" or (errorHandler.buildDepError "hspec-core"))
            (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
            (hsPkgs."iohk-monitoring" or (errorHandler.buildDepError "iohk-monitoring"))
            (hsPkgs."lobemo-backend-ekg" or (errorHandler.buildDepError "lobemo-backend-ekg"))
            (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."text-class" or (errorHandler.buildDepError "text-class"))
            (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.cardano-wallet.components.exes.cardano-wallet or (pkgs.buildPackages.cardano-wallet or (errorHandler.buildToolDepError "cardano-wallet:cardano-wallet")))
            ];
          buildable = true;
          modules = [ "Cardano/Wallet/Shelley/Faucet" ];
          hsSourceDirs = [ "test/integration" "test/data" ];
          mainPath = [ "shelley-integration-test.hs" ];
          };
        };
      benchmarks = {
        "restore" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-addresses" or (errorHandler.buildDepError "cardano-addresses"))
            (hsPkgs."cardano-wallet-core" or (errorHandler.buildDepError "cardano-wallet-core"))
            (hsPkgs."cardano-wallet-core-integration" or (errorHandler.buildDepError "cardano-wallet-core-integration"))
            (hsPkgs."cardano-wallet" or (errorHandler.buildDepError "cardano-wallet"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."fmt" or (errorHandler.buildDepError "fmt"))
            (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
            (hsPkgs."iohk-monitoring" or (errorHandler.buildDepError "iohk-monitoring"))
            (hsPkgs."say" or (errorHandler.buildDepError "say"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."text-class" or (errorHandler.buildDepError "text-class"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
            ];
          buildable = true;
          hsSourceDirs = [ "bench" ];
          };
        "latency" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."cardano-wallet-cli" or (errorHandler.buildDepError "cardano-wallet-cli"))
            (hsPkgs."cardano-wallet-core" or (errorHandler.buildDepError "cardano-wallet-core"))
            (hsPkgs."cardano-wallet-core-integration" or (errorHandler.buildDepError "cardano-wallet-core-integration"))
            (hsPkgs."cardano-wallet" or (errorHandler.buildDepError "cardano-wallet"))
            (hsPkgs."cardano-wallet-launcher" or (errorHandler.buildDepError "cardano-wallet-launcher"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
            (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
            (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."iohk-monitoring" or (errorHandler.buildDepError "iohk-monitoring"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
            ];
          buildable = true;
          modules = [ "Cardano/Wallet/Shelley/Faucet" ];
          hsSourceDirs = [ "bench" "test/integration" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ./lib/shelley; }