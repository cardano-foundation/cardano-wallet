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
    flags = { release = false; scrypt = true; };
    package = {
      specVersion = "1.10";
      identifier = { name = "cardano-wallet-core"; version = "2022.4.27"; };
      license = "Apache-2.0";
      copyright = "2018-2020 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK Engineering Team";
      homepage = "https://github.com/input-output-hk/cardano-wallet";
      url = "";
      synopsis = "The Wallet Backend for a Cardano node.";
      description = "Please see README.md";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "specifications/api/swagger.yaml" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."bech32" or (errorHandler.buildDepError "bech32"))
          (hsPkgs."bech32-th" or (errorHandler.buildDepError "bech32-th"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-addresses" or (errorHandler.buildDepError "cardano-addresses"))
          (hsPkgs."cardano-api" or (errorHandler.buildDepError "cardano-api"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-crypto" or (errorHandler.buildDepError "cardano-crypto"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-crypto-test" or (errorHandler.buildDepError "cardano-crypto-test"))
          (hsPkgs."cardano-numeric" or (errorHandler.buildDepError "cardano-numeric"))
          (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
          (hsPkgs."cardano-ledger-byron-test" or (errorHandler.buildDepError "cardano-ledger-byron-test"))
          (hsPkgs."cardano-ledger-shelley-test" or (errorHandler.buildDepError "cardano-ledger-shelley-test"))
          (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
          (hsPkgs."cardano-ledger-alonzo" or (errorHandler.buildDepError "cardano-ledger-alonzo"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
          (hsPkgs."data-interval" or (errorHandler.buildDepError "data-interval"))
          (hsPkgs."dbvar" or (errorHandler.buildDepError "dbvar"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."digest" or (errorHandler.buildDepError "digest"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."either" or (errorHandler.buildDepError "either"))
          (hsPkgs."errors" or (errorHandler.buildDepError "errors"))
          (hsPkgs."exact-combinatorics" or (errorHandler.buildDepError "exact-combinatorics"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
          (hsPkgs."fast-logger" or (errorHandler.buildDepError "fast-logger"))
          (hsPkgs."file-embed" or (errorHandler.buildDepError "file-embed"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."fmt" or (errorHandler.buildDepError "fmt"))
          (hsPkgs."foldl" or (errorHandler.buildDepError "foldl"))
          (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
          (hsPkgs."generic-arbitrary" or (errorHandler.buildDepError "generic-arbitrary"))
          (hsPkgs."generics-sop" or (errorHandler.buildDepError "generics-sop"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
          (hsPkgs."hedgehog-quickcheck" or (errorHandler.buildDepError "hedgehog-quickcheck"))
          (hsPkgs."http-api-data" or (errorHandler.buildDepError "http-api-data"))
          (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
          (hsPkgs."http-client-tls" or (errorHandler.buildDepError "http-client-tls"))
          (hsPkgs."http-media" or (errorHandler.buildDepError "http-media"))
          (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
          (hsPkgs."int-cast" or (errorHandler.buildDepError "int-cast"))
          (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          (hsPkgs."iohk-monitoring" or (errorHandler.buildDepError "iohk-monitoring"))
          (hsPkgs."lattices" or (errorHandler.buildDepError "lattices"))
          (hsPkgs."math-functions" or (errorHandler.buildDepError "math-functions"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
          (hsPkgs."MonadRandom" or (errorHandler.buildDepError "MonadRandom"))
          (hsPkgs."monad-logger" or (errorHandler.buildDepError "monad-logger"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."ntp-client" or (errorHandler.buildDepError "ntp-client"))
          (hsPkgs."OddWord" or (errorHandler.buildDepError "OddWord"))
          (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
          (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
          (hsPkgs."path-pieces" or (errorHandler.buildDepError "path-pieces"))
          (hsPkgs."persistent" or (errorHandler.buildDepError "persistent"))
          (hsPkgs."persistent-sqlite" or (errorHandler.buildDepError "persistent-sqlite"))
          (hsPkgs."persistent-template" or (errorHandler.buildDepError "persistent-template"))
          (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
          (hsPkgs."pretty-simple" or (errorHandler.buildDepError "pretty-simple"))
          (hsPkgs."profunctors" or (errorHandler.buildDepError "profunctors"))
          (hsPkgs."quiet" or (errorHandler.buildDepError "quiet"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."random-shuffle" or (errorHandler.buildDepError "random-shuffle"))
          (hsPkgs."resource-pool" or (errorHandler.buildDepError "resource-pool"))
          (hsPkgs."retry" or (errorHandler.buildDepError "retry"))
          (hsPkgs."safe" or (errorHandler.buildDepError "safe"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
          (hsPkgs."servant-client" or (errorHandler.buildDepError "servant-client"))
          (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
          (hsPkgs."split" or (errorHandler.buildDepError "split"))
          (hsPkgs."splitmix" or (errorHandler.buildDepError "splitmix"))
          (hsPkgs."statistics" or (errorHandler.buildDepError "statistics"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."streaming-commons" or (errorHandler.buildDepError "streaming-commons"))
          (hsPkgs."strict-containers" or (errorHandler.buildDepError "strict-containers"))
          (hsPkgs."strict-non-empty-containers" or (errorHandler.buildDepError "strict-non-empty-containers"))
          (hsPkgs."strict-stm" or (errorHandler.buildDepError "strict-stm"))
          (hsPkgs."string-interpolate" or (errorHandler.buildDepError "string-interpolate"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."text-class" or (errorHandler.buildDepError "text-class"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."tls" or (errorHandler.buildDepError "tls"))
          (hsPkgs."tracer-transformers" or (errorHandler.buildDepError "tracer-transformers"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
          (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
          (hsPkgs."unliftio-core" or (errorHandler.buildDepError "unliftio-core"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
          (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
          (hsPkgs."warp-tls" or (errorHandler.buildDepError "warp-tls"))
          (hsPkgs."x509" or (errorHandler.buildDepError "x509"))
          (hsPkgs."x509-store" or (errorHandler.buildDepError "x509-store"))
          (hsPkgs."x509-validation" or (errorHandler.buildDepError "x509-validation"))
          (hsPkgs."Win32-network" or (errorHandler.buildDepError "Win32-network"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."cardano-wallet-test-utils" or (errorHandler.buildDepError "cardano-wallet-test-utils"))
          ] ++ (pkgs.lib).optional (flags.scrypt) (hsPkgs."scrypt" or (errorHandler.buildDepError "scrypt"));
        buildable = true;
        modules = [
          "Paths_cardano_wallet_core"
          "Cardano/Api/Extra"
          "Cardano/Byron/Codec/Cbor"
          "Cardano/DB/Sqlite"
          "Cardano/DB/Sqlite/Delete"
          "Cardano/Pool/DB"
          "Cardano/Pool/DB/Log"
          "Cardano/Pool/DB/MVar"
          "Cardano/Pool/DB/Model"
          "Cardano/Pool/DB/Sqlite"
          "Cardano/Pool/DB/Sqlite/TH"
          "Cardano/Pool/Metadata"
          "Cardano/Pool/Rank"
          "Cardano/Pool/Rank/Likelihood"
          "Cardano/Wallet"
          "Cardano/Wallet/Address/Pool"
          "Cardano/Wallet/Api"
          "Cardano/Wallet/Api/Client"
          "Cardano/Wallet/Api/Link"
          "Cardano/Wallet/Api/Server"
          "Cardano/Wallet/Api/Server/Tls"
          "Cardano/Wallet/Api/Types"
          "Cardano/Wallet/CoinSelection"
          "Cardano/Wallet/CoinSelection/Internal"
          "Cardano/Wallet/CoinSelection/Internal/Balance"
          "Cardano/Wallet/CoinSelection/Internal/Collateral"
          "Cardano/Wallet/CoinSelection/Internal/Context"
          "Cardano/Wallet/Compat"
          "Cardano/Wallet/DB"
          "Cardano/Wallet/DB/Checkpoints"
          "Cardano/Wallet/DB/MVar"
          "Cardano/Wallet/DB/Model"
          "Cardano/Wallet/DB/Sqlite"
          "Cardano/Wallet/DB/Sqlite/AddressBook"
          "Cardano/Wallet/DB/Sqlite/CheckpointsOld"
          "Cardano/Wallet/DB/Sqlite/Migration"
          "Cardano/Wallet/DB/Sqlite/Stores"
          "Cardano/Wallet/DB/Sqlite/TH"
          "Cardano/Wallet/DB/Sqlite/Types"
          "Cardano/Wallet/DB/WalletState"
          "Cardano/Wallet/Logging"
          "Cardano/Wallet/Network"
          "Cardano/Wallet/Network/Light"
          "Cardano/Wallet/Network/Ports"
          "Cardano/Wallet/Orphans"
          "Cardano/Wallet/TokenMetadata"
          "Cardano/Wallet/Primitive/AddressDerivation"
          "Cardano/Wallet/Primitive/AddressDerivation/Byron"
          "Cardano/Wallet/Primitive/AddressDerivation/Icarus"
          "Cardano/Wallet/Primitive/AddressDerivation/MintBurn"
          "Cardano/Wallet/Primitive/AddressDerivation/Shared"
          "Cardano/Wallet/Primitive/AddressDerivation/SharedKey"
          "Cardano/Wallet/Primitive/AddressDerivation/Shelley"
          "Cardano/Wallet/Primitive/AddressDiscovery"
          "Cardano/Wallet/Primitive/AddressDiscovery/Random"
          "Cardano/Wallet/Primitive/AddressDiscovery/Sequential"
          "Cardano/Wallet/Primitive/AddressDiscovery/Shared"
          "Cardano/Wallet/Primitive/BlockSummary"
          "Cardano/Wallet/Primitive/Collateral"
          "Cardano/Wallet/Primitive/Delegation/State"
          "Cardano/Wallet/Primitive/Delegation/UTxO"
          "Cardano/Wallet/Primitive/Migration"
          "Cardano/Wallet/Primitive/Migration/Planning"
          "Cardano/Wallet/Primitive/Migration/Selection"
          "Cardano/Wallet/Primitive/Model"
          "Cardano/Wallet/Primitive/Slotting"
          "Cardano/Wallet/Primitive/SyncProgress"
          "Cardano/Wallet/Primitive/Passphrase"
          "Cardano/Wallet/Primitive/Passphrase/Current"
          "Cardano/Wallet/Primitive/Passphrase/Gen"
          "Cardano/Wallet/Primitive/Passphrase/Legacy"
          "Cardano/Wallet/Primitive/Passphrase/Types"
          "Cardano/Wallet/Primitive/Types"
          "Cardano/Wallet/Primitive/Types/Address"
          "Cardano/Wallet/Primitive/Types/Coin"
          "Cardano/Wallet/Primitive/Types/Hash"
          "Cardano/Wallet/Primitive/Types/Redeemer"
          "Cardano/Wallet/Primitive/Types/RewardAccount"
          "Cardano/Wallet/Primitive/Types/TokenBundle"
          "Cardano/Wallet/Primitive/Types/TokenMap"
          "Cardano/Wallet/Primitive/Types/TokenPolicy"
          "Cardano/Wallet/Primitive/Types/TokenQuantity"
          "Cardano/Wallet/Primitive/Types/Tx"
          "Cardano/Wallet/Primitive/Types/UTxO"
          "Cardano/Wallet/Primitive/Types/UTxOIndex"
          "Cardano/Wallet/Primitive/Types/UTxOIndex/Internal"
          "Cardano/Wallet/Primitive/Types/UTxOSelection"
          "Cardano/Wallet/Registry"
          "Cardano/Wallet/TokenMetadata/MockServer"
          "Cardano/Wallet/Transaction"
          "Cardano/Wallet/Unsafe"
          "Cardano/Wallet/Util"
          "Cardano/Wallet/Version"
          "Cardano/Wallet/Version/TH"
          "Control/Concurrent/Concierge"
          "Control/Monad/Random/Extra"
          "Crypto/Hash/Utils"
          "Data/Aeson/Extra"
          "Data/Function/Utils"
          "Data/Time/Text"
          "Data/Time/Utils"
          "Data/Quantity"
          "Data/Vector/Shuffle"
          "Network/Ntp"
          "Network/Wai/Middleware/ServerError"
          "Network/Wai/Middleware/Logging"
          "Ouroboros/Network/Client/Wallet"
          "UnliftIO/Compat"
          "Cardano/Wallet/CoinSelection/Gen"
          "Cardano/Wallet/CoinSelection/Internal/Balance/Gen"
          "Cardano/Wallet/Primitive/Types/Address/Gen"
          "Cardano/Wallet/Primitive/Types/Coin/Gen"
          "Cardano/Wallet/Primitive/Types/RewardAccount/Gen"
          "Cardano/Wallet/Primitive/Types/TokenBundle/Gen"
          "Cardano/Wallet/Primitive/Types/TokenMap/Gen"
          "Cardano/Wallet/Primitive/Types/TokenPolicy/Gen"
          "Cardano/Wallet/Primitive/Types/TokenQuantity/Gen"
          "Cardano/Wallet/Primitive/Types/Tx/Gen"
          "Cardano/Wallet/Primitive/Types/UTxO/Gen"
          "Cardano/Wallet/Primitive/Types/UTxOIndex/Gen"
          "Cardano/Wallet/Primitive/Types/UTxOSelection/Gen"
          "Cardano/Wallet/Gen"
          "Cardano/Api/Gen"
          ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "unit" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."aeson-qq" or (errorHandler.buildDepError "aeson-qq"))
            (hsPkgs."base58-bytestring" or (errorHandler.buildDepError "base58-bytestring"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-addresses" or (errorHandler.buildDepError "cardano-addresses"))
            (hsPkgs."cardano-api" or (errorHandler.buildDepError "cardano-api"))
            (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
            (hsPkgs."cardano-crypto" or (errorHandler.buildDepError "cardano-crypto"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."cardano-numeric" or (errorHandler.buildDepError "cardano-numeric"))
            (hsPkgs."cardano-ledger-byron" or (errorHandler.buildDepError "cardano-ledger-byron"))
            (hsPkgs."cardano-ledger-byron-test" or (errorHandler.buildDepError "cardano-ledger-byron-test"))
            (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
            (hsPkgs."cardano-ledger-shelley" or (errorHandler.buildDepError "cardano-ledger-shelley"))
            (hsPkgs."cardano-ledger-shelley-test" or (errorHandler.buildDepError "cardano-ledger-shelley-test"))
            (hsPkgs."cardano-wallet-core" or (errorHandler.buildDepError "cardano-wallet-core"))
            (hsPkgs."cardano-wallet-launcher" or (errorHandler.buildDepError "cardano-wallet-launcher"))
            (hsPkgs."cardano-wallet-test-utils" or (errorHandler.buildDepError "cardano-wallet-test-utils"))
            (hsPkgs."cardano-sl-x509" or (errorHandler.buildDepError "cardano-sl-x509"))
            (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."connection" or (errorHandler.buildDepError "connection"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
            (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
            (hsPkgs."data-interval" or (errorHandler.buildDepError "data-interval"))
            (hsPkgs."dbvar" or (errorHandler.buildDepError "dbvar"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."file-embed" or (errorHandler.buildDepError "file-embed"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."fmt" or (errorHandler.buildDepError "fmt"))
            (hsPkgs."foldl" or (errorHandler.buildDepError "foldl"))
            (hsPkgs."generic-arbitrary" or (errorHandler.buildDepError "generic-arbitrary"))
            (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
            (hsPkgs."generics-sop" or (errorHandler.buildDepError "generics-sop"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."hedgehog-quickcheck" or (errorHandler.buildDepError "hedgehog-quickcheck"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."hspec-core" or (errorHandler.buildDepError "hspec-core"))
            (hsPkgs."hspec-hedgehog" or (errorHandler.buildDepError "hspec-hedgehog"))
            (hsPkgs."http-api-data" or (errorHandler.buildDepError "http-api-data"))
            (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
            (hsPkgs."http-client-tls" or (errorHandler.buildDepError "http-client-tls"))
            (hsPkgs."http-media" or (errorHandler.buildDepError "http-media"))
            (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
            (hsPkgs."int-cast" or (errorHandler.buildDepError "int-cast"))
            (hsPkgs."iohk-monitoring" or (errorHandler.buildDepError "iohk-monitoring"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."io-sim" or (errorHandler.buildDepError "io-sim"))
            (hsPkgs."lattices" or (errorHandler.buildDepError "lattices"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
            (hsPkgs."MonadRandom" or (errorHandler.buildDepError "MonadRandom"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
            (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
            (hsPkgs."persistent" or (errorHandler.buildDepError "persistent"))
            (hsPkgs."persistent-sqlite" or (errorHandler.buildDepError "persistent-sqlite"))
            (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
            (hsPkgs."pretty-simple" or (errorHandler.buildDepError "pretty-simple"))
            (hsPkgs."regex-pcre-builtin" or (errorHandler.buildDepError "regex-pcre-builtin"))
            (hsPkgs."OddWord" or (errorHandler.buildDepError "OddWord"))
            (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-classes" or (errorHandler.buildDepError "quickcheck-classes"))
            (hsPkgs."quickcheck-state-machine" or (errorHandler.buildDepError "quickcheck-state-machine"))
            (hsPkgs."quickcheck-quid" or (errorHandler.buildDepError "quickcheck-quid"))
            (hsPkgs."quiet" or (errorHandler.buildDepError "quiet"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."retry" or (errorHandler.buildDepError "retry"))
            (hsPkgs."safe" or (errorHandler.buildDepError "safe"))
            (hsPkgs."scrypt" or (errorHandler.buildDepError "scrypt"))
            (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
            (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
            (hsPkgs."should-not-typecheck" or (errorHandler.buildDepError "should-not-typecheck"))
            (hsPkgs."splitmix" or (errorHandler.buildDepError "splitmix"))
            (hsPkgs."strict-non-empty-containers" or (errorHandler.buildDepError "strict-non-empty-containers"))
            (hsPkgs."openapi3" or (errorHandler.buildDepError "openapi3"))
            (hsPkgs."servant-openapi3" or (errorHandler.buildDepError "servant-openapi3"))
            (hsPkgs."string-qq" or (errorHandler.buildDepError "string-qq"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."text-class" or (errorHandler.buildDepError "text-class"))
            (hsPkgs."tls" or (errorHandler.buildDepError "tls"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."tree-diff" or (errorHandler.buildDepError "tree-diff"))
            (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
            (hsPkgs."unliftio-core" or (errorHandler.buildDepError "unliftio-core"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."x509" or (errorHandler.buildDepError "x509"))
            (hsPkgs."x509-store" or (errorHandler.buildDepError "x509-store"))
            (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
            (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
            (hsPkgs."wai-extra" or (errorHandler.buildDepError "wai-extra"))
            (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover.components.exes.hspec-discover or (pkgs.buildPackages.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
            ];
          buildable = true;
          modules = [
            "Cardano/Api/GenSpec"
            "Cardano/Byron/Codec/CborSpec"
            "Cardano/DB/Sqlite/DeleteSpec"
            "Cardano/Pool/DB/Arbitrary"
            "Cardano/Pool/DB/MVarSpec"
            "Cardano/Pool/DB/Properties"
            "Cardano/Pool/DB/SqliteSpec"
            "Cardano/Pool/RankSpec"
            "Cardano/Wallet/Address/PoolSpec"
            "Cardano/Wallet/Api/Malformed"
            "Cardano/Wallet/Api/Server/TlsSpec"
            "Cardano/Wallet/Api/ServerSpec"
            "Cardano/Wallet/Api/TypesSpec"
            "Cardano/Wallet/ApiSpec"
            "Cardano/Wallet/CoinSelectionSpec"
            "Cardano/Wallet/CoinSelection/InternalSpec"
            "Cardano/Wallet/CoinSelection/Internal/BalanceSpec"
            "Cardano/Wallet/CoinSelection/Internal/CollateralSpec"
            "Cardano/Wallet/DB/Arbitrary"
            "Cardano/Wallet/DB/MVarSpec"
            "Cardano/Wallet/DB/Properties"
            "Cardano/Wallet/DB/SqliteSpec"
            "Cardano/Wallet/DB/Sqlite/StoresSpec"
            "Cardano/Wallet/DB/Sqlite/TypesSpec"
            "Cardano/Wallet/DB/StateMachine"
            "Cardano/Wallet/DummyTarget/Primitive/Types"
            "Cardano/Wallet/Network/LightSpec"
            "Cardano/Wallet/Network/PortsSpec"
            "Cardano/Wallet/NetworkSpec"
            "Cardano/Wallet/Primitive/AddressDerivation/ByronSpec"
            "Cardano/Wallet/Primitive/AddressDerivation/IcarusSpec"
            "Cardano/Wallet/Primitive/AddressDerivation/MintBurnSpec"
            "Cardano/Wallet/Primitive/AddressDerivationSpec"
            "Cardano/Wallet/Primitive/AddressDiscovery/RandomSpec"
            "Cardano/Wallet/Primitive/AddressDiscovery/SequentialSpec"
            "Cardano/Wallet/Primitive/AddressDiscovery/SharedSpec"
            "Cardano/Wallet/Primitive/Delegation/StateSpec"
            "Cardano/Wallet/Primitive/AddressDiscoverySpec"
            "Cardano/Wallet/Primitive/BlockSummarySpec"
            "Cardano/Wallet/Primitive/CollateralSpec"
            "Cardano/Wallet/Primitive/MigrationSpec"
            "Cardano/Wallet/Primitive/Migration/PlanningSpec"
            "Cardano/Wallet/Primitive/Migration/SelectionSpec"
            "Cardano/Wallet/Primitive/ModelSpec"
            "Cardano/Wallet/Primitive/PassphraseSpec"
            "Cardano/Wallet/Primitive/Passphrase/LegacySpec"
            "Cardano/Wallet/Primitive/Slotting/Legacy"
            "Cardano/Wallet/Primitive/SlottingSpec"
            "Cardano/Wallet/Primitive/SyncProgressSpec"
            "Cardano/Wallet/Primitive/Types/AddressSpec"
            "Cardano/Wallet/Primitive/Types/CoinSpec"
            "Cardano/Wallet/Primitive/Types/HashSpec"
            "Cardano/Wallet/Primitive/Types/TokenBundleSpec"
            "Cardano/Wallet/Primitive/Types/TokenMapSpec"
            "Cardano/Wallet/Primitive/Types/TokenMapSpec/TypeErrorSpec"
            "Cardano/Wallet/Primitive/Types/TokenPolicySpec"
            "Cardano/Wallet/Primitive/Types/TokenQuantitySpec"
            "Cardano/Wallet/Primitive/Types/TxSpec"
            "Cardano/Wallet/Primitive/Types/UTxOSpec"
            "Cardano/Wallet/Primitive/Types/UTxOIndexSpec"
            "Cardano/Wallet/Primitive/Types/UTxOIndex/TypeErrorSpec"
            "Cardano/Wallet/Primitive/Types/UTxOSelectionSpec"
            "Cardano/Wallet/Primitive/Types/UTxOSelectionSpec/TypeErrorSpec"
            "Cardano/Wallet/Primitive/TypesSpec"
            "Cardano/Wallet/TokenMetadataSpec"
            "Cardano/Wallet/RegistrySpec"
            "Cardano/WalletSpec"
            "Control/Concurrent/ConciergeSpec"
            "Control/Monad/Random/ExtraSpec"
            "Data/Function/UtilsSpec"
            "Data/QuantitySpec"
            "Data/Time/TextSpec"
            "Data/Time/UtilsSpec"
            "Data/Vector/ShuffleSpec"
            "Network/Wai/Middleware/LoggingSpec"
            "Spec"
            ];
          hsSourceDirs = [ "test-common" "test/unit" "test/data" ];
          mainPath = [ "core-unit-test.hs" ];
          };
        };
      benchmarks = {
        "db" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-addresses" or (errorHandler.buildDepError "cardano-addresses"))
            (hsPkgs."cardano-crypto" or (errorHandler.buildDepError "cardano-crypto"))
            (hsPkgs."cardano-wallet-core" or (errorHandler.buildDepError "cardano-wallet-core"))
            (hsPkgs."cardano-wallet-launcher" or (errorHandler.buildDepError "cardano-wallet-launcher"))
            (hsPkgs."cardano-wallet-test-utils" or (errorHandler.buildDepError "cardano-wallet-test-utils"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."fmt" or (errorHandler.buildDepError "fmt"))
            (hsPkgs."iohk-monitoring" or (errorHandler.buildDepError "iohk-monitoring"))
            (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."text-class" or (errorHandler.buildDepError "text-class"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
            ];
          buildable = true;
          modules = [ "Cardano/Wallet/DummyTarget/Primitive/Types" ];
          hsSourceDirs = [ "bench" "test-common" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ./lib/core; }