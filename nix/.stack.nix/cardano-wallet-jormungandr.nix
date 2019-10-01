{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { development = false; };
    package = {
      specVersion = "1.10";
      identifier = {
        name = "cardano-wallet-jormungandr";
        version = "2019.9.13";
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
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.aeson)
          (hsPkgs.async)
          (hsPkgs.base58-bytestring)
          (hsPkgs.bech32)
          (hsPkgs.binary)
          (hsPkgs.bytestring)
          (hsPkgs.cardano-crypto)
          (hsPkgs.cardano-wallet-core)
          (hsPkgs.cardano-wallet-cli)
          (hsPkgs.cardano-wallet-launcher)
          (hsPkgs.cborg)
          (hsPkgs.containers)
          (hsPkgs.cryptonite)
          (hsPkgs.deepseq)
          (hsPkgs.directory)
          (hsPkgs.either)
          (hsPkgs.exceptions)
          (hsPkgs.extra)
          (hsPkgs.filepath)
          (hsPkgs.fmt)
          (hsPkgs.http-client)
          (hsPkgs.http-types)
          (hsPkgs.iohk-monitoring)
          (hsPkgs.lifted-base)
          (hsPkgs.monad-control)
          (hsPkgs.memory)
          (hsPkgs.safe)
          (hsPkgs.servant)
          (hsPkgs.servant-client)
          (hsPkgs.servant-client-core)
          (hsPkgs.text)
          (hsPkgs.text-class)
          (hsPkgs.time)
          (hsPkgs.transformers)
          (hsPkgs.yaml)
          (hsPkgs.warp)
          ];
        };
      exes = {
        "cardano-wallet-jormungandr" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.cardano-wallet-cli)
            (hsPkgs.cardano-wallet-core)
            (hsPkgs.cardano-wallet-jormungandr)
            (hsPkgs.cardano-wallet-launcher)
            (hsPkgs.filepath)
            (hsPkgs.iohk-monitoring)
            (hsPkgs.optparse-applicative)
            (hsPkgs.process)
            (hsPkgs.text)
            (hsPkgs.text-class)
            ];
          };
        };
      tests = {
        "unit" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.aeson)
            (hsPkgs.aeson-qq)
            (hsPkgs.binary)
            (hsPkgs.bytestring)
            (hsPkgs.cardano-wallet-core)
            (hsPkgs.cardano-crypto)
            (hsPkgs.cardano-wallet-jormungandr)
            (hsPkgs.containers)
            (hsPkgs.directory)
            (hsPkgs.generic-arbitrary)
            (hsPkgs.generic-lens)
            (hsPkgs.hspec)
            (hsPkgs.memory)
            (hsPkgs.QuickCheck)
            (hsPkgs.safe)
            (hsPkgs.text)
            (hsPkgs.text-class)
            (hsPkgs.time)
            (hsPkgs.transformers)
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover or (pkgs.buildPackages.hspec-discover))
            ];
          };
        "integration" = {
          depends = [
            (hsPkgs.aeson)
            (hsPkgs.aeson-qq)
            (hsPkgs.async)
            (hsPkgs.base)
            (hsPkgs.binary)
            (hsPkgs.bytestring)
            (hsPkgs.cardano-wallet-cli)
            (hsPkgs.cardano-wallet-core)
            (hsPkgs.cardano-wallet-core-integration)
            (hsPkgs.cardano-wallet-jormungandr)
            (hsPkgs.cardano-wallet-launcher)
            (hsPkgs.cardano-wallet-test-utils)
            (hsPkgs.command)
            (hsPkgs.containers)
            (hsPkgs.cryptonite)
            (hsPkgs.deepseq)
            (hsPkgs.directory)
            (hsPkgs.exceptions)
            (hsPkgs.extra)
            (hsPkgs.filepath)
            (hsPkgs.generic-arbitrary)
            (hsPkgs.generic-lens)
            (hsPkgs.hspec)
            (hsPkgs.hspec-expectations-lifted)
            (hsPkgs.http-api-data)
            (hsPkgs.http-client)
            (hsPkgs.http-types)
            (hsPkgs.iohk-monitoring)
            (hsPkgs.memory)
            (hsPkgs.persistent)
            (hsPkgs.process)
            (hsPkgs.QuickCheck)
            (hsPkgs.retry)
            (hsPkgs.servant)
            (hsPkgs.template-haskell)
            (hsPkgs.temporary)
            (hsPkgs.text)
            (hsPkgs.text-class)
            (hsPkgs.time)
            (hsPkgs.transformers)
            (hsPkgs.warp)
            (hsPkgs.yaml)
            ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././lib/jormungandr; }