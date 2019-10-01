{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { development = false; };
    package = {
      specVersion = "1.10";
      identifier = {
        name = "cardano-wallet-http-bridge";
        version = "2019.9.13";
        };
      license = "Apache-2.0";
      copyright = "2019 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK Engineering Team";
      homepage = "https://github.com/input-output-hk/cardano-wallet";
      url = "";
      synopsis = "Wallet backend protocol-specific bits implemented using the cardano-http-bridge";
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
          (hsPkgs.binary)
          (hsPkgs.bytestring)
          (hsPkgs.cardano-crypto)
          (hsPkgs.cardano-wallet-cli)
          (hsPkgs.cardano-wallet-core)
          (hsPkgs.cardano-wallet-launcher)
          (hsPkgs.cborg)
          (hsPkgs.cryptonite)
          (hsPkgs.deepseq)
          (hsPkgs.directory)
          (hsPkgs.either)
          (hsPkgs.exceptions)
          (hsPkgs.filepath)
          (hsPkgs.fmt)
          (hsPkgs.http-api-data)
          (hsPkgs.http-client)
          (hsPkgs.http-media)
          (hsPkgs.http-types)
          (hsPkgs.iohk-monitoring)
          (hsPkgs.memory)
          (hsPkgs.retry)
          (hsPkgs.servant)
          (hsPkgs.servant-client)
          (hsPkgs.servant-client-core)
          (hsPkgs.text)
          (hsPkgs.text-class)
          (hsPkgs.time)
          (hsPkgs.transformers)
          (hsPkgs.warp)
          ];
        };
      exes = {
        "cardano-wallet-http-bridge" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.cardano-wallet-cli)
            (hsPkgs.cardano-wallet-core)
            (hsPkgs.cardano-wallet-http-bridge)
            (hsPkgs.cardano-wallet-launcher)
            (hsPkgs.filepath)
            (hsPkgs.iohk-monitoring)
            (hsPkgs.optparse-applicative)
            (hsPkgs.process)
            (hsPkgs.text)
            ];
          };
        };
      tests = {
        "unit" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.aeson)
            (hsPkgs.bytestring)
            (hsPkgs.cardano-crypto)
            (hsPkgs.cardano-wallet-core)
            (hsPkgs.cardano-wallet-http-bridge)
            (hsPkgs.cborg)
            (hsPkgs.containers)
            (hsPkgs.digest)
            (hsPkgs.fmt)
            (hsPkgs.hspec)
            (hsPkgs.hspec-golden-aeson)
            (hsPkgs.memory)
            (hsPkgs.QuickCheck)
            (hsPkgs.text)
            (hsPkgs.text-class)
            (hsPkgs.transformers)
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover or (pkgs.buildPackages.hspec-discover))
            ];
          };
        "http-bridge-integration" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.aeson)
            (hsPkgs.async)
            (hsPkgs.cardano-crypto)
            (hsPkgs.cardano-wallet-cli)
            (hsPkgs.cardano-wallet-core)
            (hsPkgs.cardano-wallet-core-integration)
            (hsPkgs.cardano-wallet-http-bridge)
            (hsPkgs.cardano-wallet-launcher)
            (hsPkgs.cardano-wallet-test-utils)
            (hsPkgs.command)
            (hsPkgs.cborg)
            (hsPkgs.directory)
            (hsPkgs.filepath)
            (hsPkgs.generic-lens)
            (hsPkgs.hspec)
            (hsPkgs.hspec-core)
            (hsPkgs.hspec-expectations-lifted)
            (hsPkgs.http-client)
            (hsPkgs.http-types)
            (hsPkgs.iohk-monitoring)
            (hsPkgs.process)
            (hsPkgs.retry)
            (hsPkgs.temporary)
            (hsPkgs.text)
            (hsPkgs.text-class)
            (hsPkgs.time)
            (hsPkgs.transformers)
            ];
          };
        };
      benchmarks = {
        "restore" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.cardano-wallet-core)
            (hsPkgs.cardano-wallet-http-bridge)
            (hsPkgs.cardano-wallet-launcher)
            (hsPkgs.containers)
            (hsPkgs.criterion-measurement)
            (hsPkgs.cryptonite)
            (hsPkgs.deepseq)
            (hsPkgs.digest)
            (hsPkgs.fmt)
            (hsPkgs.generic-lens)
            (hsPkgs.iohk-monitoring)
            (hsPkgs.persistent)
            (hsPkgs.persistent-template)
            (hsPkgs.process)
            (hsPkgs.say)
            (hsPkgs.temporary)
            (hsPkgs.text)
            (hsPkgs.text-class)
            (hsPkgs.time)
            (hsPkgs.transformers)
            ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././lib/http-bridge; }