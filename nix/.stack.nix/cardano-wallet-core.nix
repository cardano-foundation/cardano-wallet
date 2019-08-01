{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { development = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "cardano-wallet-core"; version = "2019.7.24"; };
      license = "MIT";
      copyright = "2019 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK Engineering Team";
      homepage = "https://github.com/input-output-hk/cardano-wallet";
      url = "";
      synopsis = "The Wallet Backend for a Cardano node.";
      description = "Please see README.md";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.aeson)
          (hsPkgs.async)
          (hsPkgs.base)
          (hsPkgs.base58-bytestring)
          (hsPkgs.basement)
          (hsPkgs.binary)
          (hsPkgs.bytestring)
          (hsPkgs.cardano-crypto)
          (hsPkgs.cborg)
          (hsPkgs.containers)
          (hsPkgs.contra-tracer)
          (hsPkgs.cryptonite)
          (hsPkgs.deepseq)
          (hsPkgs.exceptions)
          (hsPkgs.extra)
          (hsPkgs.fast-logger)
          (hsPkgs.fmt)
          (hsPkgs.foldl)
          (hsPkgs.generic-lens)
          (hsPkgs.http-api-data)
          (hsPkgs.http-media)
          (hsPkgs.http-types)
          (hsPkgs.iohk-monitoring)
          (hsPkgs.memory)
          (hsPkgs.monad-logger)
          (hsPkgs.network)
          (hsPkgs.path-pieces)
          (hsPkgs.persistent)
          (hsPkgs.persistent-sqlite)
          (hsPkgs.persistent-template)
          (hsPkgs.retry)
          (hsPkgs.servant)
          (hsPkgs.servant-server)
          (hsPkgs.split)
          (hsPkgs.streaming-commons)
          (hsPkgs.text)
          (hsPkgs.text-class)
          (hsPkgs.time)
          (hsPkgs.transformers)
          (hsPkgs.unordered-containers)
          (hsPkgs.vector)
          (hsPkgs.wai)
          (hsPkgs.warp)
          ];
        };
      tests = {
        "unit" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.aeson)
            (hsPkgs.aeson-qq)
            (hsPkgs.async)
            (hsPkgs.bytestring)
            (hsPkgs.cardano-crypto)
            (hsPkgs.cardano-wallet-core)
            (hsPkgs.cardano-wallet-test-utils)
            (hsPkgs.containers)
            (hsPkgs.cryptonite)
            (hsPkgs.directory)
            (hsPkgs.deepseq)
            (hsPkgs.extra)
            (hsPkgs.file-embed)
            (hsPkgs.fmt)
            (hsPkgs.foldl)
            (hsPkgs.generic-arbitrary)
            (hsPkgs.generic-lens)
            (hsPkgs.hspec)
            (hsPkgs.hspec-golden-aeson)
            (hsPkgs.http-api-data)
            (hsPkgs.http-client)
            (hsPkgs.http-types)
            (hsPkgs.iohk-monitoring)
            (hsPkgs.lens)
            (hsPkgs.memory)
            (hsPkgs.network)
            (hsPkgs.QuickCheck)
            (hsPkgs.quickcheck-state-machine)
            (hsPkgs.random)
            (hsPkgs.servant)
            (hsPkgs.servant-server)
            (hsPkgs.servant-swagger)
            (hsPkgs.stm)
            (hsPkgs.swagger2)
            (hsPkgs.temporary)
            (hsPkgs.text)
            (hsPkgs.text-class)
            (hsPkgs.time)
            (hsPkgs.transformers)
            (hsPkgs.tree-diff)
            (hsPkgs.unordered-containers)
            (hsPkgs.yaml)
            (hsPkgs.warp)
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover or (pkgs.buildPackages.hspec-discover))
            ];
          };
        };
      benchmarks = {
        "db" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.cardano-crypto)
            (hsPkgs.cardano-wallet-core)
            (hsPkgs.containers)
            (hsPkgs.criterion)
            (hsPkgs.cryptonite)
            (hsPkgs.deepseq)
            (hsPkgs.directory)
            (hsPkgs.fmt)
            (hsPkgs.iohk-monitoring)
            (hsPkgs.memory)
            (hsPkgs.split)
            (hsPkgs.temporary)
            (hsPkgs.text)
            (hsPkgs.text-class)
            (hsPkgs.time)
            ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././lib/core; }
