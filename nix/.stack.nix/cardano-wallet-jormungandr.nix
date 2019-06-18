{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { development = false; };
    package = {
      specVersion = "1.10";
      identifier = {
        name = "cardano-wallet-jormungandr";
        version = "2019.5.24";
        };
      license = "MIT";
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
          (hsPkgs.base58-bytestring)
          (hsPkgs.bech32)
          (hsPkgs.binary)
          (hsPkgs.bytestring)
          (hsPkgs.cardano-wallet-core)
          (hsPkgs.cardano-crypto)
          (hsPkgs.cborg)
          (hsPkgs.exceptions)
          (hsPkgs.http-client)
          (hsPkgs.http-types)
          (hsPkgs.memory)
          (hsPkgs.servant)
          (hsPkgs.servant-client)
          (hsPkgs.servant-client-core)
          (hsPkgs.text)
          (hsPkgs.text-class)
          (hsPkgs.transformers)
          ];
        };
      tests = {
        "unit" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.cardano-wallet-core)
            (hsPkgs.cardano-crypto)
            (hsPkgs.cardano-wallet-jormungandr)
            (hsPkgs.generic-arbitrary)
            (hsPkgs.generic-lens)
            (hsPkgs.hspec)
            (hsPkgs.memory)
            (hsPkgs.QuickCheck)
            (hsPkgs.text)
            (hsPkgs.text-class)
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover or (pkgs.buildPackages.hspec-discover))
            ];
          };
        "integration" = {
          depends = [
            (hsPkgs.async)
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.cardano-wallet-core)
            (hsPkgs.cardano-wallet-jormungandr)
            (hsPkgs.cardano-wallet-launcher)
            (hsPkgs.deepseq)
            (hsPkgs.directory)
            (hsPkgs.hspec)
            (hsPkgs.QuickCheck)
            (hsPkgs.retry)
            (hsPkgs.servant)
            (hsPkgs.transformers)
            ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././lib/jormungandr; }