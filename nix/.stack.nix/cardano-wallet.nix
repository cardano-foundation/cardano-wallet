{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { development = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "cardano-wallet"; version = "2019.6.24"; };
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
      "library" = {};
      exes = {
        "cardano-wallet-http-bridge" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.async)
            (hsPkgs.cardano-wallet-cli)
            (hsPkgs.cardano-wallet-core)
            (hsPkgs.cardano-wallet-http-bridge)
            (hsPkgs.cardano-wallet-launcher)
            (hsPkgs.filepath)
            (hsPkgs.iohk-monitoring)
            (hsPkgs.optparse-applicative)
            (hsPkgs.process)
            (hsPkgs.text)
            (hsPkgs.text-class)
            (hsPkgs.warp)
            ];
          };
        "cardano-wallet-jormungandr" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.async)
            (hsPkgs.bytestring)
            (hsPkgs.cardano-wallet-cli)
            (hsPkgs.cardano-wallet-core)
            (hsPkgs.cardano-wallet-jormungandr)
            (hsPkgs.cardano-wallet-launcher)
            (hsPkgs.filepath)
            (hsPkgs.http-client)
            (hsPkgs.iohk-monitoring)
            (hsPkgs.optparse-applicative)
            (hsPkgs.process)
            (hsPkgs.servant-client)
            (hsPkgs.servant-client-core)
            (hsPkgs.text)
            (hsPkgs.text-class)
            (hsPkgs.warp)
            ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././.; }