{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "cardano-wallet"; version = "2019.5.24"; };
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
        "cardano-wallet" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.aeson)
            (hsPkgs.aeson-pretty)
            (hsPkgs.bytestring)
            (hsPkgs.cardano-wallet-cli)
            (hsPkgs.cardano-wallet-core)
            (hsPkgs.cardano-wallet-http-bridge)
            (hsPkgs.docopt)
            (hsPkgs.iohk-monitoring)
            (hsPkgs.http-client)
            (hsPkgs.servant-client)
            (hsPkgs.servant-client-core)
            (hsPkgs.servant-server)
            (hsPkgs.text)
            (hsPkgs.text-class)
            ];
          };
        "cardano-wallet-launcher" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.cardano-wallet-cli)
            (hsPkgs.cardano-wallet-launcher)
            (hsPkgs.directory)
            (hsPkgs.docopt)
            (hsPkgs.filepath)
            (hsPkgs.fmt)
            (hsPkgs.process)
            (hsPkgs.say)
            (hsPkgs.text)
            (hsPkgs.text-class)
            ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././.; }