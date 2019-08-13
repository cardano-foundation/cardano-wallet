{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { development = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "cardano-wallet-shelley"; version = "2019.7.24"; };
      license = "Apache-2.0";
      copyright = "2019 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK Engineering Team";
      homepage = "https://github.com/input-output-hk/cardano-wallet";
      url = "";
      synopsis = "Wallet backend protocol-specific bits implemented using Shelley nodes";
      description = "Please see README.md";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.bytestring)
          (hsPkgs.cardano-ledger)
          (hsPkgs.cardano-crypto-wrapper)
          (hsPkgs.contra-tracer)
          (hsPkgs.io-sim-classes)
          (hsPkgs.network)
          (hsPkgs.network-mux)
          (hsPkgs.ouroboros-consensus)
          (hsPkgs.ouroboros-network)
          (hsPkgs.serialise)
          (hsPkgs.text)
          (hsPkgs.transformers)
          (hsPkgs.typed-protocols)
          (hsPkgs.typed-protocols-cbor)
          ];
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././lib/shelley; }