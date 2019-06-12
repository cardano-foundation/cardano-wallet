{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { development = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "bech32"; version = "2019.5.24"; };
      license = "MIT";
      copyright = "2017 Marko Bencun, 2019 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK Engineering Team";
      homepage = "https://github.com/input-output-hk/cardano-wallet";
      url = "";
      synopsis = "Implementation of the Bech32 segwit address format (BIP 0173).";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.array)
          (hsPkgs.base)
          (hsPkgs.bytestring)
          (hsPkgs.containers)
          (hsPkgs.extra)
          (hsPkgs.text)
          ];
        };
      tests = {
        "bech32-test" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.bech32)
            (hsPkgs.containers)
            (hsPkgs.extra)
            (hsPkgs.hspec)
            (hsPkgs.bytestring)
            (hsPkgs.QuickCheck)
            (hsPkgs.text)
            (hsPkgs.vector)
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover or (pkgs.buildPackages.hspec-discover))
            ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././lib/bech32; }