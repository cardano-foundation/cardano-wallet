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
    flags = { development = false; external-libsodium-vrf = true; };
    package = {
      specVersion = "1.10";
      identifier = { name = "cardano-crypto-praos"; version = "2.0.0"; };
      license = "Apache-2.0";
      copyright = "2019 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Crypto primitives from libsodium";
      description = "VRF (and KES, tba) primitives from libsodium.";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
          (hsPkgs."reflection" or (errorHandler.buildDepError "reflection"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
        pkgconfig = [
          (pkgconfPkgs."libsodium" or (errorHandler.pkgConfDepError "libsodium"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-base";
      rev = "7d795c3040ea7785812efa1c97864bbb41b15d3e";
      sha256 = "130i0yj4y9br1m2bhisi6wni3f40i31nfhg878hv0kwi17chl9sz";
      });
    postUnpack = "sourceRoot+=/cardano-crypto-praos; echo source root reset to \$sourceRoot";
    }