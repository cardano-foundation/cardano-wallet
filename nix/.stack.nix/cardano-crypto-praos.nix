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
      specVersion = "2.2";
      identifier = { name = "cardano-crypto-praos"; version = "2.0.0"; };
      license = "Apache-2.0";
      copyright = "2019-2021 IOHK";
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
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
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
      rev = "c028519d1180e42d6ab3ddf97d6fa57395966c2e";
      sha256 = "11n2731y1z3ggp2lvphsbrxcppk29knknynza4259694ws533kqz";
      }) // {
      url = "https://github.com/input-output-hk/cardano-base";
      rev = "c028519d1180e42d6ab3ddf97d6fa57395966c2e";
      sha256 = "11n2731y1z3ggp2lvphsbrxcppk29knknynza4259694ws533kqz";
      };
    postUnpack = "sourceRoot+=/cardano-crypto-praos; echo source root reset to \$sourceRoot";
    }