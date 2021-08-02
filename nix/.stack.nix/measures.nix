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
    flags = { development = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "measures"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "An abstraction for (tuples of) measured quantities";
      description = "";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base-deriving-via" or (errorHandler.buildDepError "base-deriving-via"))
          ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."measures" or (errorHandler.buildDepError "measures"))
            ];
          buildable = true;
          };
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
    postUnpack = "sourceRoot+=/measures; echo source root reset to \$sourceRoot";
    }