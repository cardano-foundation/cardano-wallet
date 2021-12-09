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
      rev = "654f5b7c76f7cc57900b4ddc664a82fc3b925fb0";
      sha256 = "0j4x9zbx5dkww82sqi086h39p456iq5xr476ylmrnpwcpfb4xai4";
      }) // {
      url = "https://github.com/input-output-hk/cardano-base";
      rev = "654f5b7c76f7cc57900b4ddc664a82fc3b925fb0";
      sha256 = "0j4x9zbx5dkww82sqi086h39p456iq5xr476ylmrnpwcpfb4xai4";
      };
    postUnpack = "sourceRoot+=/measures; echo source root reset to \$sourceRoot";
    }