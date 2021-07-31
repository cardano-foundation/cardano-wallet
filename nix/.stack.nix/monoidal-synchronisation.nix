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
    flags = {};
    package = {
      specVersion = "2.4";
      identifier = { name = "monoidal-synchronisation"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2021 Input Output (Hong Kong) Ltd.";
      maintainer = "coot@coot.me";
      author = "Marcin Szamotulski";
      homepage = "";
      url = "";
      synopsis = "";
      description = "";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."io-sim" or (errorHandler.buildDepError "io-sim"))
            (hsPkgs."monoidal-synchronisation" or (errorHandler.buildDepError "monoidal-synchronisation"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/ouroboros-network";
      rev = "f026e71e8925dc2cb1a55a9a77b54bbeb1273ef0";
      sha256 = "0wmlrkdi7q868872p4ah35z51511yqkr8pj90fwadlr6ipkbpkl9";
      }) // {
      url = "https://github.com/input-output-hk/ouroboros-network";
      rev = "f026e71e8925dc2cb1a55a9a77b54bbeb1273ef0";
      sha256 = "0wmlrkdi7q868872p4ah35z51511yqkr8pj90fwadlr6ipkbpkl9";
      };
    postUnpack = "sourceRoot+=/monoidal-synchronisation; echo source root reset to \$sourceRoot";
    }