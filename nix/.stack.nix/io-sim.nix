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
    flags = { asserts = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "io-sim"; version = "0.2.0.0"; };
      license = "Apache-2.0";
      copyright = "2019-2020 Input Output (Hong Kong) Ltd.";
      maintainer = "";
      author = "Duncan Coutts, Marcin Szamotulski, Alexander Vieth";
      homepage = "";
      url = "";
      synopsis = "A pure simlator for monadic concurrency with STM";
      description = "";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."psqueues" or (errorHandler.buildDepError "psqueues"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."quiet" or (errorHandler.buildDepError "quiet"))
          ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."io-sim" or (errorHandler.buildDepError "io-sim"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
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
    postUnpack = "sourceRoot+=/io-sim; echo source root reset to \$sourceRoot";
    }