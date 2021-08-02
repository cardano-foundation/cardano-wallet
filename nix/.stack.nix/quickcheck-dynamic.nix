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
      specVersion = "2.2";
      identifier = { name = "quickcheck-dynamic"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "ulf.norell@quviq.com";
      author = "Ulf Norell";
      homepage = "https://github.com/iohk/plutus#readme";
      url = "";
      synopsis = "";
      description = "Please see the README on GitHub at <https://github.com/input-output-hk/plutus#readme>";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          ];
        buildable = true;
        };
      tests = {
        "quickcheck-dynamic-test" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."quickcheck-dynamic" or (errorHandler.buildDepError "quickcheck-dynamic"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/plutus";
      rev = "826c2514a40e962c2e4d56ce912803a434cc28fe";
      sha256 = "04jryrgi2wggkf4a994smbcp8j39clshbzfnvfvvk7lq0sm8kssz";
      }) // {
      url = "https://github.com/input-output-hk/plutus";
      rev = "826c2514a40e962c2e4d56ce912803a434cc28fe";
      sha256 = "04jryrgi2wggkf4a994smbcp8j39clshbzfnvfvvk7lq0sm8kssz";
      };
    postUnpack = "sourceRoot+=/quickcheck-dynamic; echo source root reset to \$sourceRoot";
    }