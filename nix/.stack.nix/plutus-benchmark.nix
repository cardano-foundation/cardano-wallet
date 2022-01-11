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
      identifier = { name = "plutus-benchmark"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "radu.ometita@iohk.io";
      author = "Radu Ometita";
      homepage = "https://github.com/iohk/plutus#readme";
      url = "";
      synopsis = "";
      description = "Please see the README on GitHub at <https://github.com/input-output-hk/plutus#readme>";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      sublibs = {
        "plutus-benchmark-common" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ];
          buildable = true;
          };
        "nofib-internal" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."plutus-benchmark".components.sublibs.plutus-benchmark-common or (errorHandler.buildDepError "plutus-benchmark:plutus-benchmark-common"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
            (hsPkgs."plutus-tx-plugin" or (errorHandler.buildDepError "plutus-tx-plugin"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            ];
          buildable = true;
          };
        "lists-internal" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."plutus-benchmark".components.sublibs.plutus-benchmark-common or (errorHandler.buildDepError "plutus-benchmark:plutus-benchmark-common"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
            (hsPkgs."plutus-tx-plugin" or (errorHandler.buildDepError "plutus-tx-plugin"))
            ];
          buildable = true;
          };
        };
      exes = {
        "nofib-exe" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."plutus-benchmark".components.sublibs.plutus-benchmark-common or (errorHandler.buildDepError "plutus-benchmark:plutus-benchmark-common"))
            (hsPkgs."plutus-benchmark".components.sublibs.nofib-internal or (errorHandler.buildDepError "plutus-benchmark:nofib-internal"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
            (hsPkgs."ansi-wl-pprint" or (errorHandler.buildDepError "ansi-wl-pprint"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            ];
          buildable = true;
          };
        "list-sort-exe" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."plutus-benchmark".components.sublibs.plutus-benchmark-common or (errorHandler.buildDepError "plutus-benchmark:plutus-benchmark-common"))
            (hsPkgs."plutus-benchmark".components.sublibs.lists-internal or (errorHandler.buildDepError "plutus-benchmark:lists-internal"))
            (hsPkgs."monoidal-containers" or (errorHandler.buildDepError "monoidal-containers"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            ];
          buildable = true;
          };
        };
      tests = {
        "plutus-benchmark-nofib-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."plutus-benchmark".components.sublibs.plutus-benchmark-common or (errorHandler.buildDepError "plutus-benchmark:plutus-benchmark-common"))
            (hsPkgs."plutus-benchmark".components.sublibs.nofib-internal or (errorHandler.buildDepError "plutus-benchmark:nofib-internal"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            ];
          buildable = true;
          };
        "plutus-benchmark-lists-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."plutus-benchmark".components.sublibs.plutus-benchmark-common or (errorHandler.buildDepError "plutus-benchmark:plutus-benchmark-common"))
            (hsPkgs."plutus-benchmark".components.sublibs.lists-internal or (errorHandler.buildDepError "plutus-benchmark:lists-internal"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "nofib" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."plutus-benchmark".components.sublibs.plutus-benchmark-common or (errorHandler.buildDepError "plutus-benchmark:plutus-benchmark-common"))
            (hsPkgs."plutus-benchmark".components.sublibs.nofib-internal or (errorHandler.buildDepError "plutus-benchmark:nofib-internal"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            ];
          buildable = true;
          };
        "nofib-hs" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."plutus-benchmark".components.sublibs.plutus-benchmark-common or (errorHandler.buildDepError "plutus-benchmark:plutus-benchmark-common"))
            (hsPkgs."plutus-benchmark".components.sublibs.nofib-internal or (errorHandler.buildDepError "plutus-benchmark:nofib-internal"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            ];
          buildable = true;
          };
        "lists" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."plutus-benchmark".components.sublibs.plutus-benchmark-common or (errorHandler.buildDepError "plutus-benchmark:plutus-benchmark-common"))
            (hsPkgs."plutus-benchmark".components.sublibs.lists-internal or (errorHandler.buildDepError "plutus-benchmark:lists-internal"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            ];
          buildable = true;
          };
        "validation" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."plutus-benchmark".components.sublibs.plutus-benchmark-common or (errorHandler.buildDepError "plutus-benchmark:plutus-benchmark-common"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            ];
          buildable = true;
          };
        "cek-calibration" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
            (hsPkgs."plutus-tx-plugin" or (errorHandler.buildDepError "plutus-tx-plugin"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/plutus";
      rev = "103fe104e8138dcf2a8d52d5e45177bac4e396b1";
      sha256 = "169p3si18mgzb7s91ghrq0crsqq2xbqjk1q0hpfild2dbgcdpcsd";
      }) // {
      url = "https://github.com/input-output-hk/plutus";
      rev = "103fe104e8138dcf2a8d52d5e45177bac4e396b1";
      sha256 = "169p3si18mgzb7s91ghrq0crsqq2xbqjk1q0hpfild2dbgcdpcsd";
      };
    postUnpack = "sourceRoot+=/plutus-benchmark; echo source root reset to \$sourceRoot";
    }