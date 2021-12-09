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
      identifier = { name = "word-array"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "michael.peyton-jones@iohk.io";
      author = "Zachary Churchill, Michael Peyton Jones";
      homepage = "https://github.com/plutus";
      url = "";
      synopsis = "";
      description = "Treat integral types as arrays of smaller integral types";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."mono-traversable" or (errorHandler.buildDepError "mono-traversable"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."word-array" or (errorHandler.buildDepError "word-array"))
            (hsPkgs."mono-traversable" or (errorHandler.buildDepError "mono-traversable"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-bench" or (errorHandler.buildDepError "tasty-bench"))
            (hsPkgs."word-array" or (errorHandler.buildDepError "word-array"))
            (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/plutus";
      rev = "1efbb276ef1a10ca6961d0fd32e6141e9798bd11";
      sha256 = "1jicyk4hr8p0xksj4048gdxndrb42jz4wsnkhc3ymxbm5v6snalf";
      }) // {
      url = "https://github.com/input-output-hk/plutus";
      rev = "1efbb276ef1a10ca6961d0fd32e6141e9798bd11";
      sha256 = "1jicyk4hr8p0xksj4048gdxndrb42jz4wsnkhc3ymxbm5v6snalf";
      };
    postUnpack = "sourceRoot+=/word-array; echo source root reset to \$sourceRoot";
    }