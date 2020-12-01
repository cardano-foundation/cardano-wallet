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
      identifier = { name = "nothunks"; version = "0.1.1.0"; };
      license = "MIT";
      copyright = "2018-2020 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Examine values for unexpected thunks";
      description = "Long lived application data typically should not contain\nany thunks. This library can be used to examine values for\nunexpected thunks, which can then be used in assertions.\nThis can be invaluable in avoiding memory leaks, or tracking\ndown existing ones.";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."ghc-heap" or (errorHandler.buildDepError "ghc-heap"))
          ];
        buildable = true;
        };
      tests = {
        "nothunks-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/nothunks.git";
      rev = "a4d072f973a6054518a1fc176608b7c5c8dc8917";
      sha256 = "13d44qpj2ar6mwjr8c0sxmkcqjgsrf5yybczlca2fsip5cjc16v2";
      });
    }