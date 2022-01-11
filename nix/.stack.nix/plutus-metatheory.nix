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
      identifier = { name = "plutus-metatheory"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "james.chapman@iohk.io";
      author = "James Chapman";
      homepage = "https://github.com/input-output-hk/plutus";
      url = "";
      synopsis = "Command line tool for running plutus core programs";
      description = "";
      buildType = "Custom";
      isLocal = true;
      setup-depends = [
        (hsPkgs.buildPackages.base or (pkgs.buildPackages.base or (errorHandler.setupDepError "base")))
        (hsPkgs.buildPackages.Cabal or (pkgs.buildPackages.Cabal or (errorHandler.setupDepError "Cabal")))
        (hsPkgs.buildPackages.process or (pkgs.buildPackages.process or (errorHandler.setupDepError "process")))
        (hsPkgs.buildPackages.turtle or (pkgs.buildPackages.turtle or (errorHandler.setupDepError "turtle")))
        ];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
          (hsPkgs."ieee754" or (errorHandler.buildDepError "ieee754"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
          (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ];
        buildable = true;
        };
      exes = {
        "plc-agda" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."plutus-metatheory" or (errorHandler.buildDepError "plutus-metatheory"))
            ];
          buildable = true;
          };
        };
      tests = {
        "test1" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."plutus-metatheory" or (errorHandler.buildDepError "plutus-metatheory"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.plutus-core.components.exes.plc or (pkgs.buildPackages.plc or (errorHandler.buildToolDepError "plutus-core:plc")))
            (hsPkgs.buildPackages.plutus-core.components.exes.uplc or (pkgs.buildPackages.uplc or (errorHandler.buildToolDepError "plutus-core:uplc")))
            ];
          buildable = true;
          };
        "test2" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."plutus-metatheory" or (errorHandler.buildDepError "plutus-metatheory"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.plutus-core.components.exes.plc or (pkgs.buildPackages.plc or (errorHandler.buildToolDepError "plutus-core:plc")))
            (hsPkgs.buildPackages.plutus-core.components.exes.uplc or (pkgs.buildPackages.uplc or (errorHandler.buildToolDepError "plutus-core:uplc")))
            ];
          buildable = true;
          };
        "test3" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."lazy-search" or (errorHandler.buildDepError "lazy-search"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."plutus-metatheory" or (errorHandler.buildDepError "plutus-metatheory"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."size-based" or (errorHandler.buildDepError "size-based"))
            (hsPkgs."Stream" or (errorHandler.buildDepError "Stream"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/plutus";
      rev = "4710dff2e30ce131191a6a1ccbe43595b2e3af24";
      sha256 = "0z61mxdk6r092vblfzn81m2d9ss9iqwa1k2nsmmfyhn0q7h66yvg";
      }) // {
      url = "https://github.com/input-output-hk/plutus";
      rev = "4710dff2e30ce131191a6a1ccbe43595b2e3af24";
      sha256 = "0z61mxdk6r092vblfzn81m2d9ss9iqwa1k2nsmmfyhn0q7h66yvg";
      };
    postUnpack = "sourceRoot+=/plutus-metatheory; echo source root reset to \$sourceRoot";
    }