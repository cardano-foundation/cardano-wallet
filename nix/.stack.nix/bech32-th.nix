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
    flags = { werror = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "bech32-th"; version = "1.0.2"; };
      license = "Apache-2.0";
      copyright = "2020 IOHK";
      maintainer = "operations@iohk.io, erikd@mega-nerd.com, jonathan.knowles@iohk.io";
      author = "IOHK Engineering Team";
      homepage = "https://github.com/input-output-hk/bech32";
      url = "";
      synopsis = "Template Haskell extensions to the Bech32 library.";
      description = "Template Haskell extensions to the Bech32 library, including\nquasi-quoters for compile-time checking of Bech32 string\nliterals.";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bech32" or (errorHandler.buildDepError "bech32"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
        buildable = true;
        };
      tests = {
        "bech32-th-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bech32" or (errorHandler.buildDepError "bech32"))
            (hsPkgs."bech32-th" or (errorHandler.buildDepError "bech32-th"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover or (pkgs.buildPackages.hspec-discover or (errorHandler.buildToolDepError "hspec-discover")))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/bech32";
      rev = "dd3fc9488595af23ce80740e2df133bee4c1c1a5";
      sha256 = "0dd6x7pcszn2hcfx16gjfv5v6qsx9afadws2frav0f2fasnhbrfa";
      });
    postUnpack = "sourceRoot+=/bech32-th; echo source root reset to \$sourceRoot";
    }