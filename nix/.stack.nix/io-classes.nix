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
    flags = { checktvarinvariant = false; asserts = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "io-classes"; version = "0.2.0.0"; };
      license = "Apache-2.0";
      copyright = "2019 Input Output (Hong Kong) Ltd.";
      maintainer = "";
      author = "Alexander Vieth, Marcin Szamotulski, Duncan Coutts";
      homepage = "";
      url = "";
      synopsis = "Type classes for concurrency with STM, ST and timing";
      description = "";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/ouroboros-network";
      rev = "d613de3d872ec8b4a5da0c98afb443f322dc4dab";
      sha256 = "0lfbipfdrzay8v1pcazx0qgkda3d1j0505yig9jrml9j7991rmhl";
      }) // {
      url = "https://github.com/input-output-hk/ouroboros-network";
      rev = "d613de3d872ec8b4a5da0c98afb443f322dc4dab";
      sha256 = "0lfbipfdrzay8v1pcazx0qgkda3d1j0505yig9jrml9j7991rmhl";
      };
    postUnpack = "sourceRoot+=/io-classes; echo source root reset to \$sourceRoot";
    }