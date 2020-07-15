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
      identifier = { name = "io-sim-classes"; version = "0.1.0.0"; };
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
        "test-io-sim-classes" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."io-sim-classes" or (errorHandler.buildDepError "io-sim-classes"))
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
      rev = "1c8924b856baf809f193798ff686cdb2c0ed25d2";
      sha256 = "1ird981gi7wbj21g8d65jqv4q4iwch2akky4r8pf8r2w3b3767pv";
      });
    postUnpack = "sourceRoot+=/io-sim-classes; echo source root reset to \$sourceRoot";
    }