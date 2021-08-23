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
      specVersion = "1.10";
      identifier = { name = "ouroboros-network-testing"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2019 Input Output (Hong Kong) Ltd.";
      maintainer = "";
      author = "Alexander Vieth, Marcin Szamotulski, Duncan Coutts, Karl Knuttson";
      homepage = "";
      url = "";
      synopsis = "Common modules used for testing in ouroboros-network and ouroboros-consensus";
      description = "";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
          (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          (hsPkgs."io-sim" or (errorHandler.buildDepError "io-sim"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/ouroboros-network";
      rev = "877ce057ff6fb086474c8eaad53f2b7f0e0fce6b";
      sha256 = "1kp0qysfy3hl96a3a61rijascq36f1imh3z4jy0vyiygb6qrv47z";
      }) // {
      url = "https://github.com/input-output-hk/ouroboros-network";
      rev = "877ce057ff6fb086474c8eaad53f2b7f0e0fce6b";
      sha256 = "1kp0qysfy3hl96a3a61rijascq36f1imh3z4jy0vyiygb6qrv47z";
      };
    postUnpack = "sourceRoot+=/ouroboros-network-testing; echo source root reset to \$sourceRoot";
    }