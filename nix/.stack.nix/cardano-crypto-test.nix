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
    flags = { development = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "cardano-crypto-test"; version = "1.3.0"; };
      license = "Apache-2.0";
      copyright = "2018 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Test helpers from cardano-crypto exposed to other packages";
      description = "Test helpers from cardano-crypto exposed to other packages";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-binary-test" or (errorHandler.buildDepError "cardano-binary-test"))
          (hsPkgs."cardano-crypto" or (errorHandler.buildDepError "cardano-crypto"))
          (hsPkgs."cardano-crypto-wrapper" or (errorHandler.buildDepError "cardano-crypto-wrapper"))
          (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
          (hsPkgs."cardano-prelude-test" or (errorHandler.buildDepError "cardano-prelude-test"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-ledger";
      rev = "317879efb23d119245dc900f3af8e9c3f4b4e066";
      sha256 = "1isnxralcnnniwlmh93jd4wivd7fixagldzv27dpil6crk38s79c";
      });
    postUnpack = "sourceRoot+=/crypto/test; echo source root reset to \$sourceRoot";
    }