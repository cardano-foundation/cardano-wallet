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
      identifier = { name = "cardano-client"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2019 Input Output (Hong Kong) Ltd.";
      maintainer = "operations@iohk.io";
      author = "IOHK Engineering Team";
      homepage = "";
      url = "";
      synopsis = "An API for ouroboros-network";
      description = "";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
          (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
          (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
          (hsPkgs."network-mux" or (errorHandler.buildDepError "network-mux"))
          ];
        buildable = true;
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
    postUnpack = "sourceRoot+=/cardano-client; echo source root reset to \$sourceRoot";
    }