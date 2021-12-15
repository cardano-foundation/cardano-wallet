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
      identifier = { name = "cardano-protocol-tpraos"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2021 Input Output (Hong Kong) Ltd.";
      maintainer = "formal.methods@iohk.io";
      author = "IOHK Formal Methods Team";
      homepage = "";
      url = "";
      synopsis = "";
      description = "Cardano Protocol: Transitional Praos";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cardano-ledger-core" or (errorHandler.buildDepError "cardano-ledger-core"))
          (hsPkgs."cardano-slotting" or (errorHandler.buildDepError "cardano-slotting"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."quiet" or (errorHandler.buildDepError "quiet"))
          (hsPkgs."non-integral" or (errorHandler.buildDepError "non-integral"))
          (hsPkgs."small-steps" or (errorHandler.buildDepError "small-steps"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-ledger";
      rev = "bf008ce028751cae9fb0b53c3bef20f07c06e333";
      sha256 = "0my3801w1vinc0kf5yh9lxl6saqxgwm6ccg0vvzi104pafcwwcqx";
      }) // {
      url = "https://github.com/input-output-hk/cardano-ledger";
      rev = "bf008ce028751cae9fb0b53c3bef20f07c06e333";
      sha256 = "0my3801w1vinc0kf5yh9lxl6saqxgwm6ccg0vvzi104pafcwwcqx";
      };
    postUnpack = "sourceRoot+=/libs/cardano-protocol-tpraos; echo source root reset to \$sourceRoot";
    }