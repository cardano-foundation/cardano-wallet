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
      specVersion = "2.2";
      identifier = { name = "non-integral"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "formal.methods@iohk.io";
      author = "IOHK Formal Methods Team";
      homepage = "";
      url = "";
      synopsis = "";
      description = "Implementation decision for non-integer calculations";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        buildable = true;
        };
      tests = {
        "non-integral-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."non-integral" or (errorHandler.buildDepError "non-integral"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ];
          buildable = true;
          };
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
    postUnpack = "sourceRoot+=/libs/non-integral; echo source root reset to \$sourceRoot";
    }