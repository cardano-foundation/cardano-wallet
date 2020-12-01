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
      specVersion = "1.8";
      identifier = { name = "shelley-spec-non-integral"; version = "0.1.0.0"; };
      license = "NONE";
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
        "shelley-spec-non-integral-test" = {
          depends = (pkgs.lib).optionals (!flags.development) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."shelley-spec-non-integral" or (errorHandler.buildDepError "shelley-spec-non-integral"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-ledger-specs";
      rev = "d6179d72c52588460c1d57b932a2fd0724c5db32";
      sha256 = "0fwhnib6raiq2lisbabchdd45wmlj1kfd21a3zbdmgc468j16clw";
      });
    postUnpack = "sourceRoot+=/shelley/chain-and-ledger/dependencies/non-integer; echo source root reset to \$sourceRoot";
    }