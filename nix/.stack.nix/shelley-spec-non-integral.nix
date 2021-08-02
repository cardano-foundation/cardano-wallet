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
      specVersion = "2.2";
      identifier = { name = "shelley-spec-non-integral"; version = "0.1.0.0"; };
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
        "shelley-spec-non-integral-test" = {
          depends = [
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
      rev = "bc5e54f0611a416db1e906d3783e4b973429f5eb";
      sha256 = "1j7b6aglr1j51qmfn4nx7mamv81qyl8411fwsx6bw2cpghvizpmg";
      }) // {
      url = "https://github.com/input-output-hk/cardano-ledger-specs";
      rev = "bc5e54f0611a416db1e906d3783e4b973429f5eb";
      sha256 = "1j7b6aglr1j51qmfn4nx7mamv81qyl8411fwsx6bw2cpghvizpmg";
      };
    postUnpack = "sourceRoot+=/shelley/chain-and-ledger/dependencies/non-integer; echo source root reset to \$sourceRoot";
    }