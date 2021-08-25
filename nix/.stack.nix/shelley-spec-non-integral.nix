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
      rev = "6aa1cd0a64a464371b94d4ac182e7e2cddc83a36";
      sha256 = "1yv2biqc2q01xn7i7h7d1yn8dzygnqn8mywpjfs1i0pa7gnf5q14";
      }) // {
      url = "https://github.com/input-output-hk/cardano-ledger-specs";
      rev = "6aa1cd0a64a464371b94d4ac182e7e2cddc83a36";
      sha256 = "1yv2biqc2q01xn7i7h7d1yn8dzygnqn8mywpjfs1i0pa7gnf5q14";
      };
    postUnpack = "sourceRoot+=/shelley/chain-and-ledger/dependencies/non-integer; echo source root reset to \$sourceRoot";
    }