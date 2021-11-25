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
      identifier = { name = "orphans-deriving-via"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Orphan instances for the base-deriving-via hooks";
      description = "";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base-deriving-via" or (errorHandler.buildDepError "base-deriving-via"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-base";
      rev = "12a58616587689ae725708ffba40a36b57273e91";
      sha256 = "1z98f3m4skmqy782dpcpd3y0k7hccdrz5yl1fjgjs524swh4vv56";
      }) // {
      url = "https://github.com/input-output-hk/cardano-base";
      rev = "12a58616587689ae725708ffba40a36b57273e91";
      sha256 = "1z98f3m4skmqy782dpcpd3y0k7hccdrz5yl1fjgjs524swh4vv56";
      };
    postUnpack = "sourceRoot+=/orphans-deriving-via; echo source root reset to \$sourceRoot";
    }