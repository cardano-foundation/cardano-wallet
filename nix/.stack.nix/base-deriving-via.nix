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
      identifier = { name = "base-deriving-via"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "A general hook newtype for use with deriving via";
      description = "";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-base";
      rev = "8c732560b201b5da8e3bdf175c6eda73a32d64bc";
      sha256 = "0nwy03wyd2ks4qxg47py7lm18karjz6vs7p8knmn3zy72i3n9rfi";
      }) // {
      url = "https://github.com/input-output-hk/cardano-base";
      rev = "8c732560b201b5da8e3bdf175c6eda73a32d64bc";
      sha256 = "0nwy03wyd2ks4qxg47py7lm18karjz6vs7p8knmn3zy72i3n9rfi";
      };
    postUnpack = "sourceRoot+=/base-deriving-via; echo source root reset to \$sourceRoot";
    }