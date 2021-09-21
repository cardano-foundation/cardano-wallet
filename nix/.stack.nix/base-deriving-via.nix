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
      rev = "592aa61d657ad5935a33bace1243abce3728b643";
      sha256 = "1bgq3a2wfdz24jqfwylcc6jjg5aji8dpy5gjkhpnmkkvgcr2rkyb";
      }) // {
      url = "https://github.com/input-output-hk/cardano-base";
      rev = "592aa61d657ad5935a33bace1243abce3728b643";
      sha256 = "1bgq3a2wfdz24jqfwylcc6jjg5aji8dpy5gjkhpnmkkvgcr2rkyb";
      };
    postUnpack = "sourceRoot+=/base-deriving-via; echo source root reset to \$sourceRoot";
    }