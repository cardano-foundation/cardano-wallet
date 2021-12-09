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
    flags = { systemd = true; };
    package = {
      specVersion = "2.4";
      identifier = { name = "cardano-config"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "";
      description = "";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."cardano-prelude" or (errorHandler.buildDepError "cardano-prelude"))
          (hsPkgs."file-embed" or (errorHandler.buildDepError "file-embed"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-config";
      rev = "e9de7a2cf70796f6ff26eac9f9540184ded0e4e6";
      sha256 = "1wm1c99r5zvz22pdl8nhkp13falvqmj8dgkm8fxskwa9ydqz01ld";
      }) // {
      url = "https://github.com/input-output-hk/cardano-config";
      rev = "e9de7a2cf70796f6ff26eac9f9540184ded0e4e6";
      sha256 = "1wm1c99r5zvz22pdl8nhkp13falvqmj8dgkm8fxskwa9ydqz01ld";
      };
    }