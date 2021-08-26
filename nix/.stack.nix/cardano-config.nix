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
      specVersion = "3.0";
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
      url = "https://github.com/input-output-hk/cardano-node";
      rev = "4c59442958072657812c6c0bb8e0b4ab85ce1ba2";
      sha256 = "0pc26hrgdsf93h3qvp8m76axm4jspzclg6psn14mbaf1mkc1fxmx";
      }) // {
      url = "https://github.com/input-output-hk/cardano-node";
      rev = "4c59442958072657812c6c0bb8e0b4ab85ce1ba2";
      sha256 = "0pc26hrgdsf93h3qvp8m76axm4jspzclg6psn14mbaf1mkc1fxmx";
      };
    postUnpack = "sourceRoot+=/cardano-config; echo source root reset to \$sourceRoot";
    }