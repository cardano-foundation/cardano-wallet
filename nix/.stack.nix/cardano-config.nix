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
      rev = "cc78734d263d0eec2b12070380cdfea02a5a8342";
      sha256 = "1bqrbgc11c4x5mrcm7shyvfs05fqi4v1pr4y9pw2aprab277czjr";
      }) // {
      url = "https://github.com/input-output-hk/cardano-node";
      rev = "cc78734d263d0eec2b12070380cdfea02a5a8342";
      sha256 = "1bqrbgc11c4x5mrcm7shyvfs05fqi4v1pr4y9pw2aprab277czjr";
      };
    postUnpack = "sourceRoot+=/cardano-config; echo source root reset to \$sourceRoot";
    }