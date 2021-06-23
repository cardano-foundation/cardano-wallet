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
      specVersion = "1.22";
      identifier = { name = "ghc-api-compat"; version = "8.6.1"; };
      license = "BSD-3-Clause";
      copyright = "Sylvain Henry 2020";
      maintainer = "sylvain@haskus.fr";
      author = "Sylvain Henry";
      homepage = "";
      url = "";
      synopsis = "GHC-API compatibility helpers";
      description = "GHC codebase is a moving target. This package provides\ncompatibility for codes relying on an older GHC-API version.\nSee https://gitlab.haskell.org/ghc/ghc/-/wikis/Make-GHC-codebase-more-modular for the migration plan.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."ghc" or (errorHandler.buildDepError "ghc"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ];
        buildable = true;
        modules = (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "9.0") "Outputable";
        hsSourceDirs = (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "9.0") "src";
        };
      };
    } // rec { src = (pkgs.lib).mkDefault .././.source-repository-packages/0; }