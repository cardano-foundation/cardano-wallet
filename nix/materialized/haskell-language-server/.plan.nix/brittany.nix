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
    flags = { brittany-dev-lib = false; brittany-test-perf = false; };
    package = {
      specVersion = "1.18";
      identifier = { name = "brittany"; version = "0.12.1.1"; };
      license = "AGPL-3.0-only";
      copyright = "Copyright (C) 2016-2019 Lennart Spitzner\nCopyright (C) 2019 PRODA LTD";
      maintainer = "Lennart Spitzner <hexagoxel@hexagoxel.de>";
      author = "Lennart Spitzner";
      homepage = "https://github.com/lspitzner/brittany/";
      url = "";
      synopsis = "Haskell source code formatter";
      description = "See <https://github.com/lspitzner/brittany/blob/master/README.md the README>.\n\nIf you are interested in the implementation, have a look at <https://github.com/lspitzner/brittany/blob/master/doc/implementation/theory.md this document>;\n\nThe implementation is documented in more detail <https://github.com/lspitzner/brittany/blob/master/doc/implementation/index.md here>.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = "";
      dataFiles = [];
      extraSrcFiles = [ "src-literatetests/*.blt" "srcinc/prelude.inc" ];
      extraTmpFiles = [];
      extraDocFiles = [ "ChangeLog.md" "README.md" "doc/implementation/*.md" ];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."ghc" or (errorHandler.buildDepError "ghc"))
          (hsPkgs."ghc-paths" or (errorHandler.buildDepError "ghc-paths"))
          (hsPkgs."ghc-exactprint" or (errorHandler.buildDepError "ghc-exactprint"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."multistate" or (errorHandler.buildDepError "multistate"))
          (hsPkgs."syb" or (errorHandler.buildDepError "syb"))
          (hsPkgs."data-tree-print" or (errorHandler.buildDepError "data-tree-print"))
          (hsPkgs."pretty" or (errorHandler.buildDepError "pretty"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."butcher" or (errorHandler.buildDepError "butcher"))
          (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
          (hsPkgs."uniplate" or (errorHandler.buildDepError "uniplate"))
          (hsPkgs."strict" or (errorHandler.buildDepError "strict"))
          (hsPkgs."monad-memo" or (errorHandler.buildDepError "monad-memo"))
          (hsPkgs."unsafe" or (errorHandler.buildDepError "unsafe"))
          (hsPkgs."safe" or (errorHandler.buildDepError "safe"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          (hsPkgs."cmdargs" or (errorHandler.buildDepError "cmdargs"))
          (hsPkgs."czipwith" or (errorHandler.buildDepError "czipwith"))
          (hsPkgs."ghc-boot-th" or (errorHandler.buildDepError "ghc-boot-th"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          ];
        buildable = true;
        modules = [
          "Language/Haskell/Brittany/Internal/LayouterBasics"
          "Language/Haskell/Brittany/Internal/Backend"
          "Language/Haskell/Brittany/Internal/BackendUtils"
          "Language/Haskell/Brittany/Internal/ExactPrintUtils"
          "Language/Haskell/Brittany/Internal/Layouters/Type"
          "Language/Haskell/Brittany/Internal/Layouters/Decl"
          "Language/Haskell/Brittany/Internal/Layouters/Expr"
          "Language/Haskell/Brittany/Internal/Layouters/Stmt"
          "Language/Haskell/Brittany/Internal/Layouters/Pattern"
          "Language/Haskell/Brittany/Internal/Layouters/IE"
          "Language/Haskell/Brittany/Internal/Layouters/Import"
          "Language/Haskell/Brittany/Internal/Layouters/Module"
          "Language/Haskell/Brittany/Internal/Layouters/DataDecl"
          "Language/Haskell/Brittany/Internal/Transformations/Alt"
          "Language/Haskell/Brittany/Internal/Transformations/Floating"
          "Language/Haskell/Brittany/Internal/Transformations/Par"
          "Language/Haskell/Brittany/Internal/Transformations/Columns"
          "Language/Haskell/Brittany/Internal/Transformations/Indent"
          "Language/Haskell/Brittany"
          "Language/Haskell/Brittany/Main"
          "Language/Haskell/Brittany/Internal"
          "Language/Haskell/Brittany/Internal/Prelude"
          "Language/Haskell/Brittany/Internal/PreludeUtils"
          "Language/Haskell/Brittany/Internal/Types"
          "Language/Haskell/Brittany/Internal/Utils"
          "Language/Haskell/Brittany/Internal/Config"
          "Language/Haskell/Brittany/Internal/Config/Types"
          "Language/Haskell/Brittany/Internal/Config/Types/Instances"
          "Language/Haskell/Brittany/Internal/Obfuscation"
          "Paths_brittany"
          ];
        hsSourceDirs = [ "src" ];
        includeDirs = [ "srcinc" ];
        };
      exes = {
        "brittany" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."brittany" or (errorHandler.buildDepError "brittany"))
            ];
          buildable = if flags.brittany-dev-lib then false else true;
          hsSourceDirs = [ "src-brittany" ];
          mainPath = [ "Main.hs" ] ++ [ "" ];
          };
        };
      tests = {
        "unittests" = {
          depends = [
            (hsPkgs."brittany" or (errorHandler.buildDepError "brittany"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."ghc" or (errorHandler.buildDepError "ghc"))
            (hsPkgs."ghc-paths" or (errorHandler.buildDepError "ghc-paths"))
            (hsPkgs."ghc-exactprint" or (errorHandler.buildDepError "ghc-exactprint"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."multistate" or (errorHandler.buildDepError "multistate"))
            (hsPkgs."syb" or (errorHandler.buildDepError "syb"))
            (hsPkgs."data-tree-print" or (errorHandler.buildDepError "data-tree-print"))
            (hsPkgs."pretty" or (errorHandler.buildDepError "pretty"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."butcher" or (errorHandler.buildDepError "butcher"))
            (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."uniplate" or (errorHandler.buildDepError "uniplate"))
            (hsPkgs."strict" or (errorHandler.buildDepError "strict"))
            (hsPkgs."monad-memo" or (errorHandler.buildDepError "monad-memo"))
            (hsPkgs."unsafe" or (errorHandler.buildDepError "unsafe"))
            (hsPkgs."safe" or (errorHandler.buildDepError "safe"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            (hsPkgs."cmdargs" or (errorHandler.buildDepError "cmdargs"))
            (hsPkgs."czipwith" or (errorHandler.buildDepError "czipwith"))
            (hsPkgs."ghc-boot-th" or (errorHandler.buildDepError "ghc-boot-th"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            ];
          buildable = if flags.brittany-dev-lib || !flags.brittany-test-perf
            then false
            else true;
          modules = [ "TestUtils" "AsymptoticPerfTests" ];
          hsSourceDirs = [ "src-unittests" ];
          includeDirs = [ "srcinc" ];
          mainPath = [ "TestMain.hs" ];
          };
        "littests" = {
          depends = [
            (hsPkgs."brittany" or (errorHandler.buildDepError "brittany"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."ghc" or (errorHandler.buildDepError "ghc"))
            (hsPkgs."ghc-paths" or (errorHandler.buildDepError "ghc-paths"))
            (hsPkgs."ghc-exactprint" or (errorHandler.buildDepError "ghc-exactprint"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."multistate" or (errorHandler.buildDepError "multistate"))
            (hsPkgs."syb" or (errorHandler.buildDepError "syb"))
            (hsPkgs."data-tree-print" or (errorHandler.buildDepError "data-tree-print"))
            (hsPkgs."pretty" or (errorHandler.buildDepError "pretty"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."butcher" or (errorHandler.buildDepError "butcher"))
            (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."uniplate" or (errorHandler.buildDepError "uniplate"))
            (hsPkgs."strict" or (errorHandler.buildDepError "strict"))
            (hsPkgs."monad-memo" or (errorHandler.buildDepError "monad-memo"))
            (hsPkgs."unsafe" or (errorHandler.buildDepError "unsafe"))
            (hsPkgs."safe" or (errorHandler.buildDepError "safe"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            (hsPkgs."cmdargs" or (errorHandler.buildDepError "cmdargs"))
            (hsPkgs."czipwith" or (errorHandler.buildDepError "czipwith"))
            (hsPkgs."ghc-boot-th" or (errorHandler.buildDepError "ghc-boot-th"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
            ];
          buildable = if flags.brittany-dev-lib then false else true;
          hsSourceDirs = [ "src-literatetests" ];
          includeDirs = [ "srcinc" ];
          mainPath = [ "Main.hs" ];
          };
        "libinterfacetests" = {
          depends = [
            (hsPkgs."brittany" or (errorHandler.buildDepError "brittany"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            ];
          buildable = if flags.brittany-dev-lib then false else true;
          hsSourceDirs = [ "src-libinterfacetests" ];
          includeDirs = [ "srcinc" ];
          mainPath = [ "Main.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault .././.source-repository-packages/0; }