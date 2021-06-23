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
    flags = {
      pedantic = false;
      all-plugins = false;
      all-formatters = false;
      class = true;
      haddockcomments = true;
      eval = true;
      importlens = true;
      refineimports = true;
      retrie = true;
      tactic = true;
      hlint = true;
      modulename = true;
      pragmas = true;
      splice = true;
      floskell = true;
      fourmolu = true;
      ormolu = true;
      stylishhaskell = true;
      brittany = true;
      };
    package = {
      specVersion = "2.4";
      identifier = { name = "haskell-language-server"; version = "1.2.0.0"; };
      license = "Apache-2.0";
      copyright = "The Haskell IDE Team";
      maintainer = "alan.zimm@gmail.com";
      author = "The Haskell IDE Team";
      homepage = "https://github.com/haskell/haskell-language-server#readme";
      url = "";
      synopsis = "LSP server for GHC";
      description = "Please see the README on GitHub at <https://github.com/haskell/haskell-language-server#readme>";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [
        "README.md"
        "ChangeLog.md"
        "test/testdata/**/*.project"
        "test/testdata/**/*.cabal"
        "test/testdata/**/*.yaml"
        "test/testdata/hlint/ignore/.hlint.yaml"
        "test/testdata/**/*.h"
        "test/testdata/**/*.hs"
        ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."cryptohash-sha1" or (errorHandler.buildDepError "cryptohash-sha1"))
          (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
          (hsPkgs."ghc" or (errorHandler.buildDepError "ghc"))
          (hsPkgs."ghcide" or (errorHandler.buildDepError "ghcide"))
          (hsPkgs."gitrev" or (errorHandler.buildDepError "gitrev"))
          (hsPkgs."lsp" or (errorHandler.buildDepError "lsp"))
          (hsPkgs."hie-bios" or (errorHandler.buildDepError "hie-bios"))
          (hsPkgs."hiedb" or (errorHandler.buildDepError "hiedb"))
          (hsPkgs."hls-plugin-api" or (errorHandler.buildDepError "hls-plugin-api"))
          (hsPkgs."hslogger" or (errorHandler.buildDepError "hslogger"))
          (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
          (hsPkgs."optparse-simple" or (errorHandler.buildDepError "optparse-simple"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."hls-graph" or (errorHandler.buildDepError "hls-graph"))
          (hsPkgs."safe-exceptions" or (errorHandler.buildDepError "safe-exceptions"))
          (hsPkgs."sqlite-simple" or (errorHandler.buildDepError "sqlite-simple"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."aeson-pretty" or (errorHandler.buildDepError "aeson-pretty"))
          ];
        buildable = true;
        modules = [
          "Paths_haskell_language_server"
          "Ide/Arguments"
          "Ide/Main"
          "Ide/Version"
          ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "haskell-language-server" = {
          depends = ((((((((((((((([
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."cryptohash-sha1" or (errorHandler.buildDepError "cryptohash-sha1"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."ghc" or (errorHandler.buildDepError "ghc"))
            (hsPkgs."ghc-api-compat" or (errorHandler.buildDepError "ghc-api-compat"))
            (hsPkgs."ghc-boot-th" or (errorHandler.buildDepError "ghc-boot-th"))
            (hsPkgs."ghcide" or (errorHandler.buildDepError "ghcide"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."haskell-language-server" or (errorHandler.buildDepError "haskell-language-server"))
            (hsPkgs."lsp" or (errorHandler.buildDepError "lsp"))
            (hsPkgs."hie-bios" or (errorHandler.buildDepError "hie-bios"))
            (hsPkgs."hiedb" or (errorHandler.buildDepError "hiedb"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."regex-tdfa" or (errorHandler.buildDepError "regex-tdfa"))
            (hsPkgs."hslogger" or (errorHandler.buildDepError "hslogger"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."hls-plugin-api" or (errorHandler.buildDepError "hls-plugin-api"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."safe-exceptions" or (errorHandler.buildDepError "safe-exceptions"))
            (hsPkgs."hls-graph" or (errorHandler.buildDepError "hls-graph"))
            (hsPkgs."sqlite-simple" or (errorHandler.buildDepError "sqlite-simple"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            ] ++ (pkgs.lib).optional (flags.class || flags.all-plugins) (hsPkgs."hls-class-plugin" or (errorHandler.buildDepError "hls-class-plugin"))) ++ (pkgs.lib).optional (flags.haddockcomments || flags.all-plugins) (hsPkgs."hls-haddock-comments-plugin" or (errorHandler.buildDepError "hls-haddock-comments-plugin"))) ++ (pkgs.lib).optional (flags.eval || flags.all-plugins) (hsPkgs."hls-eval-plugin" or (errorHandler.buildDepError "hls-eval-plugin"))) ++ (pkgs.lib).optional (flags.importlens || flags.all-plugins) (hsPkgs."hls-explicit-imports-plugin" or (errorHandler.buildDepError "hls-explicit-imports-plugin"))) ++ (pkgs.lib).optional (flags.refineimports || flags.all-plugins) (hsPkgs."hls-refine-imports-plugin" or (errorHandler.buildDepError "hls-refine-imports-plugin"))) ++ (pkgs.lib).optional (flags.retrie || flags.all-plugins) (hsPkgs."hls-retrie-plugin" or (errorHandler.buildDepError "hls-retrie-plugin"))) ++ (pkgs.lib).optional (flags.tactic || flags.all-plugins) (hsPkgs."hls-tactics-plugin" or (errorHandler.buildDepError "hls-tactics-plugin"))) ++ (pkgs.lib).optional (flags.hlint || flags.all-plugins) (hsPkgs."hls-hlint-plugin" or (errorHandler.buildDepError "hls-hlint-plugin"))) ++ (pkgs.lib).optional (flags.modulename || flags.all-plugins) (hsPkgs."hls-module-name-plugin" or (errorHandler.buildDepError "hls-module-name-plugin"))) ++ (pkgs.lib).optional (flags.pragmas || flags.all-plugins) (hsPkgs."hls-pragmas-plugin" or (errorHandler.buildDepError "hls-pragmas-plugin"))) ++ (pkgs.lib).optional (flags.splice || flags.all-plugins) (hsPkgs."hls-splice-plugin" or (errorHandler.buildDepError "hls-splice-plugin"))) ++ (pkgs.lib).optional (flags.floskell || flags.all-formatters) (hsPkgs."hls-floskell-plugin" or (errorHandler.buildDepError "hls-floskell-plugin"))) ++ (pkgs.lib).optional (flags.fourmolu || flags.all-formatters) (hsPkgs."hls-fourmolu-plugin" or (errorHandler.buildDepError "hls-fourmolu-plugin"))) ++ (pkgs.lib).optional (flags.ormolu || flags.all-formatters) (hsPkgs."hls-ormolu-plugin" or (errorHandler.buildDepError "hls-ormolu-plugin"))) ++ (pkgs.lib).optional (flags.stylishhaskell || flags.all-formatters) (hsPkgs."hls-stylish-haskell-plugin" or (errorHandler.buildDepError "hls-stylish-haskell-plugin"))) ++ (pkgs.lib).optional (flags.brittany || flags.all-formatters) (hsPkgs."hls-brittany-plugin" or (errorHandler.buildDepError "hls-brittany-plugin"));
          buildable = true;
          modules = [ "Ide/Plugin/Example" "Ide/Plugin/Example2" "Plugins" ];
          hsSourceDirs = [ "plugins/default/src" "exe" ];
          mainPath = (((((((((((((((([
            "Main.hs"
            ] ++ (pkgs.lib).optional (flags.class || flags.all-plugins) "") ++ (pkgs.lib).optional (flags.haddockcomments || flags.all-plugins) "") ++ (pkgs.lib).optional (flags.eval || flags.all-plugins) "") ++ (pkgs.lib).optional (flags.importlens || flags.all-plugins) "") ++ (pkgs.lib).optional (flags.refineimports || flags.all-plugins) "") ++ (pkgs.lib).optional (flags.retrie || flags.all-plugins) "") ++ (pkgs.lib).optional (flags.tactic || flags.all-plugins) "") ++ (pkgs.lib).optional (flags.hlint || flags.all-plugins) "") ++ (pkgs.lib).optional (flags.modulename || flags.all-plugins) "") ++ (pkgs.lib).optional (flags.pragmas || flags.all-plugins) "") ++ (pkgs.lib).optional (flags.splice || flags.all-plugins) "") ++ (pkgs.lib).optional (flags.floskell || flags.all-formatters) "") ++ (pkgs.lib).optional (flags.fourmolu || flags.all-formatters) "") ++ (pkgs.lib).optional (flags.ormolu || flags.all-formatters) "") ++ (pkgs.lib).optional (flags.stylishhaskell || flags.all-formatters) "") ++ (pkgs.lib).optional (flags.brittany || flags.all-formatters) "") ++ (pkgs.lib).optional (flags.pedantic) "";
          };
        "haskell-language-server-wrapper" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
            (hsPkgs."ghc" or (errorHandler.buildDepError "ghc"))
            (hsPkgs."ghc-paths" or (errorHandler.buildDepError "ghc-paths"))
            (hsPkgs."ghcide" or (errorHandler.buildDepError "ghcide"))
            (hsPkgs."gitrev" or (errorHandler.buildDepError "gitrev"))
            (hsPkgs."haskell-language-server" or (errorHandler.buildDepError "haskell-language-server"))
            (hsPkgs."hie-bios" or (errorHandler.buildDepError "hie-bios"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."optparse-simple" or (errorHandler.buildDepError "optparse-simple"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            ];
          buildable = true;
          modules = [ "Paths_haskell_language_server" ];
          hsSourceDirs = [ "exe" ];
          mainPath = [
            "Wrapper.hs"
            ] ++ (pkgs.lib).optional (flags.pedantic) "";
          };
        };
      tests = {
        "func-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
            (hsPkgs."hspec-expectations" or (errorHandler.buildDepError "hspec-expectations"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."ghcide" or (errorHandler.buildDepError "ghcide"))
            (hsPkgs."hls-test-utils" or (errorHandler.buildDepError "hls-test-utils"))
            (hsPkgs."lsp-types" or (errorHandler.buildDepError "lsp-types"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."hls-plugin-api" or (errorHandler.buildDepError "hls-plugin-api"))
            (hsPkgs."lsp-test" or (errorHandler.buildDepError "lsp-test"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.haskell-language-server.components.exes.haskell-language-server or (pkgs.buildPackages.haskell-language-server or (errorHandler.buildToolDepError "haskell-language-server:haskell-language-server")))
            (hsPkgs.buildPackages.ghcide.components.exes.ghcide-test-preprocessor or (pkgs.buildPackages.ghcide-test-preprocessor or (errorHandler.buildToolDepError "ghcide:ghcide-test-preprocessor")))
            ];
          buildable = true;
          modules = [
            "Command"
            "Completion"
            "Config"
            "Deferred"
            "Definition"
            "Diagnostic"
            "Format"
            "FunctionalBadProject"
            "FunctionalCodeAction"
            "FunctionalLiquid"
            "HieBios"
            "Highlight"
            "Progress"
            "Reference"
            "Rename"
            "Symbol"
            "TypeDefinition"
            "Test/Hls/Command"
            "Test/Hls/Flags"
            ];
          hsSourceDirs = [ "test/functional" "test/utils" ];
          mainPath = [ "Main.hs" ];
          };
        "wrapper-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."hls-test-utils" or (errorHandler.buildDepError "hls-test-utils"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.haskell-language-server.components.exes.haskell-language-server-wrapper or (pkgs.buildPackages.haskell-language-server-wrapper or (errorHandler.buildToolDepError "haskell-language-server:haskell-language-server-wrapper")))
            (hsPkgs.buildPackages.haskell-language-server.components.exes.haskell-language-server or (pkgs.buildPackages.haskell-language-server or (errorHandler.buildToolDepError "haskell-language-server:haskell-language-server")))
            ];
          buildable = true;
          hsSourceDirs = [ "test/wrapper" ];
          mainPath = [ "Main.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.; }