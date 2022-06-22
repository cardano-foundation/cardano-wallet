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
      ignore-plugins-ghc-bounds = false;
      class = true;
      callhierarchy = true;
      haddockcomments = true;
      eval = true;
      importlens = true;
      refineimports = true;
      rename = true;
      retrie = true;
      tactic = true;
      hlint = true;
      modulename = true;
      pragmas = true;
      splice = true;
      alternatenumberformat = true;
      qualifyimportednames = true;
      selectionrange = true;
      changetypesignature = true;
      floskell = true;
      fourmolu = true;
      ormolu = true;
      stylishhaskell = true;
      brittany = true;
      dynamic = true;
      };
    package = {
      specVersion = "2.4";
      identifier = { name = "haskell-language-server"; version = "1.7.0.0"; };
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
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."cryptohash-sha1" or (errorHandler.buildDepError "cryptohash-sha1"))
          (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
          (hsPkgs."ghc" or (errorHandler.buildDepError "ghc"))
          (hsPkgs."ghcide" or (errorHandler.buildDepError "ghcide"))
          (hsPkgs."githash" or (errorHandler.buildDepError "githash"))
          (hsPkgs."lsp" or (errorHandler.buildDepError "lsp"))
          (hsPkgs."hie-bios" or (errorHandler.buildDepError "hie-bios"))
          (hsPkgs."hiedb" or (errorHandler.buildDepError "hiedb"))
          (hsPkgs."hls-plugin-api" or (errorHandler.buildDepError "hls-plugin-api"))
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
          depends = ((((((((((((((((((((([
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."cryptohash-sha1" or (errorHandler.buildDepError "cryptohash-sha1"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."ghc" or (errorHandler.buildDepError "ghc"))
            (hsPkgs."ghc-boot-th" or (errorHandler.buildDepError "ghc-boot-th"))
            (hsPkgs."ghcide" or (errorHandler.buildDepError "ghcide"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."haskell-language-server" or (errorHandler.buildDepError "haskell-language-server"))
            (hsPkgs."lsp" or (errorHandler.buildDepError "lsp"))
            (hsPkgs."hie-bios" or (errorHandler.buildDepError "hie-bios"))
            (hsPkgs."hiedb" or (errorHandler.buildDepError "hiedb"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."regex-tdfa" or (errorHandler.buildDepError "regex-tdfa"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."hls-plugin-api" or (errorHandler.buildDepError "hls-plugin-api"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."safe-exceptions" or (errorHandler.buildDepError "safe-exceptions"))
            (hsPkgs."hls-graph" or (errorHandler.buildDepError "hls-graph"))
            (hsPkgs."sqlite-simple" or (errorHandler.buildDepError "sqlite-simple"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            ] ++ (pkgs.lib).optional (flags.callhierarchy) (hsPkgs."hls-call-hierarchy-plugin" or (errorHandler.buildDepError "hls-call-hierarchy-plugin"))) ++ (pkgs.lib).optional (flags.changetypesignature) (hsPkgs."hls-change-type-signature-plugin" or (errorHandler.buildDepError "hls-change-type-signature-plugin"))) ++ (pkgs.lib).optional (flags.class && (compiler.isGhc && (compiler.version).lt "9.2.1" || flags.ignore-plugins-ghc-bounds)) (hsPkgs."hls-class-plugin" or (errorHandler.buildDepError "hls-class-plugin"))) ++ (pkgs.lib).optional (flags.haddockcomments) (hsPkgs."hls-haddock-comments-plugin" or (errorHandler.buildDepError "hls-haddock-comments-plugin"))) ++ (pkgs.lib).optional (flags.eval) (hsPkgs."hls-eval-plugin" or (errorHandler.buildDepError "hls-eval-plugin"))) ++ (pkgs.lib).optional (flags.importlens) (hsPkgs."hls-explicit-imports-plugin" or (errorHandler.buildDepError "hls-explicit-imports-plugin"))) ++ (pkgs.lib).optional (flags.refineimports) (hsPkgs."hls-refine-imports-plugin" or (errorHandler.buildDepError "hls-refine-imports-plugin"))) ++ (pkgs.lib).optional (flags.rename) (hsPkgs."hls-rename-plugin" or (errorHandler.buildDepError "hls-rename-plugin"))) ++ (pkgs.lib).optional (flags.retrie && (compiler.isGhc && (compiler.version).lt "9.2.1" || flags.ignore-plugins-ghc-bounds)) (hsPkgs."hls-retrie-plugin" or (errorHandler.buildDepError "hls-retrie-plugin"))) ++ (pkgs.lib).optional (flags.tactic && (compiler.isGhc && (compiler.version).lt "9.0.1" || flags.ignore-plugins-ghc-bounds)) (hsPkgs."hls-tactics-plugin" or (errorHandler.buildDepError "hls-tactics-plugin"))) ++ (pkgs.lib).optional (flags.hlint && (compiler.isGhc && (compiler.version).lt "9.2.1" || flags.ignore-plugins-ghc-bounds)) (hsPkgs."hls-hlint-plugin" or (errorHandler.buildDepError "hls-hlint-plugin"))) ++ (pkgs.lib).optional (flags.modulename) (hsPkgs."hls-module-name-plugin" or (errorHandler.buildDepError "hls-module-name-plugin"))) ++ (pkgs.lib).optional (flags.pragmas) (hsPkgs."hls-pragmas-plugin" or (errorHandler.buildDepError "hls-pragmas-plugin"))) ++ (pkgs.lib).optional (flags.splice && (compiler.isGhc && (compiler.version).lt "9.2.1" || flags.ignore-plugins-ghc-bounds)) (hsPkgs."hls-splice-plugin" or (errorHandler.buildDepError "hls-splice-plugin"))) ++ (pkgs.lib).optional (flags.alternatenumberformat) (hsPkgs."hls-alternate-number-format-plugin" or (errorHandler.buildDepError "hls-alternate-number-format-plugin"))) ++ (pkgs.lib).optional (flags.qualifyimportednames) (hsPkgs."hls-qualify-imported-names-plugin" or (errorHandler.buildDepError "hls-qualify-imported-names-plugin"))) ++ (pkgs.lib).optional (flags.selectionrange) (hsPkgs."hls-selection-range-plugin" or (errorHandler.buildDepError "hls-selection-range-plugin"))) ++ (pkgs.lib).optional (flags.floskell) (hsPkgs."hls-floskell-plugin" or (errorHandler.buildDepError "hls-floskell-plugin"))) ++ (pkgs.lib).optional (flags.fourmolu) (hsPkgs."hls-fourmolu-plugin" or (errorHandler.buildDepError "hls-fourmolu-plugin"))) ++ (pkgs.lib).optional (flags.ormolu) (hsPkgs."hls-ormolu-plugin" or (errorHandler.buildDepError "hls-ormolu-plugin"))) ++ (pkgs.lib).optional (flags.stylishhaskell && (compiler.isGhc && (compiler.version).lt "9.0.1" || flags.ignore-plugins-ghc-bounds)) (hsPkgs."hls-stylish-haskell-plugin" or (errorHandler.buildDepError "hls-stylish-haskell-plugin"))) ++ (pkgs.lib).optional (flags.brittany && (compiler.isGhc && (compiler.version).lt "9.0.2" || flags.ignore-plugins-ghc-bounds)) (hsPkgs."hls-brittany-plugin" or (errorHandler.buildDepError "hls-brittany-plugin"));
          buildable = true;
          modules = [ "Ide/Plugin/Example" "Ide/Plugin/Example2" "Plugins" ];
          hsSourceDirs = [ "plugins/default/src" "exe" ];
          mainPath = (((((((((((((((((((((((([
            "Main.hs"
            ] ++ (pkgs.lib).optional (flags.pedantic) "") ++ (pkgs.lib).optional (flags.callhierarchy) "") ++ (pkgs.lib).optional (flags.changetypesignature) "") ++ (pkgs.lib).optional (flags.class && (compiler.isGhc && (compiler.version).lt "9.2.1" || flags.ignore-plugins-ghc-bounds)) "") ++ (pkgs.lib).optional (flags.haddockcomments) "") ++ (pkgs.lib).optional (flags.eval) "") ++ (pkgs.lib).optional (flags.importlens) "") ++ (pkgs.lib).optional (flags.refineimports) "") ++ (pkgs.lib).optional (flags.rename) "") ++ (pkgs.lib).optional (flags.retrie && (compiler.isGhc && (compiler.version).lt "9.2.1" || flags.ignore-plugins-ghc-bounds)) "") ++ (pkgs.lib).optional (flags.tactic && (compiler.isGhc && (compiler.version).lt "9.0.1" || flags.ignore-plugins-ghc-bounds)) "") ++ (pkgs.lib).optional (flags.hlint && (compiler.isGhc && (compiler.version).lt "9.2.1" || flags.ignore-plugins-ghc-bounds)) "") ++ (pkgs.lib).optional (flags.modulename) "") ++ (pkgs.lib).optional (flags.pragmas) "") ++ (pkgs.lib).optional (flags.splice && (compiler.isGhc && (compiler.version).lt "9.2.1" || flags.ignore-plugins-ghc-bounds)) "") ++ (pkgs.lib).optional (flags.alternatenumberformat) "") ++ (pkgs.lib).optional (flags.qualifyimportednames) "") ++ (pkgs.lib).optional (flags.selectionrange) "") ++ (pkgs.lib).optional (flags.floskell) "") ++ (pkgs.lib).optional (flags.fourmolu) "") ++ (pkgs.lib).optional (flags.ormolu) "") ++ (pkgs.lib).optional (flags.stylishhaskell && (compiler.isGhc && (compiler.version).lt "9.0.1" || flags.ignore-plugins-ghc-bounds)) "") ++ (pkgs.lib).optional (flags.brittany && (compiler.isGhc && (compiler.version).lt "9.0.2" || flags.ignore-plugins-ghc-bounds)) "") ++ (pkgs.lib).optional (flags.pedantic) "") ++ (pkgs.lib).optional (!system.isWindows && flags.dynamic) "";
          };
        "haskell-language-server-wrapper" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
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
            ] ++ (pkgs.lib).optionals (!system.isWindows) [
            (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            ];
          buildable = true;
          modules = [ "Paths_haskell_language_server" ];
          hsSourceDirs = [ "exe" ];
          mainPath = ([
            "Wrapper.hs"
            ] ++ (pkgs.lib).optional (flags.pedantic) "") ++ (pkgs.lib).optional (!system.isWindows) "";
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
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
            (hsPkgs."hspec-expectations" or (errorHandler.buildDepError "hspec-expectations"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."lens-aeson" or (errorHandler.buildDepError "lens-aeson"))
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
            "HieBios"
            "Highlight"
            "Progress"
            "Reference"
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
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."hls-test-utils" or (errorHandler.buildDepError "hls-test-utils"))
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