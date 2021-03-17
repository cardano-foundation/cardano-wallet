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
      agpl = true;
      pedantic = false;
      all-plugins = false;
      all-formatters = false;
      class = true;
      haddockcomments = true;
      eval = true;
      importlens = true;
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
      specVersion = "2.2";
      identifier = { name = "haskell-language-server"; version = "1.0.0.0"; };
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
        "include/ghc-api-version.h"
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
          (hsPkgs."shake" or (errorHandler.buildDepError "shake"))
          (hsPkgs."safe-exceptions" or (errorHandler.buildDepError "safe-exceptions"))
          (hsPkgs."sqlite-simple" or (errorHandler.buildDepError "sqlite-simple"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
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
          depends = ((((((((((((([
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
            (hsPkgs."shake" or (errorHandler.buildDepError "shake"))
            (hsPkgs."sqlite-simple" or (errorHandler.buildDepError "sqlite-simple"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."with-utf8" or (errorHandler.buildDepError "with-utf8"))
            ] ++ (pkgs.lib).optional (flags.class || flags.all-plugins) (hsPkgs."hls-class-plugin" or (errorHandler.buildDepError "hls-class-plugin"))) ++ (pkgs.lib).optional (flags.haddockcomments || flags.all-plugins) (hsPkgs."hls-haddock-comments-plugin" or (errorHandler.buildDepError "hls-haddock-comments-plugin"))) ++ (pkgs.lib).optional (flags.eval || flags.all-plugins) (hsPkgs."hls-eval-plugin" or (errorHandler.buildDepError "hls-eval-plugin"))) ++ (pkgs.lib).optional (flags.importlens || flags.all-plugins) (hsPkgs."hls-explicit-imports-plugin" or (errorHandler.buildDepError "hls-explicit-imports-plugin"))) ++ (pkgs.lib).optional (flags.retrie || flags.all-plugins) (hsPkgs."hls-retrie-plugin" or (errorHandler.buildDepError "hls-retrie-plugin"))) ++ (pkgs.lib).optional (flags.tactic || flags.all-plugins) (hsPkgs."hls-tactics-plugin" or (errorHandler.buildDepError "hls-tactics-plugin"))) ++ (pkgs.lib).optional (flags.hlint || flags.all-plugins) (hsPkgs."hls-hlint-plugin" or (errorHandler.buildDepError "hls-hlint-plugin"))) ++ (pkgs.lib).optional (flags.pragmas || flags.all-plugins) (hsPkgs."fuzzy" or (errorHandler.buildDepError "fuzzy"))) ++ (pkgs.lib).optional (flags.splice || flags.all-plugins) (hsPkgs."hls-splice-plugin" or (errorHandler.buildDepError "hls-splice-plugin"))) ++ (pkgs.lib).optional (flags.floskell || flags.all-formatters) (hsPkgs."floskell" or (errorHandler.buildDepError "floskell"))) ++ (pkgs.lib).optional (flags.fourmolu || flags.all-formatters) (hsPkgs."fourmolu" or (errorHandler.buildDepError "fourmolu"))) ++ (pkgs.lib).optional (flags.ormolu || flags.all-formatters) (hsPkgs."ormolu" or (errorHandler.buildDepError "ormolu"))) ++ (pkgs.lib).optional (flags.stylishhaskell || flags.all-formatters) (hsPkgs."stylish-haskell" or (errorHandler.buildDepError "stylish-haskell"))) ++ (pkgs.lib).optional ((flags.brittany || flags.all-formatters) && flags.agpl) (hsPkgs."brittany" or (errorHandler.buildDepError "brittany"));
          buildable = true;
          modules = (((((([
            "Ide/Plugin/Example"
            "Ide/Plugin/Example2"
            "Plugins"
            ] ++ (pkgs.lib).optional (flags.modulename || flags.all-plugins) "Ide/Plugin/ModuleName") ++ (pkgs.lib).optional (flags.pragmas || flags.all-plugins) "Ide/Plugin/Pragmas") ++ (pkgs.lib).optional (flags.floskell || flags.all-formatters) "Ide/Plugin/Floskell") ++ (pkgs.lib).optional (flags.fourmolu || flags.all-formatters) "Ide/Plugin/Fourmolu") ++ (pkgs.lib).optional (flags.ormolu || flags.all-formatters) "Ide/Plugin/Ormolu") ++ (pkgs.lib).optional (flags.stylishhaskell || flags.all-formatters) "Ide/Plugin/StylishHaskell") ++ (pkgs.lib).optional ((flags.brittany || flags.all-formatters) && flags.agpl) "Ide/Plugin/Brittany";
          hsSourceDirs = (((((([
            "plugins/default/src"
            "exe"
            ] ++ (pkgs.lib).optional (flags.modulename || flags.all-plugins) "plugins/default/src") ++ (pkgs.lib).optional (flags.pragmas || flags.all-plugins) "plugins/default/src") ++ (pkgs.lib).optional (flags.floskell || flags.all-formatters) "plugins/default/src") ++ (pkgs.lib).optional (flags.fourmolu || flags.all-formatters) "plugins/default/src") ++ (pkgs.lib).optional (flags.ormolu || flags.all-formatters) "plugins/default/src") ++ (pkgs.lib).optional (flags.stylishhaskell || flags.all-formatters) "plugins/default/src") ++ (pkgs.lib).optional ((flags.brittany || flags.all-formatters) && flags.agpl) "plugins/default/src";
          includeDirs = [ "include" ];
          mainPath = (((((((((((((((([
            "Main.hs"
            ] ++ (pkgs.lib).optional (flags.agpl) "") ++ (pkgs.lib).optional (flags.class || flags.all-plugins) "") ++ (pkgs.lib).optional (flags.haddockcomments || flags.all-plugins) "") ++ (pkgs.lib).optional (flags.eval || flags.all-plugins) "") ++ (pkgs.lib).optional (flags.importlens || flags.all-plugins) "") ++ (pkgs.lib).optional (flags.retrie || flags.all-plugins) "") ++ (pkgs.lib).optional (flags.tactic || flags.all-plugins) "") ++ (pkgs.lib).optional (flags.hlint || flags.all-plugins) "") ++ (pkgs.lib).optional (flags.modulename || flags.all-plugins) "") ++ (pkgs.lib).optional (flags.pragmas || flags.all-plugins) "") ++ (pkgs.lib).optional (flags.splice || flags.all-plugins) "") ++ (pkgs.lib).optional (flags.floskell || flags.all-formatters) "") ++ (pkgs.lib).optional (flags.fourmolu || flags.all-formatters) "") ++ (pkgs.lib).optional (flags.ormolu || flags.all-formatters) "") ++ (pkgs.lib).optional (flags.stylishhaskell || flags.all-formatters) "") ++ (pkgs.lib).optional ((flags.brittany || flags.all-formatters) && flags.agpl) "") ++ (pkgs.lib).optional (flags.pedantic) "";
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
          mainPath = ([
            "Wrapper.hs"
            ] ++ (pkgs.lib).optional (flags.agpl) "") ++ (pkgs.lib).optional (flags.pedantic) "";
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
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."blaze-markup" or (errorHandler.buildDepError "blaze-markup"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
            (hsPkgs."lsp" or (errorHandler.buildDepError "lsp"))
            (hsPkgs."hie-bios" or (errorHandler.buildDepError "hie-bios"))
            (hsPkgs."hls-plugin-api" or (errorHandler.buildDepError "hls-plugin-api"))
            (hsPkgs."hslogger" or (errorHandler.buildDepError "hslogger"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."hspec-core" or (errorHandler.buildDepError "hspec-core"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."lsp-test" or (errorHandler.buildDepError "lsp-test"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."tasty-expected-failure" or (errorHandler.buildDepError "tasty-expected-failure"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."hspec-expectations" or (errorHandler.buildDepError "hspec-expectations"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-ant-xml" or (errorHandler.buildDepError "tasty-ant-xml"))
            (hsPkgs."tasty-golden" or (errorHandler.buildDepError "tasty-golden"))
            (hsPkgs."tasty-rerun" or (errorHandler.buildDepError "tasty-rerun"))
            (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."ghcide" or (errorHandler.buildDepError "ghcide"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.haskell-language-server.components.exes.haskell-language-server or (pkgs.buildPackages.haskell-language-server or (errorHandler.buildToolDepError "haskell-language-server:haskell-language-server")))
            (hsPkgs.buildPackages.ghcide.components.exes.ghcide-test-preprocessor or (pkgs.buildPackages.ghcide-test-preprocessor or (errorHandler.buildToolDepError "ghcide:ghcide-test-preprocessor")))
            ];
          buildable = true;
          modules = [
            "Test/Hls/Util"
            "Class"
            "Command"
            "Completion"
            "Config"
            "Deferred"
            "Definition"
            "Diagnostic"
            "Eval"
            "Format"
            "FunctionalBadProject"
            "FunctionalCodeAction"
            "FunctionalLiquid"
            "HieBios"
            "Highlight"
            "ModuleName"
            "Progress"
            "Reference"
            "Rename"
            "Symbol"
            "TypeDefinition"
            "Splice"
            "HaddockComments"
            "Ide/Plugin/Splice/Types"
            "Ide/Plugin/Eval/Types"
            ];
          hsSourceDirs = [
            "test/utils"
            "test/functional"
            "plugins/hls-eval-plugin/test"
            "plugins/hls-splice-plugin/src"
            "plugins/hls-eval-plugin/src"
            ];
          mainPath = [ "Main.hs" ];
          };
        "wrapper-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."blaze-markup" or (errorHandler.buildDepError "blaze-markup"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
            (hsPkgs."lsp" or (errorHandler.buildDepError "lsp"))
            (hsPkgs."hie-bios" or (errorHandler.buildDepError "hie-bios"))
            (hsPkgs."hls-plugin-api" or (errorHandler.buildDepError "hls-plugin-api"))
            (hsPkgs."hslogger" or (errorHandler.buildDepError "hslogger"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."hspec-core" or (errorHandler.buildDepError "hspec-core"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."lsp-test" or (errorHandler.buildDepError "lsp-test"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."tasty-expected-failure" or (errorHandler.buildDepError "tasty-expected-failure"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-ant-xml" or (errorHandler.buildDepError "tasty-ant-xml"))
            (hsPkgs."tasty-rerun" or (errorHandler.buildDepError "tasty-rerun"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.haskell-language-server.components.exes.haskell-language-server-wrapper or (pkgs.buildPackages.haskell-language-server-wrapper or (errorHandler.buildToolDepError "haskell-language-server:haskell-language-server-wrapper")))
            ];
          buildable = true;
          modules = [ "Test/Hls/Util" ];
          hsSourceDirs = [ "test/utils" "test/wrapper" ];
          mainPath = [ "Main.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.; }