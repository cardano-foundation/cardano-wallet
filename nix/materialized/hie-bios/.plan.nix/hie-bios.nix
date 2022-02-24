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
      specVersion = "2.2";
      identifier = { name = "hie-bios"; version = "0.9.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Matthew Pickering <matthewtpickering@gmail.com>";
      author = "Matthew Pickering <matthewtpickering@gmail.com>";
      homepage = "https://github.com/mpickering/hie-bios";
      url = "";
      synopsis = "Set up a GHC API session";
      description = "Set up a GHC API session and obtain flags required to compile a source file";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [
        "ChangeLog.md"
        "README.md"
        "wrappers/cabal"
        "wrappers/cabal.hs"
        "tests/configs/*.yaml"
        "tests/projects/cabal-with-ghc/cabal-with-ghc.cabal"
        "tests/projects/cabal-with-ghc/cabal.project"
        "tests/projects/cabal-with-ghc/hie.yaml"
        "tests/projects/cabal-with-ghc/src/MyLib.hs"
        "tests/projects/symlink-test/a/A.hs"
        "tests/projects/symlink-test/hie.yaml"
        "tests/projects/deps-bios-new/A.hs"
        "tests/projects/deps-bios-new/B.hs"
        "tests/projects/deps-bios-new/hie-bios.sh"
        "tests/projects/deps-bios-new/hie.yaml"
        "tests/projects/multi-direct/A.hs"
        "tests/projects/multi-direct/B.hs"
        "tests/projects/multi-direct/hie.yaml"
        "tests/projects/multi-cabal/app/Main.hs"
        "tests/projects/multi-cabal/cabal.project"
        "tests/projects/multi-cabal/hie.yaml"
        "tests/projects/multi-cabal/multi-cabal.cabal"
        "tests/projects/multi-cabal/src/Lib.hs"
        "tests/projects/monorepo-cabal/cabal.project"
        "tests/projects/monorepo-cabal/hie.yaml"
        "tests/projects/monorepo-cabal/A/Main.hs"
        "tests/projects/monorepo-cabal/A/A.cabal"
        "tests/projects/monorepo-cabal/B/MyLib.hs"
        "tests/projects/monorepo-cabal/B/B.cabal"
        "tests/projects/multi-stack/app/Main.hs"
        "tests/projects/multi-stack/cabal.project"
        "tests/projects/multi-stack/hie.yaml"
        "tests/projects/multi-stack/multi-stack.cabal"
        "tests/projects/multi-stack/src/Lib.hs"
        "tests/projects/failing-bios/A.hs"
        "tests/projects/failing-bios/B.hs"
        "tests/projects/failing-bios/hie.yaml"
        "tests/projects/failing-bios-ghc/A.hs"
        "tests/projects/failing-bios-ghc/B.hs"
        "tests/projects/failing-bios-ghc/hie.yaml"
        "tests/projects/failing-cabal/failing-cabal.cabal"
        "tests/projects/failing-cabal/hie.yaml"
        "tests/projects/failing-cabal/MyLib.hs"
        "tests/projects/failing-stack/failing-stack.cabal"
        "tests/projects/failing-stack/hie.yaml"
        "tests/projects/failing-stack/src/Lib.hs"
        "tests/projects/nested-cabal/nested-cabal.cabal"
        "tests/projects/nested-cabal/cabal.project"
        "tests/projects/nested-cabal/hie.yaml"
        "tests/projects/nested-cabal/MyLib.hs"
        "tests/projects/nested-cabal/sub-comp/sub-comp.cabal"
        "tests/projects/nested-cabal/sub-comp/Lib.hs"
        "tests/projects/nested-stack/nested-stack.cabal"
        "tests/projects/nested-stack/hie.yaml"
        "tests/projects/nested-stack/MyLib.hs"
        "tests/projects/nested-stack/sub-comp/sub-comp.cabal"
        "tests/projects/nested-stack/sub-comp/Lib.hs"
        "tests/projects/simple-bios/A.hs"
        "tests/projects/simple-bios/B.hs"
        "tests/projects/simple-bios/hie-bios.sh"
        "tests/projects/simple-bios/hie-bios-deps.sh"
        "tests/projects/simple-bios/hie.yaml"
        "tests/projects/simple-bios-ghc/A.hs"
        "tests/projects/simple-bios-ghc/B.hs"
        "tests/projects/simple-bios-ghc/hie-bios.sh"
        "tests/projects/simple-bios-ghc/hie.yaml"
        "tests/projects/simple-bios-shell/A.hs"
        "tests/projects/simple-bios-shell/B.hs"
        "tests/projects/simple-bios-shell/hie.yaml"
        "tests/projects/simple-cabal/A.hs"
        "tests/projects/simple-cabal/B.hs"
        "tests/projects/simple-cabal/cabal.project"
        "tests/projects/simple-cabal/hie.yaml"
        "tests/projects/simple-cabal/simple-cabal.cabal"
        "tests/projects/simple-direct/A.hs"
        "tests/projects/simple-direct/B.hs"
        "tests/projects/simple-direct/hie.yaml"
        "tests/projects/simple-stack/A.hs"
        "tests/projects/simple-stack/B.hs"
        "tests/projects/simple-stack/cabal.project"
        "tests/projects/simple-stack/hie.yaml"
        "tests/projects/simple-stack/simple-stack.cabal"
        "tests/projects/space stack/A.hs"
        "tests/projects/space stack/B.hs"
        "tests/projects/space stack/hie.yaml"
        "tests/projects/space stack/stackproj.cabal"
        "tests/projects/implicit-cabal/cabal.project"
        "tests/projects/implicit-cabal/implicit-cabal.cabal"
        "tests/projects/implicit-cabal/Main.hs"
        "tests/projects/implicit-cabal-no-project/implicit-cabal-no-project.cabal"
        "tests/projects/implicit-cabal-no-project/Main.hs"
        "tests/projects/implicit-cabal-deep-project/README"
        "tests/projects/implicit-cabal-deep-project/implicit-cabal-deep-project.cabal"
        "tests/projects/implicit-cabal-deep-project/Main.hs"
        "tests/projects/implicit-cabal-deep-project/cabal.project"
        "tests/projects/implicit-cabal-deep-project/foo/foo.cabal"
        "tests/projects/implicit-cabal-deep-project/foo/Main.hs"
        "tests/projects/implicit-stack/implicit-stack.cabal"
        "tests/projects/implicit-stack/Main.hs"
        "tests/projects/implicit-stack-multi/implicit-stack-multi.cabal"
        "tests/projects/implicit-stack-multi/Main.hs"
        "tests/projects/implicit-stack-multi/other-package/other-package.cabal"
        "tests/projects/implicit-stack-multi/other-package/Main.hs"
        "tests/projects/multi-stack-with-yaml/appA/appA.cabal"
        "tests/projects/multi-stack-with-yaml/appA/src/Lib.hs"
        "tests/projects/multi-stack-with-yaml/appB/appB.cabal"
        "tests/projects/multi-stack-with-yaml/appB/src/Lib.hs"
        "tests/projects/multi-stack-with-yaml/hie.yaml"
        "tests/projects/stack-with-yaml/app/Main.hs"
        "tests/projects/stack-with-yaml/hie.yaml"
        "tests/projects/stack-with-yaml/stack-with-yaml.cabal"
        "tests/projects/stack-with-yaml/src/Lib.hs"
        ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."cryptohash-sha1" or (errorHandler.buildDepError "cryptohash-sha1"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."ghc" or (errorHandler.buildDepError "ghc"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."unix-compat" or (errorHandler.buildDepError "unix-compat"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
          (hsPkgs."hslogger" or (errorHandler.buildDepError "hslogger"))
          (hsPkgs."file-embed" or (errorHandler.buildDepError "file-embed"))
          (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
          (hsPkgs."conduit-extra" or (errorHandler.buildDepError "conduit-extra"))
          ];
        buildable = true;
        modules = [
          "Paths_hie_bios"
          "HIE/Bios"
          "HIE/Bios/Config"
          "HIE/Bios/Cradle"
          "HIE/Bios/Environment"
          "HIE/Bios/Internal/Debug"
          "HIE/Bios/Flags"
          "HIE/Bios/Types"
          "HIE/Bios/Internal/Log"
          "HIE/Bios/Ghc/Api"
          "HIE/Bios/Ghc/Check"
          "HIE/Bios/Ghc/Doc"
          "HIE/Bios/Ghc/Gap"
          "HIE/Bios/Ghc/Load"
          "HIE/Bios/Ghc/Logger"
          "HIE/Bios/Wrappers"
          ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "hie-bios" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."ghc" or (errorHandler.buildDepError "ghc"))
            (hsPkgs."hie-bios" or (errorHandler.buildDepError "hie-bios"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            ];
          buildable = true;
          modules = [ "Paths_hie_bios" ];
          hsSourceDirs = [ "exe" ];
          mainPath = [ "Main.hs" ];
          };
        };
      tests = {
        "parser-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hie-bios" or (errorHandler.buildDepError "hie-bios"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."yaml" or (errorHandler.buildDepError "yaml"))
            ];
          buildable = true;
          hsSourceDirs = [ "tests/" ];
          mainPath = [ "ParserTests.hs" ];
          };
        "bios-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-expected-failure" or (errorHandler.buildDepError "tasty-expected-failure"))
            (hsPkgs."hie-bios" or (errorHandler.buildDepError "hie-bios"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
            (hsPkgs."ghc" or (errorHandler.buildDepError "ghc"))
            ];
          buildable = true;
          hsSourceDirs = [ "tests/" ];
          mainPath = [ "BiosTests.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.; }