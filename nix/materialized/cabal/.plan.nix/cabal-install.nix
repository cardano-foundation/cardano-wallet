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
      native-dns = true;
      debug-expensive-assertions = false;
      debug-conflict-sets = false;
      debug-tracetree = false;
      lukko = true;
      };
    package = {
      specVersion = "1.10";
      identifier = { name = "cabal-install"; version = "3.2.0.0"; };
      license = "BSD-3-Clause";
      copyright = "2003-2020, Cabal Development Team";
      maintainer = "Cabal Development Team <cabal-devel@haskell.org>";
      author = "Cabal Development Team (see AUTHORS file)";
      homepage = "http://www.haskell.org/cabal/";
      url = "";
      synopsis = "The command-line interface for Cabal and Hackage.";
      description = "The \\'cabal\\' command-line program simplifies the process of managing\nHaskell software by automating the fetching, configuration, compilation\nand installation of Haskell libraries and programs.";
      buildType = "Custom";
      isLocal = true;
      setup-depends = [
        (hsPkgs.buildPackages.Cabal or (pkgs.buildPackages.Cabal or (errorHandler.buildToolDepError "Cabal")))
        (hsPkgs.buildPackages.base or (pkgs.buildPackages.base or (errorHandler.buildToolDepError "base")))
        (hsPkgs.buildPackages.process or (pkgs.buildPackages.process or (errorHandler.buildToolDepError "process")))
        (hsPkgs.buildPackages.filepath or (pkgs.buildPackages.filepath or (errorHandler.buildToolDepError "filepath")))
        ];
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = "";
      dataFiles = [];
      extraSrcFiles = [
        "README.md"
        "bash-completion/cabal"
        "bootstrap.sh"
        "changelog"
        "tests/IntegrationTests2/build/keep-going/cabal.project"
        "tests/IntegrationTests2/build/keep-going/p/P.hs"
        "tests/IntegrationTests2/build/keep-going/p/p.cabal"
        "tests/IntegrationTests2/build/keep-going/q/Q.hs"
        "tests/IntegrationTests2/build/keep-going/q/q.cabal"
        "tests/IntegrationTests2/build/local-tarball/cabal.project"
        "tests/IntegrationTests2/build/local-tarball/q/Q.hs"
        "tests/IntegrationTests2/build/local-tarball/q/q.cabal"
        "tests/IntegrationTests2/build/setup-custom1/A.hs"
        "tests/IntegrationTests2/build/setup-custom1/Setup.hs"
        "tests/IntegrationTests2/build/setup-custom1/a.cabal"
        "tests/IntegrationTests2/build/setup-custom2/A.hs"
        "tests/IntegrationTests2/build/setup-custom2/Setup.hs"
        "tests/IntegrationTests2/build/setup-custom2/a.cabal"
        "tests/IntegrationTests2/build/setup-simple/A.hs"
        "tests/IntegrationTests2/build/setup-simple/Setup.hs"
        "tests/IntegrationTests2/build/setup-simple/a.cabal"
        "tests/IntegrationTests2/exception/bad-config/cabal.project"
        "tests/IntegrationTests2/exception/build/Main.hs"
        "tests/IntegrationTests2/exception/build/a.cabal"
        "tests/IntegrationTests2/exception/configure/a.cabal"
        "tests/IntegrationTests2/exception/no-pkg/empty.in"
        "tests/IntegrationTests2/exception/no-pkg2/cabal.project"
        "tests/IntegrationTests2/regression/3324/cabal.project"
        "tests/IntegrationTests2/regression/3324/p/P.hs"
        "tests/IntegrationTests2/regression/3324/p/p.cabal"
        "tests/IntegrationTests2/regression/3324/q/Q.hs"
        "tests/IntegrationTests2/regression/3324/q/q.cabal"
        "tests/IntegrationTests2/targets/all-disabled/cabal.project"
        "tests/IntegrationTests2/targets/all-disabled/p.cabal"
        "tests/IntegrationTests2/targets/benchmarks-disabled/cabal.project"
        "tests/IntegrationTests2/targets/benchmarks-disabled/p.cabal"
        "tests/IntegrationTests2/targets/benchmarks-disabled/q/q.cabal"
        "tests/IntegrationTests2/targets/complex/cabal.project"
        "tests/IntegrationTests2/targets/complex/q/Q.hs"
        "tests/IntegrationTests2/targets/complex/q/q.cabal"
        "tests/IntegrationTests2/targets/empty-pkg/cabal.project"
        "tests/IntegrationTests2/targets/empty-pkg/p.cabal"
        "tests/IntegrationTests2/targets/empty/cabal.project"
        "tests/IntegrationTests2/targets/empty/foo.hs"
        "tests/IntegrationTests2/targets/exes-disabled/cabal.project"
        "tests/IntegrationTests2/targets/exes-disabled/p/p.cabal"
        "tests/IntegrationTests2/targets/exes-disabled/q/q.cabal"
        "tests/IntegrationTests2/targets/lib-only/p.cabal"
        "tests/IntegrationTests2/targets/libs-disabled/cabal.project"
        "tests/IntegrationTests2/targets/libs-disabled/p/p.cabal"
        "tests/IntegrationTests2/targets/libs-disabled/q/q.cabal"
        "tests/IntegrationTests2/targets/multiple-exes/cabal.project"
        "tests/IntegrationTests2/targets/multiple-exes/p.cabal"
        "tests/IntegrationTests2/targets/multiple-libs/cabal.project"
        "tests/IntegrationTests2/targets/multiple-libs/p/p.cabal"
        "tests/IntegrationTests2/targets/multiple-libs/q/q.cabal"
        "tests/IntegrationTests2/targets/multiple-tests/cabal.project"
        "tests/IntegrationTests2/targets/multiple-tests/p.cabal"
        "tests/IntegrationTests2/targets/simple/P.hs"
        "tests/IntegrationTests2/targets/simple/cabal.project"
        "tests/IntegrationTests2/targets/simple/p.cabal"
        "tests/IntegrationTests2/targets/simple/q/QQ.hs"
        "tests/IntegrationTests2/targets/simple/q/q.cabal"
        "tests/IntegrationTests2/targets/test-only/p.cabal"
        "tests/IntegrationTests2/targets/tests-disabled/cabal.project"
        "tests/IntegrationTests2/targets/tests-disabled/p.cabal"
        "tests/IntegrationTests2/targets/tests-disabled/q/q.cabal"
        "tests/IntegrationTests2/targets/variety/cabal.project"
        "tests/IntegrationTests2/targets/variety/p.cabal"
        "tests/IntegrationTests2/build/local-tarball/p-0.1.tar.gz"
        ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      exes = {
        "cabal" = {
          depends = ((((([
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."cryptohash-sha256" or (errorHandler.buildDepError "cryptohash-sha256"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."echo" or (errorHandler.buildDepError "echo"))
            (hsPkgs."edit-distance" or (errorHandler.buildDepError "edit-distance"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."HTTP" or (errorHandler.buildDepError "HTTP"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."pretty" or (errorHandler.buildDepError "pretty"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."tar" or (errorHandler.buildDepError "tar"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."zlib" or (errorHandler.buildDepError "zlib"))
            (hsPkgs."hackage-security" or (errorHandler.buildDepError "hackage-security"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
            ] ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.0")) [
            (hsPkgs."fail" or (errorHandler.buildDepError "fail"))
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            ]) ++ (pkgs.lib).optionals (flags.native-dns) (if system.isWindows
            then [ (hsPkgs."windns" or (errorHandler.buildDepError "windns")) ]
            else [
              (hsPkgs."resolv" or (errorHandler.buildDepError "resolv"))
              ])) ++ (if system.isWindows
            then [ (hsPkgs."Win32" or (errorHandler.buildDepError "Win32")) ]
            else [
              (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
              ])) ++ (if flags.lukko
            then [ (hsPkgs."lukko" or (errorHandler.buildDepError "lukko")) ]
            else [
              (hsPkgs."base" or (errorHandler.buildDepError "base"))
              ])) ++ (pkgs.lib).optional (flags.debug-conflict-sets) (hsPkgs."base" or (errorHandler.buildDepError "base"))) ++ (pkgs.lib).optional (flags.debug-tracetree) (hsPkgs."tracetree" or (errorHandler.buildDepError "tracetree"));
          libs = (pkgs.lib).optional (system.isAix) (pkgs."bsd" or (errorHandler.sysDepError "bsd"));
          buildable = true;
          modules = [
            "Distribution/Deprecated/ParseUtils"
            "Distribution/Deprecated/ReadP"
            "Distribution/Deprecated/Text"
            "Distribution/Deprecated/ViewAsFieldDescr"
            "Distribution/Client/BuildReports/Anonymous"
            "Distribution/Client/BuildReports/Storage"
            "Distribution/Client/BuildReports/Types"
            "Distribution/Client/BuildReports/Upload"
            "Distribution/Client/Check"
            "Distribution/Client/CmdBench"
            "Distribution/Client/CmdBuild"
            "Distribution/Client/CmdClean"
            "Distribution/Client/CmdConfigure"
            "Distribution/Client/CmdUpdate"
            "Distribution/Client/CmdErrorMessages"
            "Distribution/Client/CmdExec"
            "Distribution/Client/CmdFreeze"
            "Distribution/Client/CmdHaddock"
            "Distribution/Client/CmdInstall"
            "Distribution/Client/CmdInstall/ClientInstallFlags"
            "Distribution/Client/CmdRepl"
            "Distribution/Client/CmdRun"
            "Distribution/Client/CmdRun/ClientRunFlags"
            "Distribution/Client/CmdTest"
            "Distribution/Client/CmdLegacy"
            "Distribution/Client/CmdSdist"
            "Distribution/Client/Compat/Directory"
            "Distribution/Client/Compat/ExecutablePath"
            "Distribution/Client/Compat/FilePerms"
            "Distribution/Client/Compat/Orphans"
            "Distribution/Client/Compat/Prelude"
            "Distribution/Client/Compat/Process"
            "Distribution/Client/Compat/Semaphore"
            "Distribution/Client/Config"
            "Distribution/Client/Configure"
            "Distribution/Client/Dependency"
            "Distribution/Client/Dependency/Types"
            "Distribution/Client/DistDirLayout"
            "Distribution/Client/Exec"
            "Distribution/Client/Fetch"
            "Distribution/Client/FetchUtils"
            "Distribution/Client/FileMonitor"
            "Distribution/Client/Freeze"
            "Distribution/Client/GZipUtils"
            "Distribution/Client/GenBounds"
            "Distribution/Client/Get"
            "Distribution/Client/Glob"
            "Distribution/Client/GlobalFlags"
            "Distribution/Client/Haddock"
            "Distribution/Client/HashValue"
            "Distribution/Client/HttpUtils"
            "Distribution/Client/IndexUtils"
            "Distribution/Client/IndexUtils/Timestamp"
            "Distribution/Client/Init"
            "Distribution/Client/Init/Heuristics"
            "Distribution/Client/Init/Licenses"
            "Distribution/Client/Init/Types"
            "Distribution/Client/Install"
            "Distribution/Client/InstallPlan"
            "Distribution/Client/InstallSymlink"
            "Distribution/Client/JobControl"
            "Distribution/Client/List"
            "Distribution/Client/Manpage"
            "Distribution/Client/Nix"
            "Distribution/Client/Outdated"
            "Distribution/Client/PackageHash"
            "Distribution/Client/PackageUtils"
            "Distribution/Client/ParseUtils"
            "Distribution/Client/ProjectBuilding"
            "Distribution/Client/ProjectBuilding/Types"
            "Distribution/Client/ProjectConfig"
            "Distribution/Client/ProjectConfig/Legacy"
            "Distribution/Client/ProjectConfig/Types"
            "Distribution/Client/ProjectOrchestration"
            "Distribution/Client/ProjectPlanOutput"
            "Distribution/Client/ProjectPlanning"
            "Distribution/Client/ProjectPlanning/Types"
            "Distribution/Client/RebuildMonad"
            "Distribution/Client/Reconfigure"
            "Distribution/Client/Run"
            "Distribution/Client/Sandbox"
            "Distribution/Client/Sandbox/Index"
            "Distribution/Client/Sandbox/PackageEnvironment"
            "Distribution/Client/Sandbox/Timestamp"
            "Distribution/Client/Sandbox/Types"
            "Distribution/Client/SavedFlags"
            "Distribution/Client/Security/DNS"
            "Distribution/Client/Security/HTTP"
            "Distribution/Client/Setup"
            "Distribution/Client/SetupWrapper"
            "Distribution/Client/SolverInstallPlan"
            "Distribution/Client/SourceFiles"
            "Distribution/Client/SourceRepo"
            "Distribution/Client/SrcDist"
            "Distribution/Client/Store"
            "Distribution/Client/Tar"
            "Distribution/Client/TargetSelector"
            "Distribution/Client/Targets"
            "Distribution/Client/Types"
            "Distribution/Client/Update"
            "Distribution/Client/Upload"
            "Distribution/Client/Utils"
            "Distribution/Client/Utils/Assertion"
            "Distribution/Client/Utils/Json"
            "Distribution/Client/Utils/Parsec"
            "Distribution/Client/VCS"
            "Distribution/Client/Win32SelfUpgrade"
            "Distribution/Client/World"
            "Distribution/Solver/Compat/Prelude"
            "Distribution/Solver/Modular"
            "Distribution/Solver/Modular/Assignment"
            "Distribution/Solver/Modular/Builder"
            "Distribution/Solver/Modular/Configured"
            "Distribution/Solver/Modular/ConfiguredConversion"
            "Distribution/Solver/Modular/ConflictSet"
            "Distribution/Solver/Modular/Cycles"
            "Distribution/Solver/Modular/Dependency"
            "Distribution/Solver/Modular/Explore"
            "Distribution/Solver/Modular/Flag"
            "Distribution/Solver/Modular/Index"
            "Distribution/Solver/Modular/IndexConversion"
            "Distribution/Solver/Modular/LabeledGraph"
            "Distribution/Solver/Modular/Linking"
            "Distribution/Solver/Modular/Log"
            "Distribution/Solver/Modular/Message"
            "Distribution/Solver/Modular/PSQ"
            "Distribution/Solver/Modular/Package"
            "Distribution/Solver/Modular/Preference"
            "Distribution/Solver/Modular/RetryLog"
            "Distribution/Solver/Modular/Solver"
            "Distribution/Solver/Modular/Tree"
            "Distribution/Solver/Modular/Validate"
            "Distribution/Solver/Modular/Var"
            "Distribution/Solver/Modular/Version"
            "Distribution/Solver/Modular/WeightedPSQ"
            "Distribution/Solver/Types/ComponentDeps"
            "Distribution/Solver/Types/ConstraintSource"
            "Distribution/Solver/Types/DependencyResolver"
            "Distribution/Solver/Types/Flag"
            "Distribution/Solver/Types/InstSolverPackage"
            "Distribution/Solver/Types/InstalledPreference"
            "Distribution/Solver/Types/LabeledPackageConstraint"
            "Distribution/Solver/Types/OptionalStanza"
            "Distribution/Solver/Types/PackageConstraint"
            "Distribution/Solver/Types/PackageFixedDeps"
            "Distribution/Solver/Types/PackageIndex"
            "Distribution/Solver/Types/PackagePath"
            "Distribution/Solver/Types/PackagePreferences"
            "Distribution/Solver/Types/PkgConfigDb"
            "Distribution/Solver/Types/Progress"
            "Distribution/Solver/Types/ResolverPackage"
            "Distribution/Solver/Types/Settings"
            "Distribution/Solver/Types/SolverId"
            "Distribution/Solver/Types/SolverPackage"
            "Distribution/Solver/Types/SourcePackage"
            "Distribution/Solver/Types/Variable"
            "Paths_cabal_install"
            ];
          hsSourceDirs = [ "main" "." ];
          mainPath = (((((((([
            "Main.hs"
            ] ++ (pkgs.lib).optionals (compiler.isGhc && (compiler.version).ge "8.0") ([
            ""
            ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8.8") "")) ++ (pkgs.lib).optional (system.isAix) "") ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) "") ++ (pkgs.lib).optionals (flags.native-dns) ([
            ""
            ] ++ [ "" ])) ++ [ "" ]) ++ [
            ""
            ]) ++ (pkgs.lib).optional (flags.debug-expensive-assertions) "") ++ (pkgs.lib).optional (flags.debug-conflict-sets) "") ++ (pkgs.lib).optional (flags.debug-tracetree) "";
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.; }