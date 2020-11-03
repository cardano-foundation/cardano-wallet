############################################################################
#
# Hydra release jobset.
#
# The purpose of this file is to select jobs defined in default.nix and map
# them to all supported build platforms.
#
# The layout is TARGET-SYSTEM.ATTR-PATH.BUILD-SYSTEM where
#
#   * TARGET-SYSTEM is one of
#     - native - build the job for BUILD-SYSTEM
#     - x86_64-w64-mingw32 - build the job for windows
#     - musl64 - build the job for Linux, but statically linked with musl libc
#
#   * ATTR-PATH is an attribute from default.nix
#
#   * BUILD-SYSTEM is the system where the derivation is built. Hydra
#     uses this to distribute builds to its build slaves. If building
#     locally then it must reflect your local system. The value is one of:
#     - x86_64-linux - Linux
#     - x86_64-darwin - macOS
#
# Discover jobs by using tab completion in your shell:
#   nix-build release.nix -A <TAB>
# ... or by looking at the jobset evaluated by Hydra:
#   https://hydra.iohk.io/jobset/Cardano/cardano-wallet#tabs-jobs
#
# To build locally when you do not have access to remote builders for
# either macOS or Linux, change the `supportedSystems` argument.
# - To build on Linux (without macOS):
#     nix-build --arg supportedSystems '["x86_64-linux"]' release.nix
# - To build on macOS (without Linux):
#     nix-build --arg supportedSystems '["x86_64-darwin"]' supportedCrossSystems '["x86_64-darwin"]' release.nix
#
############################################################################

# The project sources
{ cardano-wallet ? { outPath = ./.; rev = pkgs.commonLib.commitIdFromGitRepoOrZero ./.git; }

# Function arguments to pass to the project
, projectArgs ? {
    config = { allowUnfree = false; inHydra = true; };
    gitrev = cardano-wallet.rev;
    inherit pr sourcesOverride;
  }

# The systems that the jobset will be built for.
, supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]

# The systems used for cross-compiling
, supportedCrossSystems ? [ "x86_64-linux" ]

# A Hydra option
, scrubJobs ? true

# Dependencies overrides
, sourcesOverride ? {}

# Import pkgs, including IOHK common nix lib
, pkgs ? import ./nix { inherit sourcesOverride; }

# GitHub PR number (as a string), provided as a Hydra input
, pr ? null
}:

let
  buildNative  = builtins.elem builtins.currentSystem supportedSystems;
  buildLinux   = builtins.elem "x86_64-linux" supportedSystems;
  buildMacOS   = builtins.elem "x86_64-darwin" supportedSystems;
  buildMusl    = builtins.elem "x86_64-linux" supportedCrossSystems && buildLinux;
  buildWindows = builtins.elem builtins.currentSystem supportedCrossSystems;
in

with (import pkgs.iohkNix.release-lib) {
  inherit pkgs;
  inherit supportedCrossSystems scrubJobs projectArgs;
  supportedSystems =
    pkgs.lib.optional buildLinux "x86_64-linux" ++
    pkgs.lib.optional buildMacOS "x86_64-darwin";
  packageSet = import cardano-wallet;
  gitrev = cardano-wallet.rev;
};

with pkgs.lib;

let
  ############################################################################
  # Mapping the default.nix derivations to build platforms

  inherit (systems.examples) mingwW64 musl64;

  mappedJobs = optionalAttrs buildNative {
    native = mapTestOn (filterMappedNative (packagePlatformsOrig (filterJobsNative project)));
  } // optionalAttrs buildWindows {
    "${mingwW64.config}" = mapTestOnCross mingwW64
      (packagePlatformsCross (filterJobsWindows project));
  } // optionalAttrs buildMusl {
    musl64 = mapTestOnCross musl64
      (packagePlatformsCross (filterJobsCross project));
  };

  ############################################################################
  # Collecting and filtering jobs

  testsSupportedSystems =
    optional buildLinux "x86_64-linux" ++
    optional buildMacOS "x86_64-darwin";

  # Recurse through an attrset, returning all test derivations in a list.
  collectTests' = ds: filter (d: elem d.system testsSupportedSystems) (collect isDerivation ds);
  # Adds the package name to the test derivations for windows-testing-bundle.nix
  # (passthru.identifier.name does not survive mapTestOn)
  collectTests = ds: concatLists (
    mapAttrsToList (packageName: package:
      map (drv: drv // { inherit packageName; }) (collectTests' package)
    ) ds);

  # Remove build jobs for which cross compiling does not make sense.
  filterJobsCross = filterAttrs (n: _: !(elem n [
    "dockerImage"
    "shell"
    "shell-prof"
    "stackShell"
    "cabalShell"
    "stackNixRegenerate"
  ]));

  # Remove cardano-node integration tests for Windows because
  # ouroboros-network doesn't work under wine.
  filterJobsWindows = let
      f = path: value: if (isCardanoNodeIntegration path) then {} else value;
      isCardanoNodeIntegration = path:
         path == ["checks" "cardano-wallet" "integration"];
    in
      js: mapAttrsRecursive f (filterJobsCross js);

  # Filters jobs for non-cross builds after platform mapping.
  # 1. Don't run tests on linux native, because they are also run for linux musl.
  filterMappedNative = let
    removeLinuxNativeChecks = path: value:
      if (head path == "checks" && builtins.typeOf value == "list")
        then remove "x86_64-linux" value
        else value;
  in mapAttrsRecursive removeLinuxNativeChecks;

  # Filters the derivations from default.nix for non-cross builds.
  # 1. Build profiled packages for the master branch, so that they are cached.
  #    But don't make profiled builds for PRs because this is a waste of time.
  # 2. Remove the test coverage report - only generate that for Linux musl.
  filterJobsNative = let
    removeProfiledBuildForPRs = if (pr == null)
      then id
      else filterAttrs (n: _: n != "shell-prof");
    removeCoverageReport = filterAttrs (n: _: n != "testCoverageReport");
  in
    drvs: removeCoverageReport (removeProfiledBuildForPRs drvs);

  ############################################################################
  # This aggregate job is what IOHK Hydra uses to update the CI status
  # in GitHub.

  required = mkRequiredJob (
    optionals buildNative (
      collectTests jobs.native.checks ++
      collectTests jobs.native.benchmarks ++
      optionals buildLinux [
        jobs.native.shell.x86_64-linux
        # executables for linux
        jobs.native.cardano-wallet.x86_64-linux
      ] ++
      optionals buildMacOS [
        jobs.native.shell.x86_64-darwin
        # executables for macOS
        jobs.native.cardano-wallet.x86_64-darwin

        # Release packages for macOS
        jobs.cardano-wallet-macos64
      ]) ++
    optionals buildMusl (
      collectTests jobs.musl64.checks ++
      # Release packages for Linux
      [ jobs.cardano-wallet-linux64
      ]) ++
    optionals buildWindows (
      collectTests jobs.x86_64-w64-mingw32.checks ++
      [ jobs.x86_64-w64-mingw32.cardano-wallet.x86_64-linux

        # Release packages for Windows
        jobs.cardano-wallet-win64

        # Windows testing package - is run nightly in CI.
        jobs.cardano-wallet-tests-win64
      ]));

  ############################################################################
  # Release distribution jobs - these all have a Hydra download link.

  # Which exes should be put in the release archive.
  releaseContents = {
    shelley = [
      "cardano-wallet"
      "bech32"
      "cardano-address"
      "cardano-cli"
      "cardano-node"
      "cardano-tx"
    ];
  };

  # function to take a list of jobs by name from a jobset.
  selectExes = subjobs: system: map (exe: subjobs.${exe}.${system});

  releaseDistJobs = optionalAttrs buildWindows {

    # Windows release ZIP archive - shelley
    cardano-wallet-win64 = import ./nix/windows-release.nix {
      inherit pkgs;
      exes = selectExes jobs.x86_64-w64-mingw32 "x86_64-linux" releaseContents.shelley;
    };

    # This is used for testing the build on windows.
    cardano-wallet-tests-win64 = let
      winJobs = jobs."${mingwW64.config}";
    in import ./nix/windows-testing-bundle.nix {
      inherit pkgs project;
      cardano-wallet = winJobs.cardano-wallet.x86_64-linux;
      cardano-node = winJobs.cardano-node.x86_64-linux;
      cardano-cli = winJobs.cardano-cli.x86_64-linux;
      tests = collectTests winJobs.tests;
      benchmarks = collectTests winJobs.benchmarks;
    };
  } // optionalAttrs buildMusl {
    cardano-wallet-linux64 = import ./nix/linux-release.nix {
      inherit pkgs;
      exes = selectExes jobs.musl64 "x86_64-linux" releaseContents.shelley;
    };
  } // optionalAttrs buildMacOS {
    cardano-wallet-macos64 = hydraJob' (import ./nix/macos-release.nix {
      inherit (pkgsFor "x86_64-darwin") pkgs;
      exes = selectExes jobs.native "x86_64-darwin" releaseContents.shelley;
    });
  };

  ############################################################################
  # Miscellaneous extra jobs

  otherJobs = optionalAttrs buildLinux {
    # Build and cache the build script used on Buildkite
    buildkiteScript = import ./.buildkite/default.nix {
      walletPackages = project;
    };
  };

  jobs = mappedJobs // required // releaseDistJobs // otherJobs;

in jobs
