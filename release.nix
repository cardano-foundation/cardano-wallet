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
    inherit pr borsBuild sourcesOverride;
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

# Can be "staging" or "trying" to indicate that this is a bors jobset
, borsBuild ? null

# Platform filter string for jobset.
, platform ? "all"
}:

assert pkgs.lib.asserts.assertOneOf "platform" platform
  ["all" "linux" "macos" "windows"];

let
  buildNative  = builtins.elem builtins.currentSystem supportedSystems;
  buildLinux   = builtins.elem "x86_64-linux" supportedSystems && buildForPlatform "linux";
  buildMacOS   = builtins.elem "x86_64-darwin" supportedSystems && buildForPlatform "macos";
  buildMusl    = builtins.elem "x86_64-linux" supportedCrossSystems && buildLinux;
  buildWindows = builtins.elem builtins.currentSystem supportedCrossSystems && buildForPlatform "windows";
  buildForPlatform = name: builtins.elem platform ["all" name];
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
  filterJobsCross = js: recursiveUpdate js {
    dockerImage = [];
    private = {
      shell = [];
      shell-prof = [];
      cabalShell = [];
      stackShell = [];
      stackNixRegenerate = [];
    };
  };

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
      else js: recursiveUpdate js { private.shell-prof = null; };
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
        jobs.native.private.shell.x86_64-linux
        # executables for linux
        jobs.native.cardano-wallet.x86_64-linux
      ] ++
      optionals buildMacOS [
        jobs.native.private.shell.x86_64-darwin
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
  releaseContents = [
    "cardano-wallet"
    "bech32"
    "cardano-address"
    "cardano-cli"
    "cardano-node"
  ];

  # function to take a list of jobs by name from a jobset.
  selectExes = subjobs: system: map (exe: subjobs.${exe}.${system});

  releaseDistJobs = optionalAttrs buildMusl {
    cardano-wallet-linux64 = import ./nix/release-package.nix {
      inherit pkgs;
      exes = selectExes jobs.musl64 "x86_64-linux" releaseContents;
      platform = "linux64";
      format = "tar.gz";
    };
  } // optionalAttrs buildMacOS {
    cardano-wallet-macos64 = hydraJob' (import ./nix/release-package.nix {
      inherit ((pkgsFor "x86_64-darwin").private) pkgs;
      exes = selectExes jobs.native "x86_64-darwin" releaseContents;
      platform = "macos64";
      format = "tar.gz";
    });
  } // optionalAttrs buildWindows {
    cardano-wallet-win64 = import ./nix/release-package.nix {
      inherit pkgs;
      exes = selectExes jobs.x86_64-w64-mingw32 builtins.currentSystem releaseContents;
      platform = "win64";
      format = "zip";
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
  };

  ############################################################################
  # Miscellaneous extra jobs

  otherJobs = optionalAttrs buildLinux {
    # Build and cache the build script used on Buildkite
    buildkiteScript = import ./.buildkite/default.nix {
      inherit pkgs;
    };
  };

  jobs = mappedJobs // required // releaseDistJobs // otherJobs;

in jobs
