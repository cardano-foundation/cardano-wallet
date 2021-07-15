############################################################################
#
# Hydra release jobset.
#
# The purpose of this file is to select jobs defined in default.nix and map
# them to all supported build platforms.
#
# The layout is PLATFORM.ATTR-PATH where
#
#   * PLATFORM is one of
#     - linux.native - normal glibc build
#     - linux.musl - fully static build with musl libc
#     - windows - cross-compiled windows
#     - musl64 - build the job for Linux, but statically linked with musl libc
#
#   * ATTR-PATH is usually an attribute from default.nix. Some
#     release-only attributes are added and others are filtered out.
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

# Enable debug tracing
, debug ? false
}:

with pkgs.lib;

assert pkgs.lib.asserts.assertOneOf "platform" platform
  ["all" "linux" "macos" "windows"];

let
  buildNative  = elem builtins.currentSystem supportedSystems;
  buildLinux   = elem names.linux supportedSystems && buildForPlatform "linux";
  buildMacOS   = elem names.macos supportedSystems && buildForPlatform "macos";
  buildMusl    = elem names.linux supportedCrossSystems && buildLinux;
  buildWindows = elem builtins.currentSystem supportedCrossSystems && buildForPlatform "windows";
  buildForPlatform = name: elem platform ["all" name];

  makeReleaseLib = system: (import pkgs.iohkNix.release-lib) {
    inherit pkgs scrubJobs projectArgs;
    supportedSystems = [ system ];
    supportedCrossSystems = filter (s: s == system) supportedCrossSystems;
    packageSet = import cardano-wallet;
    gitrev = cardano-wallet.rev;
  };

  # Shorthand system names
  names = {
    linux = "x86_64-linux";
    macos = "x86_64-darwin";
  };

  # Debug tracing function
  tr = val: if debug then __trace (__toJSON val) val else val;

  # Instantiate project package sets for linux and macos.
  releaseLib = mapAttrs (const makeReleaseLib) names // {
    windows = if (elem names.linux supportedCrossSystems)
              then releaseLib.linux
              else releaseLib.macos;
  };

  ####################################################################
  # The main jobset which maps derivations from default.nix to build
  # platforms

  platformJobs = optionalAttrs buildLinux (with releaseLib.linux; rec {
    linux = optionalAttrs buildNative {
      native = untag (mapTestOn
        (tr (packagePlatforms (filterJobs.linux.native project))));

      # Build and cache the build script used on Buildkite
      buildkiteScript = import ./.buildkite/default.nix {
        inherit pkgs;
      };
    } // optionalAttrs buildMusl {
      musl = untag (mapTestOnCross systems.examples.musl64
        (tr (packagePlatformsCross (filterJobs.linux.musl project))));
    };

    cardano-wallet-linux64 = import ./nix/release-package.nix {
      inherit pkgs;
      exes = releaseContents (if buildMusl then linux.musl else linux.native);
      platform = "linux64";
      format = "tar.gz";
    };
  }) // optionalAttrs (buildMacOS && buildNative) (with releaseLib.macos; (rec {
    macos = untag (mapTestOn (tr (packagePlatforms (filterJobs.macos project))));

    cardano-wallet-macos64 = hydraJob' (import ./nix/release-package.nix {
      inherit ((pkgsFor names.macos).private) pkgs;
      exes = releaseContents macos;
      platform = "macos64";
      format = "tar.gz";
    });
  })) // optionalAttrs buildWindows (with releaseLib.windows; (rec {
    windows = untag (mapTestOnCross systems.examples.mingwW64
      (tr (packagePlatformsCross (filterJobs.windows project))));

    cardano-wallet-win64 = import ./nix/release-package.nix {
      inherit pkgs;
      exes = releaseContents windows;
      platform = "win64";
      format = "zip";
    };

    # This is used for testing the build on windows.
    cardano-wallet-tests-win64 = import ./nix/windows-testing-bundle.nix {
      inherit pkgs project;
      cardano-wallet = windows.cardano-wallet;
      cardano-node = windows.cardano-node;
      cardano-cli = windows.cardano-cli;
      tests = collectTestsWithPackageName windows.tests;
      benchmarks = collectTestsWithPackageName windows.benchmarks;
    };
  }));

  # Which exes should be put in the release archive.
  releaseContents = jobs: map (exe: jobs.${exe}) [
    "cardano-wallet"
    "bech32"
    "cardano-address"
    "cardano-cli"
    "cardano-node"
  ];

  ####################################################################
  # Job filters

  filterJobs = {
    linux = {
      native = makeJobFilter filterJobsNative {
        # Don't run tests on linux native, because they are run for linux musl.
        checks = [];
        testCoverageReport = [];
      };
      musl = makeJobFilter filterJobsCross {};
    };
    macos = makeJobFilter filterJobsNative {
      dockerImage = [];
    };
    windows = makeJobFilter filterJobsCross {
      # Remove cardano-node integration tests for Windows because
      # ouroboros-network doesn't work under wine.
      checks.cardano-wallet.integration = [];
      # Remove the test coverage report - only generate that for Linux musl.
      testCoverageReport = [];
    };
  };

  makeJobFilter = base: filt: js: recursiveUpdate (recursiveUpdate js base) filt;

  # Remove build jobs for which cross compiling does not make sense.
  filterJobsCross = {
    dockerImage = [];
    private = {
      shell = [];
      shell-prof = [];
      cabalShell = [];
      stackShell = [];
      stackNixRegenerate = [];
    };
  };

  # Filters the derivations from default.nix for non-cross builds.
  filterJobsNative = (optionalAttrs (pr != null) {
    # Build profiled packages for the master branch, so that they are
    # cached. But don't make profiled builds for PRs because this is
    # a waste of time.
    private.shell-prof = [];
  });

  ############################################################################
  # This aggregate job is what IOHK Hydra uses to update the CI status
  # in GitHub.

  required = releaseLib.linux.mkRequiredJob (
    withPlatform "linux.native" (path: js:
      collectTestNames path "checks" js ++
      collectTestNames path "benchmarks" js ++
      [ "${path}.private.shell"
        "${path}.cardano-wallet"
      ]) ++
    withPlatform "linux.musl" (path: js:
      collectTestNames path "checks" js ++
      # Release package for Linux
      [ "cardano-wallet-linux64"
      ]) ++
    withPlatform "macos" (path: js:
      collectTestNames path "checks" js ++
      collectTestNames path "benchmarks" js ++
      [ "${path}.private.shell"
        # executables for macOS
        "${path}.cardano-wallet"

        # Release package for macOS
        "cardano-wallet-macos64"
      ]) ++
    withPlatform "windows" (path: js:
      collectTestNames path "checks" js ++
      [ "${path}.cardano-wallet"
        # Release package for Windows
        "cardano-wallet-win64"
        # Windows testing package - is run nightly in CI.
        "cardano-wallet-tests-win64"
      ])
  );

  ####################################################################
  # Jobset munging

  # Removes redundant system tags from the jobset
  untag = let
    getLeaf = attrs: let
      names = attrNames attrs;
      name = head (attrNames attrs);
    in
      if (length names == 1 && elem name supportedSystems)
        then attrs.${name}
        else null;
  in mapAttrsRecursiveCond
    (as: !(isDerivation as) && getLeaf as == null)
    (path: attrs:
      let leaf = getLeaf attrs;
      in if leaf == null then attrs else leaf);

  ############################################################################
  # Collecting jobs

  testsSupportedSystems =
    optional buildLinux names.linux ++
    optional buildMacOS names.macos;
  isTest = drv: isDerivation drv && elem drv.system testsSupportedSystems;

  # Adds the package name to the test derivations for windows-testing-bundle.nix
  # (passthru.identifier.name does not survive mapTestOn)
  collectTestsWithPackageName = ds: concatLists
    (mapAttrsToList (packageName: package: map
      (drv: drv // { inherit packageName; })
      (filter isTest (collect isDerivation package))) ds);

  collectTestNames = base: attr: js:
    optionals (hasAttr attr js)
    (collect isString
      (mapAttrsRecursiveCond
        (as: !(isDerivation as))
        (path: drv: if isTest drv
          then concatStringsSep "." ([base attr] ++ path)
          else null)
        js.${attr}));

  withPlatform = path: f:
    let jobset = attrByPath (splitString "." path) null platformJobs;
    in if (jobset != null) then f path jobset else [];

in
  pkgs.commonLib.traceNames "job " (platformJobs // required)
