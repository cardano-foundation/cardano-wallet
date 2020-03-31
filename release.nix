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
############################################################################

# The project sources
{ cardano-wallet ? { outPath = ./.; rev = "abcdef"; }

# Function arguments to pass to the project
, projectArgs ? {
    config = { allowUnfree = false; inHydra = true; };
    gitrev = cardano-wallet.rev;
    inherit pr;
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

with (import pkgs.iohkNix.release-lib) {
  inherit pkgs;
  inherit supportedSystems supportedCrossSystems scrubJobs projectArgs;
  packageSet = import cardano-wallet;
  gitrev = cardano-wallet.rev;
};

with pkgs.lib;

let
  testsSupportedSystems = [ "x86_64-linux" "x86_64-darwin" ];
  # Recurse through an attrset, returning all test derivations in a list.
  collectTests' = ds: filter (d: elem d.system testsSupportedSystems) (collect isDerivation ds);
  # Adds the package name to the test derivations for windows-testing-bundle.nix
  # (passthru.identifier.name does not survive mapTestOn)
  collectTests = ds: concatLists (
    mapAttrsToList (packageName: package:
      map (drv: drv // { inherit packageName; }) (collectTests' package)
    ) ds);

  # Remove build jobs for which cross compiling does not make sense.
  filterJobsCross = filterAttrs (n: _: n != "dockerImage" && n != "shell" && n != "stackShell");

  inherit (systems.examples) mingwW64 musl64;

  jobs = {
    native = mapTestOn (packagePlatformsOrig project);
    # Cross compilation, excluding the dockerImage and shells that we cannnot cross compile
    "${mingwW64.config}" = mapTestOnCross mingwW64
      (packagePlatformsCross (filterJobsCross project));
    musl64 = mapTestOnCross musl64
      (packagePlatformsCross (filterJobsCross project));
  }
  // {
    # This aggregate job is what IOHK Hydra uses to update
    # the CI status in GitHub.
    required = mkRequiredJob (
      collectTests jobs.native.tests ++
      collectTests jobs.native.benchmarks ++
      [ jobs.native.shell.x86_64-linux
        jobs.native.shell.x86_64-darwin

        # jormungandr
        jobs.native.cardano-wallet-jormungandr.x86_64-linux
        jobs.native.cardano-wallet-jormungandr.x86_64-darwin
        jobs.x86_64-pc-mingw32.cardano-wallet-jormungandr.x86_64-linux

        jobs.cardano-wallet-jormungandr-win64
        jobs.cardano-wallet-jormungandr-macos64
        jobs.cardano-wallet-jormungandr-tests-win64


        # cardano-node (Byron)
        jobs.native.cardano-wallet-byron.x86_64-linux
        jobs.native.cardano-wallet-byron.x86_64-darwin
      ]
    );

    # These derivations are used for the Daedalus installer.
    daedalus-jormungandr = with jobs; {
      linux = native.cardano-wallet-jormungandr.x86_64-linux;
      macos = native.cardano-wallet-jormungandr.x86_64-darwin;
      windows = x86_64-w64-mingw32.cardano-wallet-jormungandr.x86_64-linux;
    };

    # Windows release ZIP archive - jormungandr
    cardano-wallet-jormungandr-win64 = import ./nix/windows-release.nix {
      inherit pkgs;
      exe = jobs.x86_64-w64-mingw32.cardano-wallet-jormungandr.x86_64-linux;
    };

    # Windows release ZIP archive - byron rewrite
    cardano-wallet-byron-win64 = import ./nix/windows-release.nix {
      inherit pkgs;
      exe = jobs.x86_64-w64-mingw32.cardano-wallet-byron.x86_64-linux;
    };

    # This is used for testing the build on windows.
    cardano-wallet-jormungandr-tests-win64 = import ./nix/windows-testing-bundle.nix {
      inherit pkgs project;
      cardano-wallet-jormungandr = jobs.x86_64-w64-mingw32.cardano-wallet-jormungandr.x86_64-linux;
      cardano-wallet-byron = jobs.x86_64-w64-mingw32.cardano-wallet-byron.x86_64-linux;
      cardano-node = jobs.x86_64-w64-mingw32.cardano-node.x86_64-linux;
      tests = collectTests jobs.x86_64-w64-mingw32.tests;
      benchmarks = collectTests jobs.x86_64-w64-mingw32.benchmarks;
    };

    # For testing migration tests on windows
    migration-tests-win64 = import ./nix/windows-migration-tests-bundle.nix {
      inherit pkgs project;
      migration-tests = jobs.x86_64-w64-mingw32.migration-tests.x86_64-linux;
    };

    # Fully-static linux binary (placeholder - does not build)
    cardano-wallet-jormungandr-linux64 = let
      name = "cardano-wallet-jormungandr-${project.version}";
      tarname = "${name}-linux64.tar.gz";
    in pkgs.runCommand "${name}-linux64" {
      buildInputs = with pkgs.buildPackages; [ gnutar gzip binutils ];
    } ''
      cp -R ${jobs.musl64.cardano-wallet-jormungandr.x86_64-linux}/bin ${name}
      chmod -R 755 ${name}
      strip ${name}/*

      mkdir -p $out/nix-support
      tar -czf $out/${tarname} ${name}
      echo "file binary-dist $out/${tarname}" > $out/nix-support/hydra-build-products
    '';

    # macOS binary and dependencies in tarball
    cardano-wallet-jormungandr-macos64 = let
      name = "cardano-wallet-jormungandr-${project.version}";
      tarname = "${name}-macos64.tar.gz";
    in pkgs.runCommand "${name}-macos64" {
      buildInputs = with pkgs.buildPackages; [ gnutar gzip binutils nix ];
    } ''
      cp -R ${jobs.native.cardano-wallet-jormungandr.x86_64-darwin}/bin ${name}
      chmod -R 755 ${name}

      mkdir -p $out/nix-support
      tar -czf $out/${tarname} ${name}
      echo "file binary-dist $out/${tarname}" > $out/nix-support/hydra-build-products
    '';

    # Build and cache the build script used on Buildkite
    buildkiteScript = import ./.buildkite/default.nix {
      walletPackages = project;
    };
  }
  # Build the shell derivation in Hydra so that all its dependencies
  # are cached.
  // mapTestOn (packagePlatforms {
    inherit (project) shell stackShell;
  })
    # This aggregate job is what IOHK Hydra uses to update
    # the CI status in GitHub.
  // (mkRequiredJob (
      collectTests jobs.native.tests ++
      collectTests jobs.native.benchmarks ++
      [ jobs.native.cardano-wallet-jormungandr.x86_64-linux
        jobs.native.cardano-wallet-jormungandr.x86_64-darwin
        jobs.x86_64-w64-mingw32.cardano-wallet-jormungandr.x86_64-linux
        jobs.x86_64-w64-mingw32.cardano-wallet-byron.x86_64-linux
        jobs.native.shell.x86_64-linux
        jobs.native.shell.x86_64-darwin
        jobs.cardano-wallet-jormungandr-win64
        jobs.cardano-wallet-jormungandr-macos64
        jobs.cardano-wallet-jormungandr-tests-win64
      ]));

in jobs
