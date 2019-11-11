{ supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]
, supportedCrossSystems ? [ "x86_64-linux" ]
, scrubJobs ? true
, cardano-wallet ? { outPath = ./.; rev = "abcdef"; }
, projectArgs ? { config = { allowUnfree = false; inHydra = true; }; }
}:

with (import ./nix/release-lib.nix) {
  inherit (import ./nix/iohk-common.nix {}) pkgs;
  inherit supportedSystems supportedCrossSystems scrubJobs projectArgs;
  packageSet = import cardano-wallet;
  gitrev = cardano-wallet.rev;
};

with pkgs.lib;

let
  testsSupportedSystems = [ "x86_64-linux" "x86_64-darwin" ];
  collectTests = ds: filter (d: elem d.system testsSupportedSystems) (collect isDerivation ds);

  inherit (systems.examples) mingwW64 musl64;

  jobs = {
    native = mapTestOn (packagePlatforms project);
    "${mingwW64.config}" = mapTestOnCross mingwW64 (packagePlatformsCross project);
  }
  // {
    ci-tools = {
      inherit (import ./nix/iohk-common.nix {}) hlint;
      inherit ((import ./nix/nixpkgs-haskell.nix {}).haskellPackages) stylish-haskell;
    };
    inherit ((import ./. {}).pkgs.haskell-nix) haskellNixRoots;
    stackShell = import ./nix/stack-shell.nix {};

    # This aggregate job is what IOHK Hydra uses to update
    # the CI status in GitHub.
    required = mkRequiredJob (
      collectTests jobs.native.tests ++
      collectTests jobs.native.benchmarks ++
      [ jobs.native.cardano-wallet-jormungandr.x86_64-linux
        jobs.native.cardano-wallet-jormungandr.x86_64-darwin
        jobs.x86_64-pc-mingw32.cardano-wallet-jormungandr.x86_64-linux
      ]
    );

    # This is used for testing the build on windows.
    cardano-wallet-jormungandr-win64 = import ./nix/windows-release.nix {
      inherit pkgs project;
      cardano-wallet-jormungandr = jobs.x86_64-pc-mingw32.cardano-wallet-jormungandr.x86_64-linux;
      tests = collectTests jobs.x86_64-pc-mingw32.tests;
      benchmarks = collectTests jobs.x86_64-pc-mingw32.benchmarks;
    };

    # These derivations are used for the Daedalus installer.
    daedalus-jormungandr = with jobs; {
      linux = native.cardano-wallet-jormungandr.x86_64-linux;
      macos = native.cardano-wallet-jormungandr.x86_64-darwin;
      windows = x86_64-pc-mingw32.cardano-wallet-jormungandr.x86_64-linux;
    };
  }
  # Build the shell derivation in Hydra so that all its dependencies
  # are cached.
  // mapTestOn (packagePlatforms { inherit (project) shell; });

in jobs
