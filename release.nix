{ supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]
, supportedCrossSystems ? [ "x86_64-linux" ]
, scrubJobs ? true
, cardano-wallet ? { outPath = ./.; rev = "abcdef"; }
, projectArgs ? {
    config = { allowUnfree = false; inHydra = true; };
    gitrev = cardano-wallet.rev;
  }
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
    musl64 = mapTestOnCross musl64 (packagePlatformsCross project);
  }
  // {
    # This aggregate job is what IOHK Hydra uses to update
    # the CI status in GitHub.
    required = mkRequiredJob (
      collectTests jobs.native.tests ++
      collectTests jobs.native.benchmarks ++
      [ jobs.native.cardano-wallet-jormungandr.x86_64-linux
        jobs.native.cardano-wallet-jormungandr.x86_64-darwin
        jobs.x86_64-pc-mingw32.cardano-wallet-jormungandr.x86_64-linux
        jobs.native.shell.x86_64-linux
        jobs.native.shell.x86_64-darwin
        jobs.cardano-wallet-jormungandr-win64
        jobs.cardano-wallet-jormungandr-macos64
      ]
    );

    # These derivations are used for the Daedalus installer.
    daedalus-jormungandr = with jobs; {
      linux = native.cardano-wallet-jormungandr.x86_64-linux;
      macos = native.cardano-wallet-jormungandr.x86_64-darwin;
      windows = x86_64-pc-mingw32.cardano-wallet-jormungandr.x86_64-linux;
    };

    # Windows release ZIP archive
    cardano-wallet-jormungandr-win64 = import ./nix/windows-release.nix {
      inherit pkgs project;
      cardano-wallet-jormungandr = jobs.x86_64-pc-mingw32.cardano-wallet-jormungandr.x86_64-linux;
    };

    # This is used for testing the build on windows.
    cardano-wallet-jormungandr-tests-win64 = import ./nix/windows-testing-bundle.nix {
      inherit pkgs project;
      cardano-wallet-jormungandr = jobs.x86_64-pc-mingw32.cardano-wallet-jormungandr.x86_64-linux;
      tests = collectTests jobs.x86_64-pc-mingw32.tests;
      benchmarks = collectTests jobs.x86_64-pc-mingw32.benchmarks;
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
  });

in jobs
