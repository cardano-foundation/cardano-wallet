{
  description = "Cardano Wallet";

  ############################################################################
  #
  # Cardano Wallet Flake Nix build
  #
  # Nix Flake support instruction: https://nixos.wiki/wiki/Flakes
  #
  # Derivation attributes of this file can be build with "nix build .#<attribute>"
  # Discover attribute names using tab-completion in your shell.
  #
  # Interesting top-level attributes:
  #
  #   - cardano-wallet - cli executable
  #   - tests - attrset of test-suite executables
  #     - cardano-wallet.unit
  #     - cardano-wallet.integration
  #     - etc (layout is PACKAGE.COMPONENT)
  #   - checks - attrset of test-suite results
  #     - cardano-wallet.unit
  #     - cardano-wallet.integration
  #     - etc
  #   - benchmarks - attret of benchmark executables
  #     - cardano-wallet.db
  #     - cardano-wallet.latency
  #     - etc
  #   - dockerImage - tarball of the docker image
  #
  # Other documentation:
  #   https://input-output-hk.github.io/cardano-wallet/dev/Building#nix-build
  #
  ############################################################################

  ############################################################################
  # Continuous Integration (CI)
  #
  # This flake contains a few outputs useful for continous integration.
  # These outputs come in two flavors:
  #
  #   outputs.packages."<system>".ci.*  - build a test, benchmark, …
  #   outputs.apps."<system>".ci.*      - run a test, benchmark, …
  #
  # For building, say all tests, use `nix build`:
  #
  #   nix build .#ci.tests.all
  #
  # For running, say the unit tests use `nix run`:
  #
  #   nix run .#ci.tests.unit
  #
  # (Running an item will typically also build it if it has not been built
  #  in the nix store already.)
  #
  # (In some cases, building or running fails with a segmentation fault;
  #  the nix garbage collection is likely to blame.
  #  Use `GC_DONT_GC=1 nix build …` as workaround.)
  #
  #
  # The CI-related outputs are
  #
  #  - outputs.packages."<system>".ci.
  #     - tests
  #       - all             - build all test executables
  #     - benchmarks
  #       - all             - build all benchmarks
  #       - restore         - build individual benchmark
  #       - …
  #     - artifacts         - artifacts by platform
  #       - linux64.release
  #       - win64
  #         - release
  #         - tests         - bundle of executables for testing on Windows
  #       - macos-intel.release
  #       - macos-silicon.release
  #       - dockerImage
  #  - outputs.apps."<system>".ci.
  #     - tests
  #       - unit            - run the unit tests on this system
  #       - integration     - run the integration tests on this system
  #     - benchmarks
  #       - restore
  #       - …
  #
  # Recommended granularity:
  #
  #  after each commit:
  #    on x86_64-linux:
  #      nix build .#ci.tests.all
  #      nix build .#ci.benchmarks.all
  #      nix build .#ci.artifacts.linux64.release
  #      nix build .#ci.artifacts.win64.release
  #      nix build .#ci.artifacts.win64.tests
  #
  #      nix run   .#ci.tests.unit
  #
  #  before each pull request merge:
  #    on each supported system:
  #      nix build .#ci.benchmarks.all
  #      nix build .#ci.tests.all
  #
  #      nix run   .#ci.tests.unit
  #      nix run   .#ci.tests.integration
  #
  #  nightly:
  #    on x86_64-linux:
  #      nix build  .#ci.artifacts.dockerImage
  #
  #      nix run    .#ci.benchmarks.restore
  #      nix run    .#ci.benchmarks.…
  #
  #    on x65_64-darwin: (macos)
  #      nix build .#ci.artifacts.win64.release
  #
  ############################################################################

  ############################################################################
  # TODO Continuous Integration (CI)
  #
  # Make the flake.nix file *itself* part of continuous integration,
  # i.e. check that `devShells`, `scripts` or `nixosTests` evalute and build
  # correctly.
  ############################################################################

  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    hostNixpkgs.follows = "nixpkgs";
    CHaP = {
      url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
      flake = false;
    };
    haskellNix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.hackage.follows = "hackage";
    };
    hackage = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };
    flake-utils.url = "github:numtide/flake-utils";
    iohkNix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-compat = {
      url = "github:input-output-hk/flake-compat";
      flake = false;
    };
    customConfig.url = "github:input-output-hk/empty-flake";
    emanote = {
      url = "github:srid/emanote";
    };
    ema = {
      url = "github:srid/ema";
    };
  };

  outputs = { self, nixpkgs, hostNixpkgs, flake-utils, haskellNix, iohkNix, CHaP, customConfig, emanote, ... }:
    let
      inherit (nixpkgs) lib;
      config = import ./nix/config.nix lib customConfig;
      inherit (flake-utils.lib) eachSystem mkApp flattenTree;
      removeRecurse = lib.filterAttrsRecursive (n: _: n != "recurseForDerivations");
      inherit (iohkNix.lib) evalService;
      supportedSystems = import ./nix/supported-systems.nix;
      defaultSystem = lib.head supportedSystems;
      overlay = final: prev: {
        cardanoWalletHaskellProject = self.legacyPackages.${final.system};
        inherit (final.cardanoWalletHaskellProject.hsPkgs.cardano-wallet.components.exes) cardano-wallet;
      };
      nixosModule = { pkgs, lib, ... }: {
        imports = [ ./nix/nixos/cardano-wallet-service.nix ];
        services.cardano-node.package = lib.mkDefault self.defaultPackage.${pkgs.system};
      };
      nixosModules.cardano-wallet = nixosModule;
      # Which exes should be put in the release archives.
      releaseContents = jobs: map (exe: jobs.${exe}) [
        "cardano-wallet"
        "bech32"
        "cardano-address"
        "cardano-cli"
        "cardano-node"
      ];

      # Helper functions for separating unit and integration tests
      setEmptyAttrsWithCondition = cond:
        lib.mapAttrsRecursiveCond
          (value: !(lib.isDerivation value)) # do not modify attributes of derivations
          (path: value: if cond path then {} else value);
      keepIntegrationChecks =
        setEmptyAttrsWithCondition
          (path: !lib.any (lib.hasPrefix "integration") path);
      keepUnitChecks =
        setEmptyAttrsWithCondition
          (path: !lib.any (name: name == "unit" || name == "test") path);

      # Define flake outputs for a particular system.
      mkOutputs = system:
        let
          pkgs = import nixpkgs {
            inherit system;
            inherit (haskellNix) config;
            overlays = [
              haskellNix.overlay
              iohkNix.overlays.utils
              iohkNix.overlays.crypto
              iohkNix.overlays.haskell-nix-extra
              iohkNix.overlays.cardano-lib
              # Cardano deployments
              (import ./nix/overlays/cardano-deployments.nix)
              # Other packages overlay
              (import ./nix/overlays/pkgs.nix)
              # Our own utils (cardanoWalletLib)
              (import ./nix/overlays/common-lib.nix)
              overlay
            ];
          };

          inherit (pkgs.stdenv) buildPlatform;

          inherit (pkgs.haskell-nix.haskellLib)
            isProjectPackage
            collectComponents
            collectChecks
            check;

          project = (import ./nix/haskell.nix CHaP pkgs.haskell-nix).appendModule [{
            gitrev =
              if config.gitrev != null
              then config.gitrev
              else self.rev or "0000000000000000000000000000000000000000";
          }
            config.haskellNix];

          mkPackages = project:
            let
              coveredProject = project.appendModule { coverage = true; };
              self = {
                # Cardano wallet
                cardano-wallet = import ./nix/release-build.nix {
                  inherit pkgs;
                  exe = project.hsPkgs.cardano-wallet.components.exes.cardano-wallet;
                  backend = self.cardano-node;
                };
                # Local test cluster and mock metadata server
                inherit (project.hsPkgs.cardano-wallet.components.exes) local-cluster mock-token-metadata-server;

                # Adrestia tool belt
                inherit (project.hsPkgs.bech32.components.exes) bech32;
                inherit (project.hsPkgs.cardano-addresses-cli.components.exes) cardano-address;

                # Cardano
                inherit (project.hsPkgs.cardano-cli.components.exes) cardano-cli;
                cardano-node = project.hsPkgs.cardano-node.components.exes.cardano-node // {
                  deployments = pkgs.cardano-node-deployments;
                };

                # Provide db-converter, so daedalus can ship it without needing to
                # pin an ouroborus-network rev.
                inherit (project.hsPkgs.ouroboros-consensus-byron.components.exes) db-converter;

                # Combined project coverage report
                testCoverageReport = coveredProject.projectCoverageReport;
                # `tests` are the test suites which have been built.
                tests = removeRecurse (collectComponents "tests" isProjectPackage coveredProject.hsPkgs);
                # `checks` are the result of executing the tests.
                checks = removeRecurse (
                  lib.recursiveUpdate
                    (collectChecks isProjectPackage coveredProject.hsPkgs)
                    # Run the integration tests in the previous era too:
                    (
                      let
                        integrationCheck = check coveredProject.hsPkgs.cardano-wallet.components.tests.integration;
                        integrationPrevEraCheck = integrationCheck.overrideAttrs (prev: {
                          preCheck = prev.preCheck + ''
                            export LOCAL_CLUSTER_ERA=alonzo
                          '';
                        });
                      in
                        if integrationCheck.doCheck == true
                        then
                          {
                            cardano-wallet.integration-prev-era = integrationPrevEraCheck;
                          }
                        else
                          {}
                    )
                );
                # `benchmarks` are only built, not run.
                benchmarks = removeRecurse (collectComponents "benchmarks" isProjectPackage project.hsPkgs);
              };
            in
            self;

          # nix run .#<network>/wallet
          mkScripts = project: flattenTree (import ./nix/scripts.nix {
            inherit project evalService;
            customConfigs = [ config ];
          });

          # See the imported file for how to use the docker build.
          mkDockerImage = packages: pkgs.callPackage ./nix/docker.nix {
            exes = with packages; [ cardano-wallet local-cluster ];
            base = with packages; [
              bech32
              cardano-address
              cardano-cli
              cardano-node
              (pkgs.linkFarm "docker-config-layer" [{ name = "config"; path = pkgs.cardano-node-deployments; }])
            ];
          };

          mkDevShells = project: rec {
            profiled = (project.appendModule { profiling = true; }).shell;
            cabal = import ./nix/cabal-shell.nix {
              haskellProject = project;
              inherit (config) withCabalCache ghcVersion;
            };
            docs = pkgs.mkShell {
              name = "cardano-wallet-docs";
              nativeBuildInputs = [ emanote.packages.${system}.default pkgs.yq ];
              # allow building the shell so that it can be cached
              phases = [ "installPhase" ];
              installPhase = "echo $nativeBuildInputs > $out";
            };
          };

          # One ${system} can cross-compile artifacts for other platforms.
          mkReleaseArtifacts = project:
            lib.optionalAttrs buildPlatform.isLinux {
              linux64.release =
                let
                  # compiling with musl gives us a statically linked executable
                  linuxPackages = mkPackages project.projectCross.musl64;
                in
                import ./nix/release-package.nix {
                  inherit pkgs;
                  exes = releaseContents linuxPackages;
                  platform = "linux64";
                  format = "tar.gz";
                };
              win64 =
                let
                  # windows is cross-compiled from linux
                  windowsPackages = mkPackages project.projectCross.mingwW64;
                in {
                  release = import ./nix/release-package.nix {
                    inherit pkgs;
                    exes = releaseContents windowsPackages;
                    platform = "win64";
                    format = "zip";
                  };
                  # Testing on Windows is done using a collection of executables.
                  tests = import ./nix/windows-testing-bundle.nix {
                    inherit pkgs;
                    cardano-wallet = windowsPackages.cardano-wallet;
                    cardano-node = windowsPackages.cardano-node;
                    cardano-cli = windowsPackages.cardano-cli;
                    tests = lib.collect lib.isDerivation windowsPackages.tests;
                    benchmarks = lib.collect lib.isDerivation windowsPackages.benchmarks;
                  };
                };
            }
            # macos is never cross-compiled
            // lib.optionalAttrs buildPlatform.isMacOS {
              macos-intel = lib.optionalAttrs buildPlatform.isx86_64 {
                release = import ./nix/release-package.nix {
                  inherit pkgs;
                  exes = releaseContents (mkPackages project);
                  platform = "macos-intel";
                  format = "tar.gz";
                };
              };
              macos-silicon = lib.optionalAttrs buildPlatform.isAarch64 {
                release = import ./nix/release-package.nix {
                  inherit pkgs;
                  exes = releaseContents (mkPackages project);
                  platform = "macos-silicon";
                  format = "tar.gz";
                };
              };
            };
        in
        rec {

          legacyPackages = project;

          # Built by `nix build .`
          defaultPackage = packages.cardano-wallet;

          # Run by `nix run .`
          defaultApp = apps.cardano-wallet;

          packages = mkPackages project // mkScripts project // rec {
            dockerImage = mkDockerImage (mkPackages project.projectCross.musl64);
            pushDockerImage = import ./.buildkite/docker-build-push.nix {
              hostPkgs = import hostNixpkgs { inherit system; };
              inherit dockerImage;
              inherit (config) dockerHubRepoName;
            };
            inherit (pkgs) checkCabalProject cabalProjectRegenerate;
          } // (lib.optionalAttrs buildPlatform.isLinux {
            nixosTests = import ./nix/nixos/tests {
              inherit pkgs project;
            };
          }) // {
            # Continuous integration builds
            ci.tests.all = pkgs.releaseTools.aggregate {
              name = "cardano-wallet-tests";
              meta.description = "Build (all) tests";
              constituents =
                lib.collect lib.isDerivation packages.tests;
            };
            ci.benchmarks = packages.benchmarks.cardano-wallet // {
              all = pkgs.releaseTools.aggregate {
                name = "cardano-wallet-benchmarks";
                meta.description = "Build all benchmarks";
                constituents =
                  lib.collect lib.isDerivation packages.benchmarks;
              };
            };
            ci.artifacts = mkReleaseArtifacts project // {
              dockerImage = packages.dockerImage;
            };
          };

          # Heinrich: I don't quite understand the 'checks' attribute. See also
          # https://www.reddit.com/r/NixOS/comments/x5cjmz/comment/in0qqm6/?utm_source=share&utm_medium=web2x&context=3
          checks = packages.checks;

          mkApp = name: pkg: {
              type = "app";
              program = pkg.exePath or "${pkg}/bin/${pkg.name or name}";
            };
          apps = lib.mapAttrs mkApp packages;

          devShell = project.shell;

          devShells = mkDevShells project;

          ci.tests.run.unit = pkgs.releaseTools.aggregate
            {
              name = "tests.run.unit";
              meta.description = "Run unit tests";
              constituents =
                lib.collect lib.isDerivation
                  (keepUnitChecks packages.checks);
            };
          ci.tests.run.integration = pkgs.releaseTools.aggregate
            {
              name = "tests.run.integration";
              meta.description = "Run integration tests";
              constituents =
                lib.collect lib.isDerivation
                  (keepIntegrationChecks packages.checks);
            };
        };
      
      systems = eachSystem supportedSystems mkOutputs;
    in
      lib.recursiveUpdate systems {
        inherit overlay nixosModule nixosModules;
      }
  ;
}
