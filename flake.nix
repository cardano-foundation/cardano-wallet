{
  description = "Cardano Wallet";

  ############################################################################
  #
  # Cardano Wallet Flake Nix build
  #
  # See ./nix/README.md for more documentation about this file.
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
  #     - etc
  #   - benchmarks - attret of benchmark executables
  #     - cardano-wallet.db
  #     - cardano-wallet.latency
  #     - etc
  #   - dockerImage - tarball of the docker image
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
  # 2024-12-18
  #    Running tests via `nix run` is *discouraged*.
  #    We use `nix` to produce the artifacts,
  #    but we try to no longer run checks on these artifacts
  #    by building a nix derivation (we still do that for unit tests).
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
  #     - benchmarks
  #       - restore
  #       - …
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
    nixpkgs-unstable.url = "nixpkgs/nixos-unstable";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    hostNixpkgs.follows = "nixpkgs";
    CHaP = {
      url = "github:intersectmbo/cardano-haskell-packages?ref=repo";
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
      url = "github:input-output-hk/iohk-nix?rev=911835056d2b48a9ae65b4e3a2925c88a320a6ab";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-compat = {
      url = "github:input-output-hk/flake-compat";
      flake = false;
    };
    customConfig.url = "github:input-output-hk/empty-flake";
    cardano-node-runtime.url = "github:IntersectMBO/cardano-node?ref=10.2.1";
    mithril.url = "github:input-output-hk/mithril?ref=2506.0";
  };

  outputs = { self, nixpkgs, nixpkgs-unstable, hostNixpkgs, flake-utils,
              haskellNix, iohkNix, CHaP, customConfig, cardano-node-runtime,
              mithril , ... }:
    let
      # Import libraries
      lib = import ./nix/lib.nix nixpkgs.lib;
      config = import ./nix/config.nix nixpkgs.lib customConfig;
      inherit (flake-utils.lib) eachSystem mkApp flattenTree;
      inherit (iohkNix.lib) evalService;

      # Definitions
      supportedSystems = import ./nix/supported-systems.nix;
      fix-crypton-x509 = final: prev:
        let
          old = prev.haskell-nix;
          fix = { pkgs, buildModules, config, lib, ... }: {
            packages = { } // pkgs.lib.optionalAttrs
              (pkgs.stdenv.hostPlatform.isDarwin && !pkgs.stdenv.cc.nativeLibc) {
                # Workaround for broken nixpkgs darwin.security_tool in
                # Mojave. This mirrors the workaround in nixpkgs
                # haskellPackages.
                #
                # ref:
                # https://github.com/NixOS/nixpkgs/pull/47676
                # https://github.com/NixOS/nixpkgs/issues/45042
                crypton-x509-system.components.library.preBuild =
                  "substituteInPlace System/X509/MacOS.hs --replace security /usr/bin/security";
              };
          };
        in {
          haskell-nix = old // { defaultModules = old.defaultModules ++ [ fix ]; };
        };
      overlay = final: prev: {
        cardanoWalletHaskellProject = self.legacyPackages.${final.system};
        inherit (final.cardanoWalletHaskellProject.hsPkgs.cardano-wallet-application.components.exes) cardano-wallet;
      };

      nixosModule = { pkgs, lib, ... }: {
        imports = [ ./nix/nixos/cardano-wallet-service.nix ];
        services.cardano-node.package = lib.mkDefault self.packages.${pkgs.system}.cardano-node;
      };
      nixosModules.cardano-wallet = nixosModule;

      # Define flake outputs for a particular system.
      mkOutputs = system:
        let
          pkgs = import nixpkgs {
            inherit system;
            inherit (haskellNix) config;
            overlays = [
              # Same overlay sequence as used by cardano-node
              iohkNix.overlays.crypto
              haskellNix.overlay
              iohkNix.overlays.haskell-nix-extra
              iohkNix.overlays.haskell-nix-crypto
              iohkNix.overlays.cardano-lib
              # Cardano deployments
              (import ./nix/overlays/cardano-deployments.nix)
              # Our own utils (cardanoWalletLib)
              (import ./nix/overlays/common-lib.nix)
              (import ./nix/overlays/basement.nix)
              fix-crypton-x509
              overlay
            ];
          };

          inherit (pkgs.stdenv) buildPlatform;

          inherit (pkgs.haskell-nix.haskellLib)
            isProjectPackage
            collectComponents
            collectChecks
            check;

          mithrilPackages = mithril.packages.${system};
          nodePackages = cardano-node-runtime.packages.${system};
          nodeProject = cardano-node-runtime.project.${system};
          nodeConfigs = lib.fileset.toSource {
            root = ./configs;
            fileset = ./configs;
          };
          set-git-rev = import ./nix/set-git-rev/set-git-rev.nix {
            inherit system;
            inherit nixpkgs flake-utils haskellNix;
          };

          walletProject = (import ./nix/haskell.nix
              CHaP
              pkgs.haskell-nix
              nixpkgs-unstable.legacyPackages.${system}
              nodePackages
              mithrilPackages
              set-git-rev.packages.default
              rewrite-libs.packages.default
            ).appendModule [{
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
                  exe = project.hsPkgs.cardano-wallet-application.components.exes.cardano-wallet;
                  backend = self.cardano-node;
                };
                # Local test cluster and mock metadata server
                inherit (project.hsPkgs.cardano-wallet.components.exes) mock-token-metadata-server;
                inherit (project.hsPkgs.cardano-wallet-benchmarks.components.exes) benchmark-history;
                inherit (project.hsPkgs.local-cluster.components.exes) local-cluster;
                integration-exe = project.hsPkgs.cardano-wallet-integration.components.exes.integration-exe;
                e2e = project.hsPkgs.cardano-wallet-integration.components.tests.e2e;
                inherit (project.hsPkgs.local-cluster.components.exes) test-local-cluster-exe;

                # Adrestia tool belt
                inherit (project.hsPkgs.bech32.components.exes) bech32;
                inherit (project.hsPkgs.cardano-addresses-cli.components.exes) cardano-address;

                # Cardano
                cardano-cli = nodeProject.hsPkgs.cardano-cli.components.exes.cardano-cli;
                cardano-node = nodeProject.hsPkgs.cardano-node.components.exes.cardano-node // {
                  deployments = pkgs.cardano-node-deployments;
                };

                # Provide db-converter, so daedalus can ship it without needing to
                # pin an ouroborus-network rev.
                inherit (project.hsPkgs.ouroboros-consensus-byron.components.exes) db-converter;

                # Combined project coverage report
                testCoverageReport = coveredProject.projectCoverageReport;
                # `tests` are the test suites which have been built.
                tests =
                  lib.removeRecurse (collectComponents "tests" isProjectPackage coveredProject.hsPkgs);
                # `checks` are the result of executing the tests.
                checks = lib.removeRecurse (collectChecks isProjectPackage coveredProject.hsPkgs);
                # `benchmarks` are only built, not run.
                benchmarks =
                  lib.removeRecurse (collectComponents "benchmarks" isProjectPackage project.hsPkgs);
              };
            in
            self;

          # nix run .#<network>/wallet
          mkScripts = project: flattenTree (import ./nix/scripts.nix {
            inherit project evalService;
            customConfigs = [ config ];
          });

          # See the imported file for how to use the docker build.
          mkDockerImage = isRelease: packages:
            let
            version = (builtins.head exes).version;
            revision = "c-" + self.shortRev;
            exes = with packages; [ cardano-wallet local-cluster ];
            in

          pkgs.callPackage ./nix/docker.nix {
            inherit exes;
            base = with packages; [
              bech32
              cardano-address
              cardano-cli
              cardano-node
              (pkgs.linkFarm "docker-config-layer" [{ name = "config"; path = pkgs.cardano-node-deployments; }])
            ];
            tag = if isRelease then version else revision;
          };

          mkDevShells = project: rec {
            default = project.shell;
            profiled = (project.appendModule { profiling = true; }).shell;

            docs = pkgs.mkShell {
              name = "cardano-wallet-docs";
              nativeBuildInputs = [ pkgs.mdbook pkgs.mdbook-mermaid pkgs.mdbook-admonish];
              # allow building the shell so that it can be cached
              phases = [ "installPhase" ];
              installPhase = "echo $nativeBuildInputs > $out";
            };
          };
          rewrite-libs = import ./nix/rewrite-libs/rewrite-libs.nix {
            inherit system;
            inherit nixpkgs flake-utils haskellNix;
          };
          # One ${system} can cross-compile artifacts for other platforms.
          mkReleaseArtifacts = project:
            let # compiling with musl gives us a statically linked executable
              linuxPackages = mkPackages project.projectCross.musl64;
              linuxReleaseExes = [
                linuxPackages.cardano-wallet
                linuxPackages.bech32
                linuxPackages.cardano-address
                cardano-node-runtime.hydraJobs.x86_64-linux.musl.cardano-cli
                cardano-node-runtime.hydraJobs.x86_64-linux.musl.cardano-node
              ];
              # Which exes should be put in the release archives.
              checkReleaseContents = jobs: map (exe: jobs.${exe}) [
                "cardano-wallet"
                "bech32"
                "cardano-address"
                "cardano-cli"
                "cardano-node"
              ];
            in lib.optionalAttrs buildPlatform.isLinux {
              linux64.release =
                import ./nix/release-package.nix {
                  inherit pkgs nodeConfigs;
                  walletLib = lib;
                  exes = linuxReleaseExes;
                  platform = "linux64";
                  format = "tar.gz";
                };
              win64 =
                let
                  # windows is cross-compiled from linux
                  windowsPackages =
                    mkPackages project.projectCross.ucrt64 // {
                      cardano-cli =
                        cardano-node-runtime.hydraJobs.x86_64-linux.windows.cardano-cli;
                      cardano-node =
                        cardano-node-runtime.hydraJobs.x86_64-linux.windows.cardano-node;
                    };
                in {
                  release = import ./nix/release-package.nix {
                    inherit pkgs nodeConfigs;
                    walletLib = lib;
                    exes = [
                      windowsPackages.cardano-wallet
                      windowsPackages.bech32
                      windowsPackages.cardano-address
                      windowsPackages.cardano-cli
                      windowsPackages.cardano-node
                    ];
                    platform = "win64";
                    format = "zip";
                  };
                  # Testing on Windows is done using a collection of executables.
                  tests = import ./nix/windows-testing-bundle.nix {
                    inherit pkgs;
                    cardano-wallet = windowsPackages.cardano-wallet;
                    cardano-cli = windowsPackages.cardano-cli;
                    cardano-node = windowsPackages.cardano-node;
                    tests = lib.collect lib.isDerivation windowsPackages.tests;
                    benchmarks = lib.collect lib.isDerivation windowsPackages.benchmarks;
                  };
                };
            }
            # macos is never cross-compiled
            // lib.optionalAttrs buildPlatform.isMacOS {
              macos-intel = lib.optionalAttrs buildPlatform.isx86_64 {
                release = import ./nix/release-package.nix {
                  inherit pkgs nodeConfigs;
                  walletLib = lib;
                  exes = let macOsPkgs = mkPackages project; in [
                    macOsPkgs.cardano-wallet
                    macOsPkgs.bech32
                    macOsPkgs.cardano-address
                    nodePackages.cardano-cli
                    nodePackages.cardano-node
                  ];
                  platform = "macos-intel";
                  format = "tar.gz";
                  rewrite-libs = rewrite-libs.packages.default;
                };
              };
              macos-silicon = lib.optionalAttrs buildPlatform.isAarch64 {
                release = import ./nix/release-package.nix {
                  inherit pkgs nodeConfigs;
                  walletLib = lib;
                  exes = let macOsPkgs = mkPackages project; in [
                    macOsPkgs.cardano-wallet
                    macOsPkgs.bech32
                    macOsPkgs.cardano-address
                    nodePackages.cardano-cli
                    nodePackages.cardano-node
                  ];
                  platform = "macos-silicon";
                  format = "tar.gz";
                  rewrite-libs = rewrite-libs.packages.default;
                };
              };
            };
          imagePackages = mkPackages walletProject.projectCross.musl64;
        in
        rec {

          legacyPackages = walletProject;

          # Built by `nix build .`
          defaultPackage = packages.cardano-wallet;

          # Run by `nix run .`
          defaultApp = apps.cardano-wallet;

          packages =
             mkPackages walletProject
          // mkScripts walletProject
          // rec {
            dockerImage =
              mkDockerImage true imagePackages;
            dockerTestImage =
              mkDockerImage false imagePackages;
          } //

          (lib.optionalAttrs buildPlatform.isLinux {
            nixosTests = import ./nix/nixos/tests {
              inherit pkgs;
              project = walletProject;
            };
          }) // {
            # Continuous integration builds
            ci.tests.all = pkgs.releaseTools.aggregate {
              name = "cardano-wallet-tests";
              meta.description = "Build (all) tests";
              constituents =
                lib.collect lib.isDerivation packages.tests;
            };

            ci.benchmarks =
              let
                collectedBenchmarks =
                  lib.concatMapAttrs (_n: v: v) packages.benchmarks;
              in
                collectedBenchmarks // {
                  all = pkgs.releaseTools.aggregate {
                    name = "cardano-wallet-benchmarks";
                    meta.description = "Build all benchmarks";
                    constituents =
                      lib.collect lib.isDerivation collectedBenchmarks;
                  };
                };
            ci.artifacts = mkReleaseArtifacts walletProject // {
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

          devShells = mkDevShells walletProject;

          ci.tests.run.unit = pkgs.releaseTools.aggregate {
            name = "tests.run.unit";
            meta.description = "Run unit tests";
            constituents =
              lib.collect lib.isDerivation
                (lib.keepUnitChecks packages.checks);
          };
        };

      systems = eachSystem supportedSystems mkOutputs;
      rev = self.rev or null;
    in
  lib.recursiveUpdate systems { inherit overlay nixosModule nixosModules; } // {
    revision = rev;
  } ;
}
