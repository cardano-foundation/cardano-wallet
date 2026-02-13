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
    # Updated iserv-proxy with network < 3.2.8.0 constraint
    # (network 3.2.8.0 has socket options wine doesn't support)
    iserv-proxy = {
      url = "github:stable-haskell/iserv-proxy?ref=iserv-syms";
      flake = false;
    };
    haskellNix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.hackage.follows = "hackage";
      inputs.iserv-proxy.follows = "iserv-proxy";
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
    cardano-node-runtime.url = "github:IntersectMBO/cardano-node?ref=10.5.4";
    mithril = {
      url = "github:input-output-hk/mithril?ref=2543.1-hotfix";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
      };
  };

  outputs =
    {
      self,
      nixpkgs,
      nixpkgs-unstable,
      hostNixpkgs,
      flake-utils,
      haskellNix,
      iohkNix,
      CHaP,
      customConfig,
      cardano-node-runtime,
      mithril,
      ...
    }:
    let
      # Import libraries
      lib = import ./nix/lib.nix nixpkgs.lib;
      config = import ./nix/config.nix nixpkgs.lib customConfig;
      inherit (flake-utils.lib) eachSystem mkApp flattenTree;
      inherit (iohkNix.lib) evalService;

      # Definitions
      supportedSystems = import ./nix/supported-systems.nix;
      fix-crypton-x509 =
        final: prev:
        let
          old = prev.haskell-nix;
          fix =
            {
              pkgs,
              buildModules,
              config,
              lib,
              options,
              ...
            }:
            {
              packages =
                { }
                // pkgs.lib.optionalAttrs (pkgs.stdenv.hostPlatform.isDarwin && !pkgs.stdenv.cc.nativeLibc && options.packages ? crypton-x509-system) {
                  # Workaround for broken nixpkgs darwin.security_tool in
                  # Mojave. This mirrors the workaround in nixpkgs
                  # haskellPackages.
                  #
                  # ref:
                  # https://github.com/NixOS/nixpkgs/pull/47676
                  # https://github.com/NixOS/nixpkgs/issues/45042
                  crypton-x509-system.components.library.preBuild = "substituteInPlace System/X509/MacOS.hs --replace security /usr/bin/security";
                };
            };
        in
        {
          haskell-nix = old // {
            defaultModules = old.defaultModules ++ [ fix ];
          };
        };
      overlay = final: prev: {
        cardanoWalletHaskellProject = self.legacyPackages.${final.system};
        inherit (final.cardanoWalletHaskellProject.hsPkgs.cardano-wallet-application.components.exes)
          cardano-wallet
          ;
      };

      nixosModule =
        { pkgs, lib, ... }:
        {
          imports = [ ./nix/nixos/cardano-wallet-service.nix ];
          services.cardano-node.package = lib.mkDefault self.packages.${pkgs.system}.cardano-node;
        };
      nixosModules.cardano-wallet = nixosModule;

      # Define flake outputs for a particular system.
      mkOutputs =
        system:
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
              (import ./nix/overlays/ghc-lib-parser.nix)
              fix-crypton-x509
              overlay
            ];
          };

          inherit (pkgs.stdenv) buildPlatform;

          inherit (pkgs.haskell-nix.haskellLib)
            isProjectPackage
            collectComponents
            collectChecks
            check
            ;

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

          # Git revision for stamping release binaries (injected in
          # release-build.nix, outside the Haskell project, so that
          # changing the commit hash does not invalidate nix caches).
          gitrev =
            if config.gitrev != null then
              config.gitrev
            else
              self.rev or "0000000000000000000000000000000000000000";

          walletProject =
            (import ./nix/haskell.nix CHaP pkgs.haskell-nix nixpkgs-unstable.legacyPackages.${system}
              nodePackages
              mithrilPackages
              set-git-rev.packages.default
              rewrite-libs.packages.default
            ).appendModule
              [
                config.haskellNix
              ];

          mkPackages =
            project:
            let
              coveredProject = project.appendModule { coverage = true; };
              self = {
                # Cardano wallet
                cardano-wallet = import ./nix/release-build.nix {
                  inherit pkgs gitrev;
                  set-git-rev = set-git-rev.packages.default;
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
                inherit (project.hsPkgs.cardano-addresses.components.exes) cardano-address;

                # Cardano
                cardano-cli = nodeProject.hsPkgs.cardano-cli.components.exes.cardano-cli;
                cardano-node = nodeProject.hsPkgs.cardano-node.components.exes.cardano-node // {
                  deployments = pkgs.cardano-node-deployments;
                };

                # Provide db-converter, so daedalus can ship it without needing to
                # pin an ouroborus-network rev.
                inherit (project.hsPkgs.ouroboros-consensus-byron.components.exes) db-converter;

                # Unit test executables (for running with custom RTS flags)
                unit-cardano-wallet-unit = project.hsPkgs.cardano-wallet-unit.components.tests.unit;
                unit-cardano-wallet-primitive = project.hsPkgs.cardano-wallet-primitive.components.tests.test;
                unit-cardano-wallet-secrets = project.hsPkgs.cardano-wallet-secrets.components.tests.test;
                unit-cardano-wallet-network-layer = project.hsPkgs.cardano-wallet-network-layer.components.tests.unit;
                unit-cardano-wallet-test-utils = project.hsPkgs.cardano-wallet-test-utils.components.tests.unit;
                unit-cardano-wallet-launcher = project.hsPkgs.cardano-wallet-launcher.components.tests.unit;
                unit-cardano-wallet-application-tls = project.hsPkgs.cardano-wallet-application-tls.components.tests.unit;
                unit-cardano-numeric = project.hsPkgs.cardano-numeric.components.tests.unit;
                unit-cardano-balance-tx = project.hsPkgs.cardano-balance-tx.components.tests.test;
                unit-cardano-wallet-blackbox-benchmarks = project.hsPkgs.cardano-wallet-blackbox-benchmarks.components.tests.unit;
                unit-delta-chain = project.hsPkgs.delta-chain.components.tests.unit;
                unit-delta-store = project.hsPkgs.delta-store.components.tests.unit;
                unit-delta-table = project.hsPkgs.delta-table.components.tests.unit;
                unit-delta-types = project.hsPkgs.delta-types.components.tests.unit;
                unit-std-gen-seed = project.hsPkgs.std-gen-seed.components.tests.unit;
                unit-wai-middleware-logging = project.hsPkgs.wai-middleware-logging.components.tests.unit;
                unit-benchmark-history = project.hsPkgs.cardano-wallet-benchmarks.components.tests.benchmark-history-test;

                # Combined project coverage report
                testCoverageReport = coveredProject.projectCoverageReport;
                # `tests` are the test suites which have been built.
                tests = lib.removeRecurse (collectComponents "tests" isProjectPackage coveredProject.hsPkgs);
                # `checks` are the result of executing the tests.
                checks = lib.removeRecurse (collectChecks isProjectPackage coveredProject.hsPkgs);
                # `benchmarks` are only built, not run.
                benchmarks = lib.removeRecurse (collectComponents "benchmarks" isProjectPackage project.hsPkgs);
              };
            in
            self;

          # nix run .#<network>/wallet
          mkScripts =
            project:
            flattenTree (
              import ./nix/scripts.nix {
                inherit project evalService;
                customConfigs = [ config ];
              }
            );

          # See the imported file for how to use the docker build.
          mkDockerImage =
            isRelease: packages:
            let
              version = (builtins.head exes).version;
              revision = "c-" + self.shortRev;
              exes = with packages; [
                cardano-wallet
                local-cluster
              ];
            in

            pkgs.callPackage ./nix/docker.nix {
              inherit exes;
              base = with packages; [
                bech32
                cardano-address
                cardano-cli
                cardano-node
                (pkgs.linkFarm "docker-config-layer" [
                  {
                    name = "config";
                    path = pkgs.cardano-node-deployments;
                  }
                ])
              ];
              tag = if isRelease then version else revision;
            };

          mkDevShells = project: rec {
            default = project.shell;
            profiled = (project.appendModule { profiling = true; }).shell;

            docs = pkgs.mkShell {
              name = "cardano-wallet-docs";
              nativeBuildInputs = [
                pkgs.mdbook
                pkgs.mdbook-mermaid
                pkgs.mdbook-admonish
              ];
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
          mkReleaseArtifacts =
            project:
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
              checkReleaseContents =
                jobs:
                map (exe: jobs.${exe}) [
                  "cardano-wallet"
                  "bech32"
                  "cardano-address"
                  "cardano-cli"
                  "cardano-node"
                ];
            in
            lib.optionalAttrs buildPlatform.isLinux {
              linux64.release = import ./nix/release-package.nix {
                inherit pkgs nodeConfigs;
                walletLib = lib;
                exes = linuxReleaseExes;
                platform = "linux64";
                format = "tar.gz";
              };
              win64 =
                let
                  # windows is cross-compiled from linux
                  windowsPackages = mkPackages project.projectCross.ucrt64 // {
                    cardano-cli = cardano-node-runtime.hydraJobs.x86_64-linux.windows.cardano-cli;
                    cardano-node = cardano-node-runtime.hydraJobs.x86_64-linux.windows.cardano-node;
                  };
                in
                {
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
                  # Per-test-exe bundles for Windows CI.
                  tests = let
                    mkTest = name: test:
                      import ./nix/windows-test-exe.nix {
                        inherit pkgs test name;
                      };
                  in {
                    wallet-unit = import ./nix/windows-test-exe.nix {
                      inherit pkgs;
                      name = "wallet-unit";
                      test = windowsPackages.unit-cardano-wallet-unit;
                      extraPkgs = [windowsPackages.cardano-cli];
                      testDataDirs = [
                        ./lib/unit/test/data
                        ./lib/local-cluster/test/data
                      ];
                    };
                    wallet-primitive = import ./nix/windows-test-exe.nix {
                      inherit pkgs;
                      name = "wallet-primitive";
                      test = windowsPackages.unit-cardano-wallet-primitive;
                      testDataDirs = [./lib/primitive/test/data];
                    };
                    wallet-secrets = mkTest "wallet-secrets" windowsPackages.unit-cardano-wallet-secrets;
                    wallet-network-layer = mkTest "wallet-network-layer" windowsPackages.unit-cardano-wallet-network-layer;
                    wallet-test-utils = mkTest "wallet-test-utils" windowsPackages.unit-cardano-wallet-test-utils;
                    wallet-launcher = mkTest "wallet-launcher" windowsPackages.unit-cardano-wallet-launcher;
                    wallet-application-tls = mkTest "wallet-application-tls" windowsPackages.unit-cardano-wallet-application-tls;
                    cardano-numeric = mkTest "cardano-numeric" windowsPackages.unit-cardano-numeric;
                    cardano-balance-tx = import ./nix/windows-test-exe.nix {
                      inherit pkgs;
                      name = "cardano-balance-tx";
                      test = windowsPackages.unit-cardano-balance-tx;
                      testDataDirs = [./lib/balance-tx/test/data];
                    };
                    wallet-blackbox-benchmarks = import ./nix/windows-test-exe.nix {
                      inherit pkgs;
                      name = "wallet-blackbox-benchmarks";
                      test = windowsPackages.unit-cardano-wallet-blackbox-benchmarks;
                      testDataDirs = [./lib/wallet-benchmarks/test/data];
                    };
                    delta-chain = mkTest "delta-chain" windowsPackages.unit-delta-chain;
                    delta-store = mkTest "delta-store" windowsPackages.unit-delta-store;
                    delta-table = mkTest "delta-table" windowsPackages.unit-delta-table;
                    delta-types = mkTest "delta-types" windowsPackages.unit-delta-types;
                    std-gen-seed = mkTest "std-gen-seed" windowsPackages.unit-std-gen-seed;
                    wai-middleware-logging = mkTest "wai-middleware-logging" windowsPackages.unit-wai-middleware-logging;
                  };
                  e2e = import ./nix/windows-test-exe.nix {
                    inherit pkgs;
                    name = "e2e";
                    test = windowsPackages.e2e;
                    extraPkgs = [
                      windowsPackages.cardano-wallet
                      windowsPackages.cardano-node
                      windowsPackages.cardano-cli
                    ];
                  };
                };
            }
            # macos is never cross-compiled
            // lib.optionalAttrs buildPlatform.isMacOS {
              macos-intel = lib.optionalAttrs buildPlatform.isx86_64 {
                release = import ./nix/release-package.nix {
                  inherit pkgs nodeConfigs;
                  walletLib = lib;
                  exes =
                    let
                      macOsPkgs = mkPackages project;
                    in
                    [
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
                  exes =
                    let
                      macOsPkgs = mkPackages project;
                    in
                    [
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
              dockerImage = mkDockerImage true imagePackages;
              dockerTestImage = mkDockerImage false imagePackages;
            }
            //

              (lib.optionalAttrs buildPlatform.isLinux {
                nixosTests = import ./nix/nixos/tests {
                  inherit pkgs;
                  project = walletProject;
                };
              })
            // {
              # Continuous integration builds
              ci.tests.all = pkgs.releaseTools.aggregate {
                name = "cardano-wallet-tests";
                meta.description = "Build (all) tests";
                constituents = lib.collect lib.isDerivation packages.tests;
              };

              ci.benchmarks =
                let
                  collectedBenchmarks = lib.concatMapAttrs (_n: v: v) packages.benchmarks;
                in
                collectedBenchmarks
                // {
                  all = pkgs.releaseTools.aggregate {
                    name = "cardano-wallet-benchmarks";
                    meta.description = "Build all benchmarks";
                    constituents = lib.collect lib.isDerivation collectedBenchmarks;
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
            constituents = lib.collect lib.isDerivation (lib.keepUnitChecks packages.checks);
          };
        };

      systems = eachSystem supportedSystems mkOutputs;
      rev = self.rev or null;
    in
    lib.recursiveUpdate systems { inherit overlay nixosModule nixosModules; }
    // {
      revision = rev;
    };
}
