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
  #     - cardano-wallet-core.unit
  #     - cardano-wallet.integration
  #     - etc (layout is PACKAGE.COMPONENT)
  #   - checks - attrset of test-suite results
  #     - cardano-wallet-core.unit
  #     - cardano-wallet.integration
  #     - etc
  #   - benchmarks - attret of benchmark executables
  #     - cardano-wallet-core.db
  #     - cardano-wallet.latency
  #     - etc
  #   - dockerImage - tarball of the docker image
  #
  # Other documentation:
  #   https://input-output-hk.github.io/cardano-wallet/dev/Building#nix-build
  #
  ############################################################################

  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-2111";
    hostNixpkgs.follows = "nixpkgs";
    haskellNix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils.url = "github:numtide/flake-utils";
    iohkNix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-compat = {
      url = "github:input-output-hk/flake-compat/fixes";
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

  outputs = { self, nixpkgs, hostNixpkgs, flake-utils, haskellNix, iohkNix, customConfig, emanote, ... }:
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

      mkRequiredJob = hydraJobs:
        let
          nonRequiredPaths = map lib.hasPrefix [ ];
        in
        self.legacyPackages.${lib.head supportedSystems}.pkgs.releaseTools.aggregate {
          name = "github-required";
          meta.description = "All jobs required to pass CI";
          constituents = lib.collect lib.isDerivation (lib.mapAttrsRecursiveCond (v: !(lib.isDerivation v))
            (path: value:
              let stringPath = lib.concatStringsSep "." path; in if (lib.any (p: p stringPath) nonRequiredPaths) then { } else value)
            hydraJobs);
        };

      mkHydraJobs = systemsJobs:
        let hydraJobs = lib.foldl' lib.mergeAttrs { } (lib.attrValues systemsJobs);
        in
        hydraJobs // {
          required = mkRequiredJob hydraJobs;
        };

      systems = eachSystem supportedSystems
        (system:
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
                # Haskell build tools
                (import ./nix/overlays/build-tools.nix)
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
              collectChecks;

            project = (import ./nix/haskell.nix pkgs.haskell-nix).appendModule [{
              gitrev =
                if config.gitrev != null
                then config.gitrev
                else self.rev or "0000000000000000000000000000000000000000";
            }
              config.haskellNix];
            profiledProject = project.appendModule { profiling = true; };
            hydraProject = project.appendModule ({ pkgs, ... }: {
              # FIXME: Set in the CI so we don't make mistakes.
              #checkMaterialization = true;
              # Don't build benchmarks for musl.
              buildBenchmarks = !pkgs.stdenv.hostPlatform.isMusl;
            });
            hydraProjectBors = hydraProject.appendModule ({ pkgs, ... }: {
              # Sets the anti-cache cookie only when building a jobset for bors.
              cacheTestFailures = false;
            });
            hydraProjectPr = hydraProject.appendModule ({ pkgs, ... }: {
              # Don't run integration tests on PR jobsets. Note that
              # the master branch jobset will just re-use the cached Bors
              # staging build and test results.
              doIntegrationCheck = false;
            });

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
                  inherit (project.hsPkgs.cardano-wallet.components.exes)
                    local-cluster
                    mock-token-metadata-server;

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
                  inherit (project.hsPkgs.ouroboros-consensus-byron.components.exes)
                    db-converter;

                  # Combined project coverage report
                  testCoverageReport = coveredProject.projectCoverageReport;
                  # `tests` are the test suites which have been built.
                  tests = removeRecurse (collectComponents "tests" isProjectPackage coveredProject.hsPkgs);
                  # `checks` are the result of executing the tests.
                  checks = removeRecurse (collectChecks isProjectPackage coveredProject.hsPkgs);
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
              stack = cabal.overrideAttrs (old: {
                name = "cardano-wallet-stack-env";
                nativeBuildInputs = old.nativeBuildInputs ++ [ pkgs.stack ];
                # Build environment setup copied from
                # <nixpkgs/pkgs/development/haskell-modules/generic-stack-builder.nix>
                STACK_PLATFORM_VARIANT = "nix";
                STACK_IN_NIX_SHELL = 1;
                STACK_IN_NIX_EXTRA_ARGS = config.stackExtraArgs;
              });
              docs = pkgs.mkShell {
                name = "cardano-wallet-docs";
                nativeBuildInputs = [ emanote.defaultPackage.${system} pkgs.yq ];
                # allow building the shell so that it can be cached in hydra
                phases = [ "installPhase" ];
                installPhase = "echo $nativeBuildInputs > $out";
              };
            };

            mkSystemHydraJobs = hydraProject: lib.optionalAttrs buildPlatform.isLinux
              rec {
                linux = {
                  # Don't run tests on linux native, because they are run for linux musl.
                  native = removeAttrs (mkPackages hydraProject) [ "checks" "testCoverageReport" ] // {
                    scripts = mkScripts hydraProject;
                    shells = (mkDevShells hydraProject) // {
                      default = hydraProject.shell;
                    };
                    # Build and cache the build script used on Buildkite
                    buildkiteScript = import ./.buildkite/default.nix {
                      inherit pkgs;
                      stackShell = linux.native.shells.stack;
                    };
                    internal.roots = {
                      project = hydraProject.roots;
                      iohk-nix-utils = pkgs.iohk-nix-utils.roots;
                    };
                    nixosTests = import ./nix/nixos/tests {
                      inherit pkgs;
                      project = hydraProject;
                    };
                  };
                  musl =
                    let
                      project = hydraProject.projectCross.musl64;
                      packages = mkPackages project;
                    in
                    packages // {
                      dockerImage = mkDockerImage packages;
                      internal.roots = {
                        project = project.roots;
                      };
                      cardano-wallet-linux64 = import ./nix/release-package.nix {
                        inherit pkgs;
                        exes = releaseContents packages;
                        platform = "linux64";
                        format = "tar.gz";
                      };
                    };
                  windows =
                    let
                      project = hydraProject.projectCross.mingwW64;
                      # Remove the test coverage report - only generate that for Linux musl.
                      windowsPackages = removeAttrs (mkPackages project) [ "testCoverageReport" ];
                    in
                    windowsPackages // {
                      cardano-wallet-win64 = import ./nix/release-package.nix {
                        inherit pkgs;
                        exes = releaseContents windowsPackages;
                        platform = "win64";
                        format = "zip";
                      };
                      # This is used for testing the build on windows.
                      cardano-wallet-tests-win64 = import ./nix/windows-testing-bundle.nix {
                        inherit pkgs;
                        cardano-wallet = windowsPackages.cardano-wallet;
                        cardano-node = windowsPackages.cardano-node;
                        cardano-cli = windowsPackages.cardano-cli;
                        tests = lib.collect lib.isDerivation windowsPackages.tests;
                        benchmarks = lib.collect lib.isDerivation windowsPackages.benchmarks;
                      };
                      internal.roots = {
                        project = project.roots;
                      };
                    };
                };
              } // (lib.optionalAttrs buildPlatform.isMacOS {
                macos.intel = lib.optionalAttrs buildPlatform.isx86_64 (let
                  packages = mkPackages hydraProject;
                in packages // {
                  cardano-wallet-macos-intel = import ./nix/release-package.nix {
                    inherit pkgs;
                    exes = releaseContents packages;
                    platform = "macos-intel";
                    format = "tar.gz";
                  };
                  shells = mkDevShells hydraProject // {
                    default = hydraProject.shell;
                  };
                  scripts = mkScripts hydraProject;
                  internal.roots = {
                    project = hydraProject.roots;
                    iohk-nix-utils = pkgs.iohk-nix-utils.roots;
                  };
                });

                macos.silicon = lib.optionalAttrs buildPlatform.isAarch64 (let
                  packages = mkPackages hydraProject;
                in packages // {
                  cardano-wallet-macos-silicon = import ./nix/release-package.nix {
                    inherit pkgs;
                    exes = releaseContents packages;
                    platform = "macos-silicon";
                    format = "tar.gz";
                  };
                  shells = mkDevShells hydraProject // {
                    default = hydraProject.shell;
                  };
                  scripts = mkScripts hydraProject;
                  internal.roots = {
                    project = hydraProject.roots;
                    iohk-nix-utils = pkgs.iohk-nix-utils.roots;
                  };
                });
              });
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
              inherit (pkgs) sha256map-regenerate checkStackProject;
              inherit (project.stack-nix.passthru) generateMaterialized;
              buildToolsGenerateMaterialized = pkgs.haskell-build-tools.regenerateMaterialized;
              iohkNixGenerateMaterialized = pkgs.iohk-nix-utils.regenerateMaterialized;
            } // (lib.optionalAttrs buildPlatform.isLinux {
              nixosTests = import ./nix/nixos/tests {
                inherit pkgs project;
              };
            });

            apps = lib.mapAttrs (n: p: { type = "app"; program = p.exePath or "${p}/bin/${p.name or n}"; }) packages;

            devShell = project.shell;

            devShells = mkDevShells project;

            systemHydraJobs = mkSystemHydraJobs hydraProject;
            systemHydraJobsPr = mkSystemHydraJobs hydraProjectPr;
            systemHydraJobsBors = mkSystemHydraJobs hydraProjectBors;
          });

    in
    lib.recursiveUpdate (removeAttrs systems [ "systemHydraJobs" "systemHydraJobsPr" "systemHydraJobsBors" ])
      {
        inherit overlay nixosModule nixosModules;
        hydraJobs = mkHydraJobs systems.systemHydraJobs;
        hydraJobsPr = mkHydraJobs systems.systemHydraJobsPr;
        hydraJobsBors = mkHydraJobs systems.systemHydraJobsBors;
      }
  ;
}
