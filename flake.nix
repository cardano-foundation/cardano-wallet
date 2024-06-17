{
  description = "Cardano Wallet";

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
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-compat = {
      url = "github:input-output-hk/flake-compat";
      flake = false;
    };
    customConfig.url = "github:input-output-hk/empty-flake";
    cardano-node-runtime.url = "github:IntersectMBO/cardano-node?ref=8.9.3";
  };

  outputs = { self, nixpkgs, nixpkgs-unstable, hostNixpkgs, flake-utils,
              haskellNix, iohkNix, CHaP, customConfig, cardano-node-runtime,
              ... }:
    let
      lib = import ./nix/lib.nix nixpkgs.lib;
      config = import ./nix/config.nix nixpkgs.lib customConfig;
      inherit (flake-utils.lib) eachSystem mkApp flattenTree;
      inherit (iohkNix.lib) evalService;

      supportedSystems = import ./nix/supported-systems.nix;

      overlay = final: prev: {
        cardanoWalletHaskellProject = self.legacyPackages.${final.system};
        inherit (final.cardanoWalletHaskellProject.hsPkgs.cardano-wallet-api.components.exes) cardano-wallet;
        haskell-nix = prev.haskell-nix // {
          extraPkgconfigMappings = prev.haskell-nix.extraPkgconfigMappings // {
            "libblst" = [ "blst" ];
          };
        };
      };

      nixosModule = { pkgs, lib, ... }: {
        imports = [ ./nix/nixos/cardano-wallet-service.nix ];
        services.cardano-node.package = lib.mkDefault self.defaultPackage.${pkgs.system};
      };
      nixosModules.cardano-wallet = nixosModule;

      mkOutputs = system:
        let
          pkgs = import nixpkgs {
            inherit system;
            inherit (haskellNix) config;
            overlays = [
              iohkNix.overlays.utils
              iohkNix.overlays.crypto
              iohkNix.overlays.cardano-lib
              haskellNix.overlay
              iohkNix.overlays.haskell-nix-extra
              (import ./nix/overlays/cardano-deployments.nix)
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

          nodePackages = cardano-node-runtime.packages.${system};
          nodeProject = cardano-node-runtime.project.${system};

          walletProject = (import ./nix/haskell.nix
              CHaP
              pkgs.haskell-nix
              nixpkgs-unstable.legacyPackages.${system}
              nodePackages
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
                cardano-wallet = import ./nix/release-build.nix {
                  inherit pkgs;
                  exe = project.hsPkgs.cardano-wallet-api.components.exes.cardano-wallet;
                  backend = self.cardano-node;
                };
                inherit (project.hsPkgs.cardano-wallet.components.exes) mock-token-metadata-server;
                inherit (project.hsPkgs.local-cluster.components.exes) local-cluster;
                inherit (project.hsPkgs.cardano-wallet-integration.components.exes) integration-exe;
                inherit (project.hsPkgs.local-cluster.components.exes) test-local-cluster-exe;
                inherit (project.hsPkgs.bech32.components.exes) bech32;
                inherit (project.hsPkgs.cardano-addresses-cli.components.exes) cardano-address;
                cardano-cli = nodeProject.hsPkgs.cardano-cli.components.exes.cardano-cli;
                cardano-node = nodeProject.hsPkgs.cardano-node.components.exes.cardano-node // {
                  deployments = pkgs.cardano-node-deployments;
                };
                cardano-wallet-e2e = project.hsPkgs.cardano-wallet-e2e.components.exes.wallet-e2e;
                inherit (project.hsPkgs.ouroboros-consensus-byron.components.exes) db-converter;
                testCoverageReport = coveredProject.projectCoverageReport;
                tests = lib.removeRecurse (collectComponents "tests" isProjectPackage coveredProject.hsPkgs);
                checks = lib.removeRecurse (collectChecks isProjectPackage coveredProject.hsPkgs);
                benchmarks = lib.removeRecurse (collectComponents "benchmarks" isProjectPackage project.hsPkgs);
              };
            in self;

          mkScripts = project: flattenTree (import ./nix/scripts.nix {
            inherit project evalService;
            customConfigs = [ config ];
          });

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
            default = project.shell;
            profiled = (project.appendModule { profiling = true; }).shell;
            docs = pkgs.mkShell {
              name = "cardano-wallet-docs";
              nativeBuildInputs = [ pkgs.mdbook pkgs.mdbook-mermaid pkgs.mdbook-admonish];
              phases = [ "installPhase" ];
              installPhase = "echo $nativeBuildInputs > $out";
            };
          };

          mkReleaseArtifacts = project:
            let 
              linuxPackages = mkPackages project.projectCross.musl64;
              linuxReleaseExes = [
                linuxPackages.cardano-wallet
                linuxPackages.bech32
                linuxPackages.cardano-address
                cardano-node-runtime.hydraJobs.x86_64-linux.musl.cardano-cli
                cardano-node-runtime.hydraJobs.x86_64-linux.musl.cardano-node
              ];
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
                  inherit pkgs;
                  walletLib = lib;
                  exes = linuxReleaseExes;
                  platform = "linux64";
                  format = "tar.gz";
                };
              win64 =
                let
                  windowsPackages =
                    mkPackages project.projectCross.mingwW64 // {
                      cardano-cli =
                        cardano-node-runtime.hydraJobs.x86_64-linux.windows.cardano-cli;
                      cardano-node =
                        cardano-node-runtime.hydraJobs.x86_64-linux.windows.cardano-node;
                    };
                in {
                  release = import ./nix/release-package.nix {
                    inherit pkgs;
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
            // macos is never cross-compiled
            // lib.optionalAttrs buildPlatform.isMacOS {
              macos-intel = lib.optionalAttrs buildPlatform.isx86_64 {
                release = import ./nix/release-package.nix {
                  inherit pkgs;
                  walletLib = lib;
                  exes = let macOsPkgs = mkPackages project; in [
                    macOsPkgs.cardano-wallet
                    macOsPkgs.b                  inherit pkgs;
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
                };
              };
              macos-silicon = lib.optionalAttrs buildPlatform.isAarch64 {
                release = import ./nix/release-package.nix {
                  inherit pkgs;
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
                };
              };
            };
        in
        rec {
          legacyPackages = walletProject;
          defaultPackage = packages.cardano-wallet;
          defaultApp = apps.cardano-wallet;
          packages =
             mkPackages walletProject
          // mkScripts walletProject
          // rec {
            dockerImage =
              mkDockerImage (mkPackages walletProject.projectCross.musl64);
            pushDockerImage = import ./.buildkite/docker-build-push.nix {
              hostPkgs = import hostNixpkgs { inherit system; };
              inherit dockerImage;
              inherit (config) dockerHubRepoName;
            };
            inherit (pkgs) checkCabalProject;
          } // (lib.optionalAttrs buildPlatform.isLinux {
            nixosTests = import ./nix/nixos/tests {
              inherit pkgs;
              project = walletProject;
            };
          }) // {
            ci.tests.all = pkgs.releaseTools.aggregate {
              name = "cardano-wallet-tests";
              meta.description = "Build (all) tests";
              constituents =
                lib.collect lib.isDerivation packages.tests;
            };

            ci.benchmarks =
            packages.benchmarks.cardano-wallet-benchmarks //
            packages.benchmarks.cardano-wallet-blackbox-benchmarks // {
              all = pkgs.releaseTools.aggregate {
                name = "cardano-wallet-benchmarks";
                meta.description = "Build all benchmarks";
                constituents =
                  lib.collect lib.isDerivation packages.benchmarks;
              };
            };
            ci.artifacts = mkReleaseArtifacts walletProject // {
              dockerImage = packages.dockerImage;
            };
          };

          checks = packages.checks;

          mkApp = name: pkg: {
              type = "app";
              program = pkg.exePath or "${pkg}/bin/${pkg.name or name}";
            };
          apps = lib.mapAttrs mkApp packages;

          devShells = mkDevShells walletProject;

          ci.tests.run.unit = pkgs.releaseTools.aggregate
            {
              name = "tests.run.unit";
              meta.description = "Run unit tests";
              constituents =
                lib.collect lib.isDerivation
                  (lib.keepUnitChecks packages.checks);
            };
          ci.tests.run.integration = pkgs.releaseTools.aggregate
            {
              name = "tests.run.integration";
              meta.description = "Run integration tests";
              constituents =
                lib.collect lib.isDerivation
                  (lib.keepIntegrationChecks packages.checks);
            };
        };

      systems = eachSystem supportedSystems mkOutputs;
    in
      lib.recursiveUpdate systems {
        inherit overlay nixosModule nixosModules;
      }
  ;
}
