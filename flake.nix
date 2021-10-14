{
  description = "Cardano Wallet";

  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-2105";
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
      url = "github:input-output-hk/flake-compat/devShells-fix";
      flake = false;
    };
    customConfig.url = "github:input-output-hk/empty-flake";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix, iohkNix, customConfig, ... }:
    let
      inherit (nixpkgs) lib;
      config = import ./nix/config.nix lib customConfig;
      cardanoWalletLib = import ./nix/util.nix { inherit lib; };
      inherit (flake-utils.lib) eachSystem mkApp flattenTree;
      inherit (iohkNix.lib) prefixNamesWith;
      supportedSystems = import ./nix/supported-systems.nix;
      defaultSystem = lib.head supportedSystems;
      overlay = final: prev:
        {
          cardanoWalletHaskellProject = self.legacyPackages.${final.system};
          #inherit (final.cardanoWalletHaskellProject.cardano-addresses-cli.components.exes) cardano-address;
          #inherit (final.cardanoWalletHaskellProject.projectCross.ghcjs.hsPkgs) cardano-addresses-jsapi;
          #inherit (self.packages.${final.system}) cardano-addresses-js cardano-addresses-demo-js;
        };
      # Which exes should be put in the release archives.
      releaseContents = jobs: map (exe: jobs.${exe}) [
        "cardano-wallet"
        "bech32"
        "cardano-address"
        "cardano-cli"
        "cardano-node"
      ];
    in
    {
      inherit overlay;
    } // eachSystem supportedSystems
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
              (final: prev: {
                # TODO: get those into iohkNix.overlays:
                checkStackProject = final.callPackage (iohkNix + "/ci/check-stack-project.nix") { };
              })
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
            collectComponents;

          collectProjectComponentsAsList = type: hsPkgs: lib.attrValues (flattenTree (collectComponents type isProjectPackage hsPkgs));

          baseProject = (import ./nix/haskell.nix pkgs.haskell-nix).appendModule [{
            gitrev =
              if config.gitrev != null
              then config.gitrev
              else self.rev or "0000000000000000000000000000000000000000";
          }
            config.haskellNix];
          project = baseProject.appendModule { coverage = true; };
          profiledProject = baseProject.appendModule { profiling = true; };
          hydraProject = project.appendModule ({ pkgs, ... }: {
            # FIXME: Set in the CI so we don't make mistakes.
            #checkMaterialization = true;
            # on linux, only run test for musl and windows.
            doCheck = pkgs.stdenv.hostPlatform.isMusl || pkgs.stdenv.hostPlatform.isWindows || !pkgs.stdenv.buildPlatform.isLinux;
            # Only run integration tests on non-PR jobsets. Note that
            # the master branch jobset will just re-use the cached Bors
            # staging build and test results.
            doIntegrationCheck = config.pr == "";
            # Sets the anti-cache cookie only when building a jobset for bors.
            cacheTestFailures = config.borsBuild == "";
            # Don't build benchmarks for musl.
            buildBenchmarks = !pkgs.stdenv.hostPlatform.isMusl;
          });

          flakeArgs = {
            crossPlatforms = p: lib.optionals buildPlatform.isLinux (with p; [
              mingwW64
              musl64
            ]);
          };

          flake = project.flake flakeArgs;

          packages = project:
            let self = {
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
              testCoverageReport = project.projectCoverageReport;

            }
            // (lib.optionalAttrs buildPlatform.isLinux {
              # See the imported file for how to use the docker build.
              dockerImage = pkgs.callPackage ./nix/docker.nix {
                exes = with (packages project.projectCross.musl64); [ cardano-wallet local-cluster ];
                base = with (packages project.projectCross.musl64); [
                  bech32
                  cardano-address
                  cardano-cli
                  cardano-node
                  (pkgs.linkFarm "docker-config-layer" [{ name = "config"; path = pkgs.cardano-node-deployments; }])
                ];
              };
            });
            in self;

          devShells = project: rec {
            profiled = profiledProject.shell;
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
          };

          hydraJobs =
            let
              hydraFlake = hydraProject.flake flakeArgs;
              windowsProject = hydraProject.projectCross.mingwW64;
            in
            hydraFlake.packages
            // (packages hydraProject)
            // (prefixNamesWith "checks/" hydraFlake.checks)
            // (flattenTree {
              inherit (hydraFlake) devShell;
              devShells = pkgs.recurseIntoAttrs (devShells hydraProject);
              # Ensure that the project's eval-time GC roots are built and
              # cached by CI.
              roots = pkgs.recurseIntoAttrs
                {
                  project = hydraProject.roots;
                  iohk-nix-utils = pkgs.iohk-nix-utils.roots;
                } // (lib.optionalAttrs buildPlatform.isLinux {
                muslProject = hydraProject.projectCross.musl64.roots;
                windowsProject = windowsProject.roots;
              });
            })
            // (lib.optionalAttrs buildPlatform.isLinux (
              let
                windowsPackages = packages windowsProject;
              in
              {
                cardano-wallet-linux64 = import ./nix/release-package.nix {
                  inherit pkgs;
                  exes = releaseContents (packages hydraProject.projectCross.musl64);
                  platform = "linux64";
                  format = "tar.gz";
                };
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
                  tests = collectProjectComponentsAsList "tests" windowsProject.hsPkgs;
                  benchmarks = collectProjectComponentsAsList "benchmarks" windowsProject.hsPkgs;
                };
                # Build and cache the build script used on Buildkite
                buildkiteScript = import ./.buildkite/default.nix {
                  inherit pkgs;
                };

              }
            )) // (lib.optionalAttrs buildPlatform.isMacOS {
              cardano-wallet-macos64 = import ./nix/release-package.nix {
                inherit pkgs;
                exes = releaseContents hydraJobs;
                platform = "macos64";
                format = "tar.gz";
              };
            }) // (lib.optionalAttrs (system == lib.head supportedSystems) {
              # Only on first supported system (there can only be one required job):
              required = pkgs.releaseTools.aggregate {
                name = "github-required";
                meta.description = "All jobs required to pass CI";
                constituents = let hj = self.hydraJobs; in
                  lib.concatMap lib.attrValues ([
                    hj.devShell
                    hj.cardano-wallet
                    hj.cardano-wallet-linux64
                    hj.cardano-wallet-win64
                    hj.cardano-wallet-tests-win64
                    (hj.cardano-wallet-macos64 or { })
                  ] ++ lib.attrValues (lib.filterAttrs
                    (n: _:
                      lib.hasPrefix "checks/" n
                      || (lib.hasInfix ":benchmarks:" n)
                    )
                    hj));
              };
            });

        in
        lib.recursiveUpdate flake {

          legacyPackages = project;

          # Built by `nix build .`
          defaultPackage = flake.packages."cardano-wallet:exe:cardano-wallet";

          # Run by `nix run .`
          defaultApp = flake.apps."cardano-wallet:exe:cardano-wallet";

          packages = (packages project)
          // (prefixNamesWith "checks/" flake.checks);

          devShells = devShells project;

          hydraJobs = lib.mapAttrs (_: lib.hydraJob) hydraJobs;
        }
      );
}
