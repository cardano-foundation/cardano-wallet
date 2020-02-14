{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
# Import IOHK common nix lib
, iohkLib ? import ./nix/iohk-common.nix { inherit system crossSystem config; }
# Use pinned Nixpkgs with Haskell.nix overlay
, pkgs ? import ./nix/nixpkgs-haskell.nix  { inherit system crossSystem config; }
# Use this git revision for stamping executables
, gitrev ? iohkLib.commitIdFromGitRepoOrZero ./.git
}:

with import ./nix/util.nix { inherit pkgs; };

let
  src = pkgs.haskell-nix.cleanSourceHaskell {
    src = ./.;
    name = "cardano-wallet-src";
  };

  iohk-nix-rust = import (import ./nix/sources.nix {}).iohk-nix { inherit system crossSystem config; };
  jmPkgs = import ./nix/jormungandr.nix { iohkLib = iohk-nix-rust; };
  inherit (jmPkgs) jormungandr jormungandr-cli;

  haskellPackages = import ./nix/pkgs.nix {
    inherit pkgs src jmPkgs;
  };

  filterCardanoPackages = pkgs.lib.filterAttrs (_: package: isCardanoWallet package);
  getPackageChecks = pkgs.lib.mapAttrs (_: package: package.checks);

  self = {
    inherit pkgs iohkLib src haskellPackages;
    inherit jormungandr jormungandr-cli;
    inherit (haskellPackages.cardano-wallet-core.identifier) version;

    cardano-wallet-jormungandr = import ./nix/package-jormungandr.nix {
      inherit (haskellPackages.cardano-wallet-jormungandr.components.exes)
        cardano-wallet-jormungandr;
      inherit pkgs jmPkgs gitrev;
      haskellBuildUtils = iohkLib.haskellBuildUtils.package;
    };

    # `tests` are the test suites which have been built.
    tests = collectComponents "tests" isCardanoWallet haskellPackages;
    # `checks` are the result of executing the tests.
    checks = pkgs.recurseIntoAttrs (getPackageChecks (filterCardanoPackages haskellPackages));
    # `benchmarks` are only built, not run.
    benchmarks = collectComponents "benchmarks" isCardanoWallet haskellPackages;
    # `migration-tests` build previous releases then check if the database successfully upgrades.
    migration-tests = import ./nix/migration-tests.nix { inherit system crossSystem config pkgs; };

    dockerImage = pkgs.callPackage ./nix/docker.nix {
      inherit (self) cardano-wallet-jormungandr;
    };

    shell = haskellPackages.shellFor {
      name = "cardano-wallet-shell";
      packages = ps: with ps; [
        bech32
        cardano-wallet-cli
        cardano-wallet-core
        cardano-wallet-core-integration
        cardano-wallet-byron
        cardano-wallet-jormungandr
        cardano-wallet-launcher
        cardano-wallet-test-utils
        text-class
      ];
      buildInputs = (with pkgs.haskell-nix.haskellPackages; [
          weeder.components.exes.weeder
          hlint.components.exes.hlint
        ])
        ++ [(pkgs.callPackage ./nix/stylish-haskell.nix {})]
        ++ (with iohkLib; [ openapi-spec-validator ])
        ++ [ jormungandr jormungandr-cli
             pkgs.pkgconfig pkgs.sqlite-interactive
             pkgs.cabal-install ];
      meta.platforms = pkgs.lib.platforms.unix;
    };
    stackShell = import ./nix/stack-shell.nix {
      walletPackages = self;
    };
  };

in
  self
