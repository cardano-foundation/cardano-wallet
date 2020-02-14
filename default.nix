{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
# Import IOHK common nix lib
, sourcesOverride ? {}
# Use pinned Nixpkgs with Haskell.nix overlay
, pkgs ? import ./nix { inherit system crossSystem config sourcesOverride; }
# Use this git revision for stamping executables
, gitrev ? pkgs.commonLib.commitIdFromGitRepoOrZero ./.git
}:

with pkgs; with commonLib; with pkgs.haskell-nix.haskellLib;

let
  inherit (jmPkgs) jormungandr jormungandr-cli;

  haskellPackages = cardanoWalletHaskellPackages;

  filterCardanoPackages = pkgs.lib.filterAttrs (_: package: isCardanoWallet package);
  getPackageChecks = pkgs.lib.mapAttrs (_: package: package.checks);

  self = {
    inherit pkgs commonLib src haskellPackages;
    inherit jormungandr jormungandr-cli;
    inherit (haskellPackages.cardano-wallet-core.identifier) version;

    cardano-wallet-jormungandr = import ./nix/package-jormungandr.nix {
      inherit (haskellPackages.cardano-wallet-jormungandr.components.exes)
        cardano-wallet-jormungandr;
      inherit pkgs jmPkgs gitrev;
      haskellBuildUtils = haskellBuildUtils.package;
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
        ++ [ jormungandr jormungandr-cli
             pkgs.pkgconfig pkgs.sqlite-interactive
             pkgs.cabal-install pkgs.pythonPackages.openapi-spec-validator ];
      meta.platforms = pkgs.lib.platforms.unix;
    };
    stackShell = import ./nix/stack-shell.nix {
      walletPackages = self;
    };
  };

in
  self
