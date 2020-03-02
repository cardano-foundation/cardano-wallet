{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
# Import pinned Nixpkgs with iohk-nix and Haskell.nix overlays
, pkgs ? import ./nix/default.nix { inherit system crossSystem config sourcesOverride; }
# Use this git revision for stamping executables
, gitrev ? pkgs.commonLib.commitIdFromGitRepoOrZero ./.git
# Use this to reference local sources rather than the pinned versions (see nix/default.nix)
, sourcesOverride ? {}
}:

with pkgs; with commonLib; with pkgs.haskell-nix.haskellLib;

let
  haskellPackages = cardanoWalletHaskellPackages;

  filterCardanoPackages = pkgs.lib.filterAttrs (_: package: isCardanoWallet package);
  getPackageChecks = pkgs.lib.mapAttrs (_: package: package.checks);

  self = {
    inherit pkgs commonLib src haskellPackages stackNixRegenerate;
    inherit (jmPkgs) jormungandr jormungandr-cli;
    inherit (haskellPackages.cardano-wallet-core.identifier) version;

    cardano-wallet-jormungandr = import ./nix/package-jormungandr.nix {
      inherit (haskellPackages.cardano-wallet-jormungandr.components.exes)
        cardano-wallet-jormungandr;
      inherit pkgs jmPkgs gitrev;
      haskellBuildUtils = haskellBuildUtils.package;
    };

    inherit (haskellPackages.cardano-wallet-byron.components.exes) cardano-wallet-byron;

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
        ++ [ self.jormungandr self.jormungandr-cli
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
