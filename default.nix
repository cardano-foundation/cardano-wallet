############################################################################
#
# Cardano Wallet Nix build
#
# Derivation attributes of this file can be build with "nix-build -A ..."
# Discover attribute names using tab-completion in your shell.
#
# Interesting top-level attributes:
#
#   - cardano-wallet-jormungandr - cli executable
#   - cardano-wallet-byron - cli executable
#   - tests - attrset of test-suite executables
#     - cardano-wallet-core.unit
#     - cardano-wallet-jormungandr.integration
#     - etc (layout is PACKAGE.COMPONENT)
#   - checks - attrset of test-suite results
#     - cardano-wallet-core.unit
#     - cardano-wallet-jormungandr.integration
#     - etc
#   - benchmarks - attret of benchmark executables
#     - cardano-wallet-core.db
#     - cardano-wallet-jormungandr.latency
#     - etc
#   - migration-tests - tests db migrations from previous versions
#   - dockerImage - tarball of the docker image
#   - shell - imported by shell.nix
#   - haskellPackages - a Haskell.nix package set of all packages and their dependencies
#     - cardano-wallet-core.components.library
#     - etc (layout is PACKAGE-NAME.components.COMPONENT-TYPE.COMPONENT-NAME)
#
# The attributes of this file are imported by the Hydra jobset and
# mapped into the layout TARGET-SYSTEM.ATTR-PATH.BUILD-SYSTEM.
# See release.nix for more info about that.
#
# Other documentation:
#   https://github.com/input-output-hk/cardano-wallet/wiki/Building#nix-build
#
############################################################################

{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
# Import pinned Nixpkgs with iohk-nix and Haskell.nix overlays
, pkgs ? import ./nix/default.nix { inherit system crossSystem config sourcesOverride; }
# Use this git revision for stamping executables
, gitrev ? pkgs.commonLib.commitIdFromGitRepoOrZero ./.git
# Use this to reference local sources rather than the niv pinned versions (see nix/default.nix)
, sourcesOverride ? {}
}:

# commonLib includes iohk-nix utilities, our util.nix and nixpkgs lib.
with pkgs; with commonLib; with pkgs.haskell-nix.haskellLib;

let
  src = pkgs.haskell-nix.cleanSourceHaskell {
    src = ./.;
    name = "cardano-wallet-src";
  };

  haskellPackages = import ./nix/haskell.nix {
    inherit config lib stdenv pkgs buildPackages;
    inherit (pkgs) haskell-nix;
    inherit src;
  };

  filterCardanoPackages = lib.filterAttrs (_: package: isCardanoWallet package);
  getPackageChecks = lib.mapAttrs (_: package: package.checks);

  self = {
    inherit pkgs commonLib src haskellPackages stackNixRegenerate;
    inherit (jmPkgs) jormungandr jormungandr-cli;
    inherit (pkgs.cardanoNodePkgs) cardano-node;
    inherit (haskellPackages.cardano-wallet-core.identifier) version;

    cardano-wallet-jormungandr = import ./nix/package-jormungandr.nix {
      inherit (haskellPackages.cardano-wallet-jormungandr.components.exes)
        cardano-wallet-jormungandr;
      inherit pkgs jmPkgs gitrev;
      haskellBuildUtils = haskellBuildUtils.package;
    };

    cardano-wallet-byron = import ./nix/package-cardano-node.nix {
      inherit pkgs gitrev;
      haskellBuildUtils = haskellBuildUtils.package;
      exe = haskellPackages.cardano-wallet-byron.components.exes.cardano-wallet-byron;
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
        ++ [ self.jormungandr self.jormungandr-cli
             self.cardano-node
             pkgs.pkgconfig pkgs.sqlite-interactive
             pkgs.cabal-install pkgs.pythonPackages.openapi-spec-validator ];
      meta.platforms = lib.platforms.unix;
    };
    stackShell = import ./nix/stack-shell.nix {
      walletPackages = self;
    };
  };

in
  self
