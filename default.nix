############################################################################
#
# Cardano Wallet Nix build
#
# Derivation attributes of this file can be build with "nix-build -A ..."
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
#   https://github.com/input-output-hk/cardano-wallet/wiki/Building#nix-build
#
############################################################################

{ ... }@args:
let
  self = (import ./nix/flake-compat.nix args).defaultNix;
  project = self.legacyPackages.${builtins.currentSystem};
  inherit (project.pkgs.haskell-nix.haskellLib)
    selectProjectPackages
    isProjectPackage
    collectComponents
    collectChecks;

in
self // self.packages.${builtins.currentSystem} // {

  # `tests` are the test suites which have been built.
  tests = collectComponents "tests" isProjectPackage project.hsPkgs;
  # `checks` are the result of executing the tests.
  checks = collectChecks isProjectPackage project.hsPkgs;
  # `benchmarks` are only built, not run.
  benchmarks = collectComponents "benchmarks" isProjectPackage project.hsPkgs;

  private = {
    inherit project;
  };

}
