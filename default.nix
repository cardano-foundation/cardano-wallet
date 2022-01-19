############################################################################
#
# Cardano Wallet Nix build - legacy flake compatibility layer.
# See flake.nix for native flake instructions.
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
#   https://input-output-hk.github.io/cardano-wallet/dev/Building#nix-build
#
############################################################################

{ ... }@args:
with (import ./nix/flake-compat.nix args);
defaultNix // defaultNix.packages.${builtins.currentSystem} // {
  private.project = defaultNix.legacyPackages.${builtins.currentSystem};
}
