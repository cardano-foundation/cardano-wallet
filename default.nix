############################################################################
#
# Cardano Wallet Nix build - legacy flake compatibility layer.
# See flake.nix for further documentation on top-level attributes.
#
############################################################################

{ ... }@args:
with (import ./nix/flake-compat.nix args);
defaultNix // defaultNix.packages.${builtins.currentSystem} // {
  private.project = defaultNix.legacyPackages.${builtins.currentSystem};
}
