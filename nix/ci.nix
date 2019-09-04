# Hercules CI attribute set
# https://hercules-ci.com/github/input-output-hk/cardano-wallet
# https://docs.hercules-ci.com/hercules-ci/getting-started/minimal-repository/

builtins.mapAttrs (system: _:
  let
    walletPkgs = import ../default.nix { inherit system; };
  in
    walletPkgs.pkgs.recurseIntoAttrs {
      inherit (walletPkgs)
        cardano-wallet-http-bridge
        cardano-wallet-jormungandr
        # fixme: fix failing tests
        # tests
        benchmarks;
    }
) {
  x86_64-linux = {};
  # Uncomment to test build on macOS too
  # x86_64-darwin = {};
}
