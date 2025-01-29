{
  description = ''
    Shell for the Ruby E2E tests.
  '';

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    wallet.url = "../..";
  };

  outputs = inputs:
    let
      supportedSystems = [
        "x86_64-linux"
        "x86_64-darwin"
        # The `blake2b` Ruby gem does not compile on aarch64-*
        # "aarch64-linux"
        # "aarch64-darwin"
       ]; in
    inputs.flake-utils.lib.eachSystem supportedSystems (system:
      let
        # Imports
        walletPkgs
          = inputs.wallet.packages.${system};
        pkgs
          = inputs.nixpkgs.legacyPackages.${system};
        mithrilClient
          = inputs.wallet.inputs.mithril.packages.${system}.mithril-client-cli;
        cardanoNode
          = walletPkgs.cardano-node.package.components.exes.cardano-node;
        cardanoCli
          = walletPkgs.cardano-cli.package.components.exes.cardano-cli;
        cardanoAddresses
          = inputs.wallet.packages.${system}.cardano-address.package.components.exes.cardano-address;
        cardanoWallet
          = walletPkgs.cardano-wallet;

        # To update gemset.nix, run:
        #   nix develop -c bundix
        gems = pkgs.bundlerEnv {
          name = "gems-cardano-wallet-e2e";
          gemdir = ./.;
          ruby = pkgs.ruby_3_1;
        };
      in {
        packages = { };

        devShells.default = pkgs.mkShell {
          buildInputs = [
            pkgs.curl
            pkgs.jq
            gems
            gems.wrappedRuby
            pkgs.bundix
            pkgs.screen
            pkgs.gnupg
            pkgs.rsync
            pkgs.gnutar
            mithrilClient
            cardanoNode
            cardanoCli
            cardanoAddresses
            cardanoWallet

          ];
          shellHook = ''
            # use this hook to set up additional environment variables
          '';
        };
      }
    );
}
