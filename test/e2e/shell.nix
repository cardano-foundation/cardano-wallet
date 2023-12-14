{ pkgs ? import ../../nix/default.nix {}
, cardanoWallet ? import ../../default.nix { inherit pkgs; }
# Whether to build cardano-wallet from this source directory and
# include in the shell.
, bins ? true
}:

let
  # To update gemset.nix, run:
  #   nix-shell --arg bins false --run bundix
  gems = pkgs.bundlerEnv {
    name = "gems-cardano-wallet-e2e";
    gemdir = ./.;
    ruby = pkgs.ruby_3_1;
  };
in pkgs.mkShell {
  name = "cardano-wallet-e2e";
  buildInputs = [
    gems
    gems.wrappedRuby
    pkgs.bundix
    pkgs.screen
  ] ++ pkgs.lib.optionals bins [
    cardanoWallet.cardano-wallet
    cardanoWallet.cardano-node
    cardanoWallet.cardano-cli
  ];
  CARDANO_NODE_CONFIGS = pkgs.cardano-node-deployments;
}
