{ lib
, mkShell
, ruby_2_7
, bundlerEnv
, bundix
, screen
, cardano-wallet ? null
, cardano-node ? null
, cardano-node-deployments ? null
}:

let
  # To update gemset.nix, run:
  #   nix develop ../..#e2e-regen --command bundix
  gems = bundlerEnv {
    name = "gems-cardano-wallet-e2e";
    gemfile = ./Gemfile;
    lockfile = ./Gemfile.lock;
    gemset = ./gemset.nix;
    ruby = ruby_2_7;
  };
in mkShell {
  name = "cardano-wallet-e2e";
  buildInputs = [
    gems
    gems.wrappedRuby
    bundix
    screen
  ] ++ lib.filter (drv: drv != null) [
    cardano-wallet
    cardano-node
  ];
} // lib.optionalAttrs (cardano-node-deployments != null) {
  CARDANO_NODE_CONFIGS = cardano-node-deployments;
}
