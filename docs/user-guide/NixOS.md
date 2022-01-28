---
order: 2
---

# NixOS service

## cardano-wallet NixOS module

A nixos service definition for cardano-wallet server is available by importing the flake `nixosModule` attribute into your nixos configuration. Or by importing [`nix/nixos`](https://github.com/input-output-hk/cardano-wallet/tree/master/nix/nixos).

Then cardano-wallet server can then be activated and configured:

```nix
({config, ...}: {
  imports = [
    inputs.cardano-wallet.nixosModule
  ];
  services.config.cardano-wallet = {
    enable = true;
    walletMode = "mainnet";
    nodeSocket = config.services.cardano-node.socketPath;
    poolMetadataFetching = {
      enable = true;
      smashUrl = "https://smash.cardano-mainnet.iohk.io";
    };
    tokenMetadataServer = "https://tokens.cardano.org";
  };
})
```

See [`nix/nixos/cardano-wallet-service.nix`](https://github.com/input-output-hk/cardano-wallet/tree/master/nix/nixos/cardano-wallet-service.nix) for the other configuration options (such as the `genesisFile` option to run cardano-wallet on a testnet) and complete documentation.
