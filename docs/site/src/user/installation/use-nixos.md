# Running cardano-wallet with NixOS

## Running without installation

The following command will download and run a given release of `cardano-wallet` using Nix:

```console
> nix run github:cardano-foundation/cardano-wallet/v2022-01-18 -- version
v2022-01-18 (git revision: ce772ff33623e2a522dcdc15b1d360815ac1336a)
```

It's also possible to run the very latest version from the master branch on GitHub:

```console
> nix run github:cardano-foundation/cardano-wallet -- --help
...
```

### Wrapper script with pre-configured network settings

To run a wallet on _mainnet_:

```console
> CARDANO_NODE_SOCKET_PATH=../cardano-node/node.socket

> nix run github:cardano-foundation/cardano-wallet#mainnet/wallet
```

## Installing into user profile

```
> nix profile install github:cardano-foundation/cardano-wallet/v2022-01-18
> cardano-wallet version
v2022-01-18 (git revision: ce772ff33623e2a522dcdc15b1d360815ac1336a)
```

## NixOS Module

A nixos service definition for cardano-wallet server is available by importing the flake `nixosModule` attribute into your nixos configuration. Or by importing [`nix/nixos`](https://github.com/cardano-foundation/cardano-wallet/tree/master/nix/nixos).

Then `cardano-wallet` server can then be activated and configured:

```nix
{
  description = "Flake example with cardano-wallet NixOS module";
  inputs.cardano-wallet.url = github:cardano-foundation/cardano-wallet;
  outputs = { self, cardano-wallet }@inputs: {
    nixosModules.example = { config, ...}: {
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
    };
  };
}
```

See [`nix/nixos/cardano-wallet-service.nix`](https://github.com/cardano-foundation/cardano-wallet/tree/master/nix/nixos/cardano-wallet-service.nix) for the other configuration options (such as the `genesisFile` option to run cardano-wallet on a testnet) and complete documentation.
