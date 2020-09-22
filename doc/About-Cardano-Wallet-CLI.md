# About Cardano Wallet Command Line Interface (CLI)

The wallet CLI is a command line interface that provides a convenient way of using the cardano-wallet API. The wallet application programming interface (API) is a web service used to manage wallets, make payments, and update wallet data such as addresses and keys, for instance. The wallet CLI can start the wallet server, or run a number of commands on a running server, and supports most functions of the API itself. 

The intended audience of the CLI are users who run a wallet API server outside of Daedalus and work with the cardano-wallet API directly - these include application developers, stake pool operators, and exchanges.

CLI commands allow you to make and track changes while maintaining the wallet API. The wallet CLI converts commands into corresponding API calls and submits them to a running server. 

## How to set up the wallet CLI

To set up the cardano-wallet CLI, first, go to the latest [wallet releases](https://github.com/input-output-hk/cardano-wallet/releases) in GitHub and download the zip/tar.gz for your platform.

You will find installation instructions after the *new features, improvements, resolved and known issues, and limitations* sections.

> Please note that installation instructions are regularly updated and the latest version will always appear at the top of the list. 

1.  Install the latest `cardano-node` version. You will find an active link in the latest [wallet releases](https://github.com/input-output-hk/cardano-wallet/releases).
    
2.  Download the provided `cardano-wallet` for your platform, and uncompress it in a directory that is on your `$PATH`, for example: `/usr/local/bin` or `%PATH%` on Windows.
    
3.  Start `cardano-wallet --help` and see the available parameters.
    
4.  To verify that the version matches the latest release, run the following command:  
    `$ cardano-wallet version`
    
## How to start the wallet API server

To start the wallet API server, first launch the `cardano-node`. 

**Example on mainnet**

1.  Build `cardano-node` with mainnet configuration:
```
git clone https://github.com/input-output-hk/cardano-node.git
cd cardano-node
nix-build -A scripts.mainnet.node -o mainnet-node-local
```

2.  Start `cardano-node` for mainnet. This will create a directory called `./state-node-mainnet`.

`./mainnet-node-local`

3. Start `cardano-wallet` (in another terminal):
```
cardano-wallet serve \
    --mainnet \
    --node-socket state-node-mainnet/node.socket \
    --database ./wallets-mainnet
```

**Example on testnet**

> Note that for testnets, a Byron genesis file is required, even though the network is in the Shelley era. This is because the chain is synchronized from the beginning of the first era.

1.  Download *byronGenesis* for testnet from [Cardano Configurations](https://hydra.iohk.io/build/4095824/download/1/index.html).
2.  Build `cardano-node` with testnet configuration:
```
git clone https://github.com/input-output-hk/cardano-node.git
cd cardano-node
nix-build -A scripts.testnet.node -o testnet-node-local
```

3. Start `cardano-node` for testnet. This will create a directory called `./state-node-testnet`.

`./testnet-node-local`

4.  Start `cardano-wallet` (in another terminal):
```
cardano-wallet serve \
    --testnet testnet-byron-genesis.json \
    --node-socket state-node-testnet/node.socket \
    --database ./wallets-testnet
```

## How to use the wallet CLI

1.  Launch the wallet CLI with:

`$ cardano-wallet --help`

Then, you can use a list of commands for various purposes, such as:

-   list, create, update, or delete wallets
-   create, submit, forget, or track fees in regards to transactions
-   list, create, and import wallet addresses
-   view network information and parameters
-   manage private and public keys

> **Example 1**
>
> With this command, you can randomly generate a new 24-word recovery phrase:
>
> `$ cardano-wallet recovery-phrase generate`

> **Example 2**
>
> This command creates a new wallet named “MyWallet”, and prompts for a recovery phrase and a spending password:
> 
> `$ cardano-wallet wallet create from-recovery-phrase MyWallet`

Please refer to the [wallet command line interface](https://github.com/input-output-hk/cardano-wallet/wiki/Wallet-command-line-interface/c8121f00c29476784a79fe75c54515b7217d9042) instructions on the cardano-wallet Wiki for more details and code examples.



