The CLI is a proxy to the wallet server, which is required for most commands. Commands are turned into corresponding API calls, and submitted to an up-and-running server. Some commands do not require an active server and can be run "offline". (e.g. 'generate mnemonic')

> :warning: Options are positional (--a --b is not equivalent to --b --a) ! :warning:

<!-- 
ATTENTION:

The left and right chevrons (`<` and `>`) aren't displayed in markdown, nor anything in between. So below, I am using
special unicode characters that look alike: `ᐸ` and `ᐳ`
-->

<pre>
Usage:
  cardano-wallet <a href="#server">server</a> [--port=INT] [--bridge-port=INT]
  cardano-wallet <a href="#mnemonic-generate">mnemonic generate</a> [--size=INT]
  cardano-wallet <a href="#wallet-list">wallet list</a> [--port=INT]
  cardano-wallet <a href="#wallet-create">wallet create</a> [--port=INT] ᐸnameᐳ [--address-pool-gap=INT]
  cardano-wallet <a href="#wallet-get">wallet get</a> [--port=INT] ᐸwallet-idᐳ
  cardano-wallet <a href="#wallet-update">wallet update</a> [--port=INT] ᐸwallet-idᐳ --name=STRING
  cardano-wallet <a href="#wallet-delete">wallet delete</a> [--port=INT] ᐸwallet-idᐳ
  cardano-wallet <a href="#transaction-create">transaction create</a> [--port=INT] ᐸwallet-idᐳ --payment=PAYMENT...
  cardano-wallet <a href="#address-list">address list</a> [--port=INT] ᐸwallet-idᐳ
  cardano-wallet -h | --help
  cardano-wallet --version
</pre> 

> :information_source: The CLI commands for `wallet`, `transaction` and `address` only output valid JSON on `stdout`. So you may redirect the output to a file with `>` or pipe it into utility softwares like `jq`!

> :gift_heart: For bash auto-completion, see [cardano-wallet.sh](https://gist.github.com/KtorZ/b4c6935a6412ac5287129078bef93b13).

# Commands

## server

> `cardano-wallet server [--port=INT] [--bridge-port=INT]`

Launches API that listens for commands/actions. Before launching user should build `cardano-http-bridge` https://github.com/input-output-hk/cardano-http-bridge/ (see details on the provided link). To run `cardano-http-bridge` do:

```
$ cardano-http-bridge start --template testnet --port=8080
```

Then you should launch the API with:

```
$ NETWORK=testnet cardano-wallet server --bridge-port 8080
Wallet backend server listening on: 8090
```

This will launch the API

## mnemonic generate

> `cardano-wallet mnemonic generate [--size=INT]`

Generates mnemonic words

```
$ cardano-wallet mnemonic generate
```

These words will be used to create a wallet later. You may also ask for a specific number of words using the `--size` option:

```
$ cardano-wallet mnemonic generate --size 21
```

## wallet list

> `cardano-wallet wallet list [--port=INT]`

Lists all your wallets:

```
$ cardano-wallet wallet list
```

## wallet create

> `cardano-wallet wallet create [--port=INT] <name> [--address-pool-gap=INT]`

Create a new wallet using a sequential address scheme. This is an interactive command that will prompt you for mnemonic words and password.

```
$ cardano-wallet wallet create "My Wallet"
Please enter a 15–24 word mnemonic sentence: <enter generated mnemonic words here>
(Enter a blank line if you do not wish to use a second factor.)
Please enter a 9–12 word mnemonic second factor: <skip or enter new mnemonic words here>
Please enter a passphrase: ****************
Enter the passphrase a second time: ****************
```

after this your new wallet will be created

## wallet get

> `cardano-wallet wallet get [--port=INT] <wallet-id>`

Fetches the wallet with specified wallet id:

```
$ cardano-wallet wallet get 2512a00e9653fe49a44a5886202e24d77eeb998f
```

## wallet update

> `cardano-wallet wallet update [--port=INT] <wallet-id> --name=STRING

Updates name of a wallet given wallet id:

```
$ cardano-wallet wallet update 2512a00e9653fe49a44a5886202e24d77eeb998f \
    --name NewName
```

## wallet delete

> `cardano-wallet wallet delete [--port=INT] <wallet-id>` 

Deletes wallet with specified wallet id:

```
$ cardano-wallet wallet delete 2512a00e9653fe49a44a5886202e24d77eeb998f
```

## transaction create

> `cardano-wallet transaction create [--port=INT] <wallet-id> --payment=PAYMENT...`

Creates and submits a new transaction:

```
$ cardano-wallet transaction create 2512a00e9653fe49a44a5886202e24d77eeb998f \
    --payment 22@Ae2tdPwUPEZ...nRtbfw6EHRv1D \
    --payment 5@Ae2tdPwUPEZ7...pVwEPhKwseVvf
```

This creates a transaction that sends 22 lovelace to `Ae2tdPwUPEZ...nRtbfw6EHRv1D` and 5 lovelace to `Ae2tdPwUPEZ7...pVwEPhKwseVvf` from wallet with id 2512a00e9653fe49a44a5886202e24d77eeb998f.

## address list

> `cardano-wallet address list [--port=INT] <wallet-id>` 

List all known (used or not) addresses and their corresponding status.

```
$ cardano-wallet list addresses 2512a00e9653fe49a44a5886202e24d77eeb998f
```