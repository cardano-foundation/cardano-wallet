Wallet command line interface (CLI) is a small executable which makes sending requests to wallet backend and node an easy task to do. Over the time CLI will support the whole API https://input-output-hk.github.io/cardano-wallet/api/ . For now, these commands are supported:
 * server
 * mnemonic generate
 * wallet list
 * wallet create
 * wallet update
 * wallet delete
 * transaction create

In the next section we will give examples of these commands.

# Commands

## server

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

## mnemonic

Generates mnemonic words

```
$ cardano-wallet mnemonic generate
```

These words will be used to create a wallet later.

## wallet list

Lists all our wallets

```
$ cardano-wallet wallet list
```

## wallet create

Create a new wallet. This is an interactive command that will prompt you for mnemonic words and password.

```
$ cardano-wallet wallet create --name=foo
Please enter a 15–24 word mnemonic sentence: <enter generated mnemonic words here>
(Enter a blank line if you do not wish to use a second factor.)
Please enter a 9–12 word mnemonic second factor: <skip or enter new mnemonic words here>
Please enter a passphrase: ****************
Enter the passphrase a second time: ****************
```

after this your new wallet will be created

## wallet get

Fetches the wallet with specified wallet id:

```
$ cardano-wallet wallet get 2512a00e9653fe49a44a5886202e24d77eeb998f
```

## wallet update

Updates name of a wallet given wallet id:

```
$ cardano-wallet wallet update 2512a00e9653fe49a44a5886202e24d77eeb998f \
    --name NewName
```

## wallet delete

Deletes wallet with specified wallet id:

```
$ cardano-wallet wallet delete 2512a00e9653fe49a44a5886202e24d77eeb998f
```

## transaction create

Creates and submits a new transaction:

```
cardano-wallet transaction create 2512a00e9653fe49a44a5886202e24d77eeb998f \
    --payment 22@Ae2tdPwUPEZ...nRtbfw6EHRv1D \
    --payment 5@Ae2tdPwUPEZ7...pVwEPhKwseVvf
```

This creates a transaction that sends 22 lovelace to `Ae2tdPwUPEZ...nRtbfw6EHRv1D` and 5 lovelace to `Ae2tdPwUPEZ7...pVwEPhKwseVvf` from wallet 2512a00e9653fe49a44a5886202e24d77eeb998f .

