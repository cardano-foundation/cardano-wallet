# Wallet Identifiers (WalletId)

The `WalletId` is a hexadecimal string derived from the wallet's mnemonic.

It is used by the `cardano-wallet` server to refer to specific wallets.

For all wallet types, the `WalletId` is a _blake2b-160_ hash of
something. This hash function produces a 20-byte digest, which becomes
40 hexadecimal digits.

## Shelley Wallets

The `WalletId` is calculated the same way for shared (multi-sig) and non-shared Shelley wallets.

Therefore, each signer in a shared wallet will have a unique `WalletId`, because they have a different mnemonic.

### Full Wallets (the default)

The extended private key is derived from the mnemonic, and then its public key is hashed to produce the `WalletId`.

$$WalletId = \mathrm{base16}(\mathrm{blake2b_{160}}(\mathrm{xpub}(rootXPrv)))$$

### Single Account Wallets

These are wallets for which only an account-level XPrv or XPub is known.

$$WalletId = \mathrm{base16}(\mathrm{blake2b_{160}}(accountXPub))$$

## Byron Wallets

The `WalletId` comes from the extended public key of the wallet root key.

$$WalletId = \mathrm{base16}(\mathrm{blake2b_{160}}(\mathrm{xpub}(rootXPrv)))$$

## Example code

This shell script will produce a Shelley `WalletId` from the mnemonic words provided on stdin.

```bash
#!/usr/bin/env bash

xargs \
  | cardano-address key from-recovery-phrase Shelley \
  | cardano-address key public --with-chain-code \
  | bech32 \
  | xxd -r -p \
  | b2sum -b -l 160 \
  | cut -d' ' -f1
```
