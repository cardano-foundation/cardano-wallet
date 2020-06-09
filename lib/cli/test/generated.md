                           
   wallet                  Manage wallets.
    list                   List all known wallets.
    create                 Create a new Byron wallet.
     from-mnemonic         Create a new wallet using a mnemonic.
    get                    Fetch the wallet with specified id.
    update                 Update a wallet.
     name                  Update a wallet's name.
     passphrase            Update a wallet's passphrase.
    delete                 Deletes wallet with specified wallet id.
    utxo                   Get UTxO statistics for the wallet with specified id.
   key                     Derive and manipulate keys.
    root                   Extract root extended private key from a mnemonic
  sentence.
    child                  Derive child keys.
    public                 Extract the public key from a private key.
    inspect                Show information about a key.
   mnemonic                Manage mnemonic phrases.
    generate               Generate English BIP-0039 compatible mnemonic words.
    reward-credentials     Derive reward account private key from a given
  mnemonic.
   network                 Manage network.
    information            View network information.
    parameters             View network parameters for the current epoch.
    clock                  View NTP offset.
## 

```

```
###  wallet
Manage wallets.
```
  -h,--help                Show this help text
```
####  wallet list
List all known wallets.
```
  -h,--help                Show this help text
  --port INT               port used for serving the wallet API. (default: 8090)
```
####  wallet create
Create a new Byron wallet.
```
  -h,--help                Show this help text
```
#####  wallet create from-mnemonic
Create a new wallet using a mnemonic.
```
  -h,--help                Show this help text
  --port INT               port used for serving the wallet API. (default: 8090)
  --wallet-style WALLET_STYLE
                           Any of the following (default: icarus)
                             random (12 mnemonic words)
                             icarus (15 mnemonic words)
                             trezor (12, 15, 18, 21 or 24 mnemonic words)
                             ledger (12, 15, 18, 21 or 24 mnemonic words)
```
####  wallet get
Fetch the wallet with specified id.
```
  -h,--help                Show this help text
  --port INT               port used for serving the wallet API. (default: 8090)
```
####  wallet update
Update a wallet.
```
  -h,--help                Show this help text
```
#####  wallet update name
Update a wallet's name.
```
  -h,--help                Show this help text
  --port INT               port used for serving the wallet API. (default: 8090)
```
#####  wallet update passphrase
Update a wallet's passphrase.
```
  -h,--help                Show this help text
  --port INT               port used for serving the wallet API. (default: 8090)
```
####  wallet delete
Deletes wallet with specified wallet id.
```
  -h,--help                Show this help text
  --port INT               port used for serving the wallet API. (default: 8090)
```
####  wallet utxo
Get UTxO statistics for the wallet with specified id.
```
  -h,--help                Show this help text
  --port INT               port used for serving the wallet API. (default: 8090)
```
###  key
Derive and manipulate keys.
```
  -h,--help                Show this help text
```
####  key root
Extract root extended private key from a mnemonic sentence.
```
  -h,--help                Show this help text
  --wallet-style WALLET_STYLE
                           Any of the following (default: icarus)
                             icarus (15 mnemonic words)
                             trezor (12, 15, 18, 21 or 24 mnemonic words)
                             ledger (12, 15, 18, 21 or 24 mnemonic words)
  --encoding KEY-ENCODING  Either 'hex' or 'bech32' (default: hex)
```
####  key child
Derive child keys.
```
  -h,--help                Show this help text
  --path DER-PATH          Derivation path e.g. 44H/1815H/0H/0
```
####  key public
Extract the public key from a private key.
```
  -h,--help                Show this help text
```
####  key inspect
Show information about a key.
```
  -h,--help                Show this help text
```
###  mnemonic
Manage mnemonic phrases.
```
  -h,--help                Show this help text
```
####  mnemonic generate
Generate English BIP-0039 compatible mnemonic words.
```
  -h,--help                Show this help text
  --size INT               number of mnemonic words to generate. (default: 15)
```
####  mnemonic reward-credentials
Derive reward account private key from a given mnemonic.
```
  -h,--help                Show this help text
```
###  network
Manage network.
```
  -h,--help                Show this help text
```
####  network information
View network information.
```
  -h,--help                Show this help text
  --port INT               port used for serving the wallet API. (default: 8090)
```
####  network parameters
View network parameters for the current epoch.
```
  -h,--help                Show this help text
  --port INT               port used for serving the wallet API. (default: 8090)
```
####  network clock
View NTP offset.
```
  -h,--help                Show this help text
  --port INT               port used for serving the wallet API. (default: 8090)
  --force-ntp-check        When set, will block and force an NTP check with the
                           server. Otherwise, uses an available cached result.
```