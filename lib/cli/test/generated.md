                           
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
Usage:  
```
###  wallet
Manage wallets.
```
Usage:  wallet COMMAND  -h,--help                Show this help text
```
####  wallet list
List all known wallets.
```
Usage:  wallet list [--port INT]  -h,--help                Show this help text
  --port INT               port used for serving the wallet API. (default: 8090)
```
####  wallet create
Create a new Byron wallet.
```
Usage:  wallet create COMMAND  -h,--help                Show this help text
```
#####  wallet create from-mnemonic
Create a new wallet using a mnemonic.
```
Usage:  wallet create from-mnemonic [--port INT] STRING
[--wallet-style WALLET_STYLE]  -h,--help                Show this help text
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
Usage:  wallet get [--port INT] WALLET_ID  -h,--help                Show this
                                                                    help text
  --port INT               port used for serving the wallet API. (default: 8090)
```
####  wallet update
Update a wallet.
```
Usage:  wallet update COMMAND  -h,--help                Show this help text
```
#####  wallet update name
Update a wallet's name.
```
Usage:  wallet update name [--port INT] WALLET_ID
STRING  -h,--help                Show this help text
  --port INT               port used for serving the wallet API. (default: 8090)
```
#####  wallet update passphrase
Update a wallet's passphrase.
```
Usage:  wallet update passphrase [--port INT]
WALLET_ID  -h,--help                Show this help text
  --port INT               port used for serving the wallet API. (default: 8090)
```
####  wallet delete
Deletes wallet with specified wallet id.
```
Usage:  wallet delete [--port INT] WALLET_ID  -h,--help                Show this
                                                                       help text
  --port INT               port used for serving the wallet API. (default: 8090)
```
####  wallet utxo
Get UTxO statistics for the wallet with specified id.
```
Usage:  wallet utxo [--port INT] WALLET_ID  -h,--help                Show this
                                                                     help text
  --port INT               port used for serving the wallet API. (default: 8090)
```
###  key
Derive and manipulate keys.
```
Usage:  key COMMAND  -h,--help                Show this help text
```
####  key root
Extract root extended private key from a mnemonic sentence.
```
Usage:  key root [--wallet-style WALLET_STYLE] [--encoding KEY-ENCODING]
MNEMONIC_WORD...  -h,--help                Show this help text
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
Usage:  key child --path DER-PATH  -h,--help                Show this help text
  --path DER-PATH          Derivation path e.g. 44H/1815H/0H/0
```
####  key public
Extract the public key from a private key.
```
Usage:  key public   -h,--help                Show this help text
```
####  key inspect
Show information about a key.
```
Usage:  key inspect   -h,--help                Show this help text
```
###  mnemonic
Manage mnemonic phrases.
```
Usage:  mnemonic COMMAND  -h,--help                Show this help text
```
####  mnemonic generate
Generate English BIP-0039 compatible mnemonic words.
```
Usage:  mnemonic generate [--size INT]  -h,--help                Show this help
                                                                 text
  --size INT               number of mnemonic words to generate. (default: 15)
```
####  mnemonic reward-credentials
Derive reward account private key from a given mnemonic.
```
Usage:  mnemonic reward-credentials   -h,--help                Show this help
                                                               text
```
###  network
Manage network.
```
Usage:  network COMMAND  -h,--help                Show this help text
```
####  network information
View network information.
```
Usage:  network information [--port INT]  -h,--help                Show this
                                                                   help text
  --port INT               port used for serving the wallet API. (default: 8090)
```
####  network parameters
View network parameters for the current epoch.
```
Usage:  network parameters [--port INT]  -h,--help                Show this help
                                                                  text
  --port INT               port used for serving the wallet API. (default: 8090)
```
####  network clock
View NTP offset.
```
Usage:  network clock [--port INT]
[--force-ntp-check]  -h,--help                Show this help text
  --port INT               port used for serving the wallet API. (default: 8090)
  --force-ntp-check        When set, will block and force an NTP check with the
                           server. Otherwise, uses an available cached result.
```