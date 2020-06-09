Usage: cardano-wallet COMMAND
  Cardano Wallet Command-Line Interface (CLI)

Available OPTIONS:
  -h | --help

Available COMMANDS:
  <a href="#launch">launch</a>                Launch and monitor a wallet server and its chain producers.
  <a href="#serve">serve</a>                 Serve an HTTP API that listens for commands/actions.
  mnemonic
    <a href="#mnemonic-generate">generate</a>            Generate BIP-39 mnemonic words
    <a href="#mnemonic-reward-credentials">reward-credentials</a>  Derive reward account private key from mnemonic.
  wallet
    <a href="#wallet-list">list</a>                List all known wallets
    create
      <a href="#wallet-create-from-mnemonic">from-mnemonic</a>     Create a new wallet using a mnemonic
      <a href="#wallet-create-from-public-key">from-public-key</a>   Create a wallet using a public account key
    <a href="#wallet-get">get</a>                 Fetch a particular wallet
    <a href="#wallet-utxo">utxo</a>                Get a wallet's UTxO distribution
    update
      <a href="#wallet-update-passphrase">passphrase</a>        Update a wallet's master passphrase
      <a href="#wallet-update-name">name</a>              Update a wallet's name
    <a href="#wallet-delete">delete</a>              Forget a wallet and its metadata
  transaction
    <a href="#transaction-create">create</a>              Create a transaction from a known wallet
    <a href="#transaction-fees">fees</a>                Estimate fees for a transaction
    <a href="#transaction-list">list</a>                List the transactions associated with a wallet
    <a href="#transaction-submit">submit</a>              Submit an externally-signed transaction
    <a href="#transaction-forget">forget</a>              Forget a pending transaction with specified id
  address
    <a href="#address-list">list</a>                List all known addresses of a wallet
  stake-pool
      <a href="#stake-pool-list">list</a>              List all known stake pools
  network
    <a href="#network-information">information</a>         View network information
    <a href="#network-parameters">parameters</a>          View network parameters
    <a href="#network-clock">clock</a>               View NTP offset
  key
    <a href="#key-root">root</a>                Extract root extended private key from a mnemonic sentence.
    <a href="#key-child">child</a>               Derive child keys.
    <a href="#key-public">public</a>              Extract public key from a private key.
    <a href="#key-inspect">inspect</a>             Show information about a key.
  <a href="#version">version</a>               Show the program's current version
