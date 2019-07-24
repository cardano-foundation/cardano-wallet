The CLI is a proxy to the wallet server, which is required for most commands. Commands are turned into corresponding API calls, and submitted to an up-and-running server. Some commands do not require an active server and can be run "offline". (e.g. 'mnemonic generate')

<!-- 
ATTENTION:

The left and right chevrons (`<` and `>`) aren't displayed in markdown, nor anything in between. So below, I am using
special unicode characters that look alike: `ᐸ` and `ᐳ`
-->

<pre>
Usage: cardano-wallet COMMAND
  Cardano Wallet Command-Line Interface (CLI)

Available OPTIONS:
  -h | --help

Available COMMANDS:
  <a href="#launch">launch</a>              Launch and monitor a wallet server and its chain producers.
  <a href="#serve">serve</a>               Serve an HTTP API that listens for commands/actions.
  mnemonic                            
    <a href="#mnemonic-generate">generate</a>          Generate BIP-39 mnemonic words
  wallet
    <a href="#wallet-list">list</a>              List all known wallets
    <a href="#wallet-create">create</a>            Create & restore a wallet from a BIP-39 mnemonic sentence
    <a href="#wallet-get">get</a>               Fetch a particular wallet
    <a href="#wallet-utxo">utxo</a>              Get a wallet's UTxO distribution 
    update
      <a href="#wallet-update-passphrase">passphrase</a>      Update a wallet's master passphrase
      <a href="#wallet-update-metadata">metadata</a>        Update a wallet's metadata 
    <a href="#wallet-delete">delete</a>            Forget a wallet and its metadata
  transaction
    <a href="#transaction-create">create</a>            Create a transaction from a known wallet
    <a href="#transaction-fees">fees</a>              Estimate fees for a transaction
  address
    <a href="#address-list">list</a>              List all known addresses of a wallet
  <a href="#version">version</a>             Show the program's current version
</pre> 

> :information_source: The CLI commands for `wallet`, `transaction` and `address` only output valid JSON on `stdout`. So you may redirect the output to a file with `>` or pipe it into utility softwares like `jq`!

> :gift_heart: For bash/zsh auto-completion, put the following script in your `/etc/bash_completion.d`:
> 
> <details>
> <summary>cardano-wallet.sh</summary>
>
> ```bash
> _cardano-wallet()
> {
>    local CMDLINE
>    local IFS=$'\n'
>    CMDLINE=(--bash-completion-index $COMP_CWORD)
>
>    for arg in ${COMP_WORDS[@]}; do
>        CMDLINE=(${CMDLINE[@]} --bash-completion-word $arg)
>    done
>
>    COMPREPLY=( $(cardano-wallet "${CMDLINE[@]}") )
> }
>
> complete -o filenames -F _cardano-wallet cardano-wallet
> ```
> </details>
# Commands

## launch 

Launches and manages two sub-processes:

- A wallet server (using `cardano-wallet serve`)
- A target chain producer (e.g. [cardano-http-bridge](https://github.com/input-output-hk/cardano-http-bridge), [Jörmungandr](https://github.com/input-output-hk/jormungandr))

This is a shortcut command for those looking into an _out-of-the-box_ solution for running the software.

### via Jörmungandr

> ```
> cardano-wallet launch 
>   ([--random-port] | [--port INT])
>   [--node-port INT]
>   [--state-dir DIR]
>   ([--quiet] | [--verbose])
>   --genesis-block FILE
>   --bft-leaders FILE

- :information_source: Please note that launch will generate a configuration for Jörmungandr in a folder specified by '--state-dir'.
- :information_source: The wallet backend only works with Jörmungandr in BFT mode with a test discrimination.

##### --genesis-block

The genesis block file can be obtained using Jörmungandr's tool command-line `jcli` as follows:

```bash
$ jcli genesis encode --input genesis.yaml > block0.bin
```

Here below is an example of genesis file that can be used for testing:

<details>
  <summary>genesis.yaml</summary>

```yaml
# The Blockchain Configuration defines the different settings
# of the blockchain that cannot be changed once the blockchain
# is started.
blockchain_configuration:
  # The block0-date defines the date the blockchain starts
  # expected value in seconds since UNIX_EPOCH
  block0_date: 1556202057

  # This is the type of dicrimination of the blockchain
  # of this blockchain is meant for production then
  # use 'production' instead.
  #
  # otherwise leave as this
  discrimination: test

  # The initial consensus version:
  #
  # * BFT consensus: bft
  # * Genesis Praos consensus: genesis
  block0_consensus: bft

  # Number of slots in each epoch
  slots_per_epoch: 500

  # The slot duration, in seconds, is the time between the creation
  # of 2 blocks
  slot_duration: 10

  # The number of blocks (*10) per epoch
  epoch_stability_depth: 10

  # A list of Ed25519 Extended PublicKey that represents the
  # BFT leaders encoded as bech32. The order in the list matters.
  consensus_leader_ids:
    - ed25519_pk1haythczarvl75wt6y6fmq0gjz7j9xcyfatwchj523wdawechkd8qp735w5

  # Genesis praos parameter D
  bft_slots_ratio: 0.220

  # Genesis praos active slot coefficient
  # Determines minimum stake required to try becoming slot leader, must be in range (0,1]
  consensus_genesis_praos_active_slot_coeff: 0.22

  # This is the max number of messages allowed in a given Block
  max_number_of_transactions_per_block: 255

  # The fee calculations settings
  #
  # fee(num_bytes, has_certificate) = constant + num_bytes * coefficient + has_certificate * certificate
  linear_fees:
    constant: 42
    coefficient: 0
    certificate: 0

  # The speed to update the KES Key in seconds
  kes_update_speed: 43200 # 12hours

# The initial deposits present in the blockchain.
initial:
  - fund:
      address: ta1swk7svu8avn5jysl83vja72lp0sqfczanqee6q08a4j7q27a77kpg4p254a
      value: 100000000000
  # And so forth... 
```
</details>


##### --bft-leaders

The BFT leaders is a yaml file listing the private keys of the BFT leaders defined in the genesis file. For example:

<details>
  <summary>secret.yaml</summary>

```yaml
bft:
  signing_key: ed25519_sk1ga6n6fdsrruumg6nh0epdrqswrsdxhq4q7g5enun8v2jnk4u2gls08wfu3
```

</details>

##### example

```
$ cardano-wallet launch --genesis-file block0.bin --bft-leaders secret.yaml
```

<p align=right><a href="#">top :arrow_heading_up:</a></p>

### via cardano-http-bridge

> ```
> cardano-wallet launch 
>   ([--local-network] | [--network STRING])
>   ([--random-port] | [--port INT])
>   [--node-port INT] 
>   [--state-dir DIR] 
>   ([--quiet] | [--verbose])
> ```

##### example

```
$ cardano-wallet launch --network=testnet
```

<p align=right><a href="#">top :arrow_heading_up:</a></p>

## serve

Serve API that listens for commands/actions. Before launching user should build `cardano-http-bridge` https://github.com/input-output-hk/cardano-http-bridge/ (see details on the provided link). To run `cardano-http-bridge` do:

### via Jörmungandr

> ```
> cardano-wallet serve
>   ([--random-port] | [--port INT])
>   [--node-port INT]
>   [--database FILEPATH]
>   ([--quiet] | [--verbose])
>   --genesis-hash STRING
> ```

##### --genesis-hash

This is a hash of the the genesis block file which can obtained using Jörmungandr's tool command-line `jcli` as follows:

```
$ jcli genesis hash --input block0.bin
```

See also [`launch` via Jörmungandr](#launch) for details about how to generate `block0.bin`.

##### example

Start Jörmungandr:

```
$ jormungandr --genesis-block block0.bin --config config.yaml --secret secret.yaml
```

Then, start the wallet backend server:

```
$ cardano-wallet serve --genesis-hash $(jcli genesis hash --input block0.bin)
```

<p align=right><a href="#">top :arrow_heading_up:</a></p>

### via cardano-http-bridge

```
cardano-wallet serve
  [--network STRING]
  ([--random-port] | [--port INT])
  [--node-port INT]
  [--database FILEPATH]
  ([--quiet] | [--verbose])
```

##### example

Start cardano-http-bridge:

```
$ cardano-http-bridge start --template testnet --port=8080
```

Then, start the wallet backen server:

```
$ cardano-wallet serve --network=testnet --node-port 8080

```

<p align=right><a href="#">top :arrow_heading_up:</a></p>

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

<p align=right><a href="#">top :arrow_heading_up:</a></p>

## wallet list

> `cardano-wallet wallet list [--port=INT]`

Lists all your wallets:

```
$ cardano-wallet wallet list
```

<p align=right><a href="#">top :arrow_heading_up:</a></p>

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

<p align=right><a href="#">top :arrow_heading_up:</a></p>

## wallet get

> `cardano-wallet wallet get [--port=INT] WALLET_ID`

Fetches the wallet with specified wallet id:

```
$ cardano-wallet wallet get 2512a00e9653fe49a44a5886202e24d77eeb998f
```

<p align=right><a href="#">top :arrow_heading_up:</a></p>

## wallet utxo

> `cardano-wallet wallet utxo [--port=INT] WALLET_ID`

Visualize a wallet's UTxO distribution in the form of an histrogram with a logarithmic scale.
The distribution's data is returned by the API in a JSON format, e.g.:

```json
{
  "distribution": {
      "10": 1,
      "100": 0,
      "1000": 8,
      "10000": 14,
      "100000": 32,
      "1000000": 3,
      "10000000": 0,
      "100000000": 12,
      "1000000000": 0,
      "10000000000": 0,
      "100000000000": 0,
      "1000000000000": 0,
      "10000000000000": 0,
      "100000000000000": 0,
      "1000000000000000": 0,
      "10000000000000000": 0,
      "45000000000000000": 0
  }
}
```

which could be plotted as:

```
    │
100 ─
    │
    │                                 ┌───┐
 10 ─                         ┌───┐   │   │                   ┌───┐
    │                 ┌───┐   │   │   │   │                   │   │
    │                 │   │   │   │   │   │   ┌───┐           │   │
  1 ─ ┌───┐           │   │   │   │   │   │   │   │           │   │
    │ │   │           │   │   │   │   │   │   │   │           │   │
    │ │   │ │       │ │   │ │ │   │ ╷ │   │ ╷ │   │ ╷       ╷ │   │      ╷ 
    └─┘   └─│───────│─┘   └─│─┘   └─│─┘   └─│─┘   └─│───────│─┘   └──────│────────────
          10μ₳    100μ₳   1000μ₳   0.1₳    1₳      10₳     100₳        1000₳
```

<p align=right><a href="#">top :arrow_heading_up:</a></p>

## wallet update name

> `cardano-wallet wallet update name [--port=INT] WALLET_ID STRING`

Updates name of a wallet given wallet id:

```
$ cardano-wallet wallet update name 2512a00e9653fe49a44a5886202e24d77eeb998f NewName
```

<p align=right><a href="#">top :arrow_heading_up:</a></p>

## wallet update passphrase

> `cardano-wallet wallet update passphrase [--port=INT] WALLET_ID`

Interactive prompt to update the wallet master's passphrase (old passphrase required).

```
$ cardano-wallet wallet update passphrase 2512a00e9653fe49a44a5886202e24d77eeb998f
Please enter your current passphrase: **********
Please enter a new passphrase: **********
Enter the passphrase a second time: **********
```

<p align=right><a href="#">top :arrow_heading_up:</a></p>

## wallet delete

> `cardano-wallet wallet delete [--port=INT] WALLET_ID` 

Deletes wallet with specified wallet id:

```
$ cardano-wallet wallet delete 2512a00e9653fe49a44a5886202e24d77eeb998f
```

<p align=right><a href="#">top :arrow_heading_up:</a></p>

## transaction create

> `cardano-wallet transaction create [--port=INT] WALLET_ID --payment=PAYMENT...`

Creates and submits a new transaction:

```
$ cardano-wallet transaction create 2512a00e9653fe49a44a5886202e24d77eeb998f \
    --payment 22@Ae2tdPwUPEZ...nRtbfw6EHRv1D \
    --payment 5@Ae2tdPwUPEZ7...pVwEPhKwseVvf
```

This creates a transaction that sends 22 lovelace to `Ae2tdPwUPEZ...nRtbfw6EHRv1D` and 5 lovelace to `Ae2tdPwUPEZ7...pVwEPhKwseVvf` from wallet with id 2512a00e9653fe49a44a5886202e24d77eeb998f.

<p align=right><a href="#">top :arrow_heading_up:</a></p>

## transaction fees

> `cardano-wallet transaction fees [--port=INT] WALLET_ID --payment=PAYMENT...`

Creates and submits a new transaction:

```
$ cardano-wallet transaction create 2512a00e9653fe49a44a5886202e24d77eeb998f \
    --payment 22@Ae2tdPwUPEZ...nRtbfw6EHRv1D \
    --payment 5@Ae2tdPwUPEZ7...pVwEPhKwseVvf
```

This estimates fees for a transaction that sends 22 lovelace to `Ae2tdPwUPEZ...nRtbfw6EHRv1D` and 5 lovelace to `Ae2tdPwUPEZ7...pVwEPhKwseVvf` from wallet with id 2512a00e9653fe49a44a5886202e24d77eeb998f.

<p align=right><a href="#">top :arrow_heading_up:</a></p>

## address list

> `cardano-wallet address list [--port=INT] WALLET_ID [--state=STRING]` 

List all known (used or not) addresses and their corresponding status.

```
$ cardano-wallet list addresses 2512a00e9653fe49a44a5886202e24d77eeb998f
```

<p align=right><a href="#">top :arrow_heading_up:</a></p>