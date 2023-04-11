---
order: 3
pandoc:
  rewriteClass:
    cli: bg-${theme}-50 text-sm p-2 rounded-md shadow mb-4 mx-2
    divider: text-sm text-gray-600 text-center border-slate-500 border-b mt-6 mb-0 p-0
---

# Command-Line Interface

The CLI is a proxy to the wallet server, which is required for most commands. Commands are turned into corresponding API calls, and submitted to an up-and-running server. Some commands do not require an active server and can be run "offline". (e.g. `recovery-phrase generate`)

The wallet command-line interface (abbrev. CLI) is a tool that provides a convenient way of using the cardano-wallet HTTP Application Programming Interface (abbrev. API). The wallet API is an HTTP service used to manage wallets, make payments, and update wallet data such as addresses and keys, for instance. The wallet CLI can start the wallet server, or run a number of commands on a running server, and supports most functions of the API itself.

The intended audience of the CLI are users who run a wallet API server outside of Daedalus and work with the cardano-wallet API directly - these include application developers, stake pool operators, and exchanges.

CLI commands allow you to make and track changes while maintaining the wallet API. The wallet CLI converts commands into corresponding API calls, submits them to a running server and display the server's responses in a readable way into the terminal.

## Pre-Requisites

 - [[how-to-start-wallet-server]]

## How to Run

You can explore the wallet CLI with:

```console
$ cardano-wallet --help
```

Then, you can use a list of commands for various purposes, such as:

-   list, create, update, or delete wallets
-   create, submit, forget, or track fees in regards to transactions
-   list, create, and import wallet addresses
-   view network information and parameters
-   manage private and public keys

Each sub-command will also provide some additional help when passed `--help`. For example:

```console
$ cardano-wallet transaction --help
```

## Commands

:::{.cli}
<pre>
Usage: cardano-wallet COMMAND
  Cardano Wallet Command-Line Interface (CLI)

Available OPTIONS:
  -h | --help

Available COMMANDS:
  <a href="#serve">serve</a>                    Serve an HTTP API that listens for commands/actions.
  recovery-phrase
    <a href="#recovery-phrase-generate">generate</a>               Generate an English recovery phrase
  wallet
    <a href="#wallet-list">list</a>                   List all known wallets
    create
      <a href="#wallet-create-from-recovery-phrase">from-recovery-phrase</a> Create a new wallet using a recovery-phrase
    <a href="#wallet-get">get</a>                    Fetch a particular wallet
    <a href="#wallet-utxo">utxo</a>                   Get a wallet's UTxO distribution
    <a href="#wallet-utxo-snapshot">utxo-snapshot</a>          Get UTxO snapshot for wallet with specified id.
    update
      <a href="#wallet-update-passphrase">passphrase</a>           Update a wallet's master passphrase
      <a href="#wallet-update-name">name</a>                 Update a wallet's name
    <a href="#wallet-delete">delete</a>                 Forget a wallet and its metadata
  transaction
    <a href="#transaction-create">create</a>                 Create a transaction from a known wallet
    <a href="#transaction-fees">fees</a>                   Estimate fees for a transaction
    <a href="#transaction-list">list</a>                   List the transactions associated with a wallet
    <a href="#transaction-submit">submit</a>                 Submit an externally-signed transaction
    <a href="#transaction-forget">forget</a>                 Forget a pending transaction with specified id
    <a href="#transaction-get">get</a>                    Get a transaction with specified id
  address
    <a href="#address-list">list</a>                   List all known addresses of a wallet
    <a href="#address-create">create</a>                 Create a new random address.
    <a href="#address-import">import</a>                 Import a random address generated elsewhere.
  network
    <a href="#network-information">information</a>            View network information
    <a href="#network-parameters">parameters</a>             View network parameters
    <a href="#network-clock">clock</a>                  View NTP offset
  key
    <a href="#key-from-recovery-phrase">from-recovery-phrase</a>   Convert a recovery phrase to an extended private key
    <a href="#key-child">child</a>                  Derive child keys from a parent public or private key
    <a href="#key-public">public</a>                 Get the public counterpart of a private key
    <a href="#key-inspect">inspect</a>                Show information about a key
  stake-pool
    <a href="#stake-pool-list">list</a>                   List all known stake pools
  <a href="#version">version</a>                  Show the program's current version
</pre>
:::

:::{.info-block}
:information_source: The CLI commands for `wallet`, `transaction` and `address` only output valid JSON on `stdout`. So you may redirect the output to a file with `>` or pipe it into utility software like `jq`!
:::

## Commands

### serve

Serve API that listens for commands/actions. Before launching user should start [`cardano-node`](https://github.com/input-output-hk/cardano-node).

:::{.cli}
```
Usage: cardano-wallet serve [--listen-address HOST] 
                            (--node-socket FILE [--sync-tolerance DURATION]) 
                            [--random-port | --port INT] 
                            [--tls-ca-cert FILE --tls-sv-cert FILE
                              --tls-sv-key FILE] 
                            (--mainnet | --testnet FILE ) 
                            [--database DIR] [--shutdown-handler] 
                            [--pool-metadata-fetching ( none | direct | SMASH-URL )]
                            [--token-metadata-server URL] 
                            [--trace-NAME SEVERITY]

  Serve API that listens for commands/actions.

Available options:
  -h,--help                Show this help text
  --help-tracing           Show help for tracing options
  --listen-address HOST    Specification of which host to bind the API server
                           to. Can be an IPv[46] address, hostname, or '*'.
                           (default: 127.0.0.1)
  --node-socket FILE       Path to the node's domain socket file (POSIX) or pipe
                           name (Windows). Note: Maximum length for POSIX socket
                           files is approx. 100 bytes. Note: Windows named pipes
                           are of the form \\.\pipe\cardano-node
  --sync-tolerance DURATION
                           time duration within which we consider being synced
                           with the network. Expressed in seconds with a
                           trailing 's'. (default: 300s)
  --random-port            serve wallet API on any available port (conflicts
                           with --port)
  --port INT               port used for serving the wallet API. (default: 8090)
  --tls-ca-cert FILE       A x.509 Certificate Authority (CA) certificate.
  --tls-sv-cert FILE       A x.509 Server (SV) certificate.
  --tls-sv-key FILE        The RSA Server key which signed the x.509 server
                           certificate.
  --mainnet                Use Cardano mainnet protocol
  --testnet FILE           Path to the byron genesis data in JSON format.
  --database DIR           use this directory for storing wallets. Run in-memory
                           otherwise.
  --shutdown-handler       Enable the clean shutdown handler (exits when stdin
                           is closed)
  --pool-metadata-fetching ( none | direct | SMASH-URL )
                           Sets the stake pool metadata fetching strategy.
                           Provide a URL to specify a SMASH metadata proxy
                           server, use "direct" to fetch directly from the
                           registered pool URLs, or "none" to completely disable
                           stake pool metadata. The initial setting is "none"
                           and changes by either this option or the API will
                           persist across restarts.
  --token-metadata-server URL
                           Sets the URL of the token metadata server. If unset,
                           metadata will not be fetched. By using this option,
                           you are fully trusting the operator of the metadata
                           server to provide authentic token metadata.
  --log-level SEVERITY     Global minimum severity for a message to be logged.
                           Individual tracers severities still need to be
                           configured independently. Defaults to "DEBUG".
  --trace-NAME SEVERITY    Individual component severity for 'NAME'. See
                           --help-tracing for details and available tracers.
```
:::

#### Minimal Arguments

> :information_source: More information on starting the wallet server can be found in [[how-to-start-wallet-server]].

In order to start the wallet server, you'll need to provide _at least_ the path to cardano-node's socket/pipe and a target network. That socket is automatically created when starting a cardano-node and it exists so long as the node remains running.

We also recommend to pass a `--database` option pointing to a directory on the file-system; without this option, the wallet will maintain a state in-memory which will vanish once stopped.

#### Runtime flags

By default, the wallet runs **on a single core** which is sufficient for most 'normal users'. Application running larger wallets like exchanges should configure the server to use multiple cores for some database blocking operations may have a visible negative effect on the overall server behavior. This can be achieved by providing specific runtime flags to the serve command delimited by `+RTS <flags> -RTS`. To configure the how much cores are available to the server, use the `-N` flag. For example, to configure 2 cores do:

```console
$ cardano-wallet serve ... +RTS -N2 -RTS
```

Using `+RTS -N4 -RTS` will tell the server to use 4 cores. Note that there's little performance benefits between 2 and 4 cores for server running a single wallet, but there are visible performance improvements from 1 to 2.

#### Domain socket/named pipe

On POSIX systems (i.e. Linux and macOS), a [UNIX domain socket](https://en.wikipedia.org/wiki/Unix_domain_socket) is used for communication between the cardano-wallet and cardano-node processes.

On these systems, the `--node-socket` argument must be a path to a socket file. Note that there is a limitation on socket path lengths of about 100 characters or so.

On Windows systems, a [Named Pipe](https://en.wikipedia.org/wiki/Named_pipe#In_Windows) is used instead.

[Windows Named Pipes](https://docs.microsoft.com/en-us/windows/win32/ipc/named-pipes) do not have filenames. So on Windows systems, the `--node-socket` argument must be a pipe name. Pipe names are a string of the form `\\.\pipe\name`. For example, `\\.\pipe\cardano-wallet`.

#### Examples

##### Mainnet

```console
$ cardano-wallet serve \
  --mainnet \
  --node-socket CARDANO_NODE_SOCKET_PATH_OR_PIPE \
  --database ./wallets-mainnet
```

##### Testnet

Note that for testnets, a _byron_ genesis file is required (see [pre-requisites](#pre-requisites)), even though the network is in the shelley era. This is because the chain is
synced from the beginning of the first era.

```console
$ cardano-wallet serve \
  --testnet testnet-byron-genesis.json \
  --node-socket CARDANO_NODE_SOCKET_PATH_OR_PIPE \
  --database ./wallets-testnet
```

#### Metadata

For the wallet to show stake pool metadata, you need to set `--pool-metadata-fetching ( none | direct | SMASH-URL )`. And for the wallet to show token metadata, you need to set `--token-metadata-server URL`.

#### Logging options for serve

`serve` accepts extra command-line arguments for logging (also called "tracing"). Use `--help-tracing` to show the
options, the tracer names, and the possible log levels.

:::{.cli}
<pre>
$ <b>cardano-wallet serve --help-tracing</b>

Additional tracing options:

  --log-level SEVERITY     Global minimum severity for a message to be logged.
                           Defaults to "DEBUG" unless otherwise configured.
  --trace-NAME=off         Disable logging on the given tracer.
  --trace-NAME=SEVERITY    Set the minimum logging severity for the given
                           tracer. Defaults to "INFO".

The possible log levels (lowest to highest) are:
  debug info notice warning error critical alert emergency

The possible tracers are:
  application    About start-up logic and the server's surroundings.
  api-server     About the HTTP API requests and responses.
  wallet-engine  About background wallet workers events and core wallet engine.
  wallet-db      About database operations of each wallet.
  pools-engine   About the background worker monitoring stake pools and stake pools engine.
  pools-db       About database operations on stake pools.
  ntp-client     About ntp-client.
  network        About network communication with the node.
</pre>
:::

##### example

Use these options to enable debug-level tracing for certain components
of the wallet. For example, to log all database queries for the wallet
databases, use:

```
$ cardano-wallet serve --trace-wallet-db=debug ...
```

{.divider}
<a href="#">top :arrow_heading_up:</a>


### recovery-phrase generate

:::{.cli}
```
cardano-wallet recovery-phrase generate [--size=INT]
```
:::

Generates an English recovery phrase.

```console
$ cardano-wallet recovery-phrase generate
```

These words will be used to create a wallet later. You may also ask for a specific number of words using the `--size` option:

```
$ cardano-wallet recovery-phrase generate --size 21
```

{.divider}
<a href="#">top :arrow_heading_up:</a>

### wallet list

:::{.cli}
```
cardano-wallet wallet list [--port=INT]
```
:::

Lists all your wallets:

```console
$ cardano-wallet wallet list
```

{.divider}
<a href="#">top :arrow_heading_up:</a>

### wallet create from recovery-phrase

:::{.cli}
```
cardano-wallet wallet create from-recovery-phrase [--port=INT] <name> [--address-pool-gap=INT]
```
:::

Create a new wallet using a sequential address scheme. This is an interactive command that will prompt you for recovery-phrase words and password.

```console
$ cardano-wallet wallet create "My Wallet"
Please enter a 15–24 word recovery-phrase sentence: <enter generated recovery-phrase here>
(Enter a blank line if you do not wish to use a second factor.)
Please enter a 9–12 word recovery-phrase second factor: <skip or enter new recovery-phrase here>
Please enter a passphrase: ****************
Enter the passphrase a second time: ****************
```

after this your new wallet will be created

{.divider}
<a href="#">top :arrow_heading_up:</a>

### wallet get

:::{.cli}
```
cardano-wallet wallet get [--port=INT] WALLET_ID
```
:::

Fetches the wallet with specified wallet id:

```console
$ cardano-wallet wallet get 2512a00e9653fe49a44a5886202e24d77eeb998f
```

{.divider}
<a href="#">top :arrow_heading_up:</a>

### wallet utxo

:::{.cli}
```
cardano-wallet wallet utxo [--port=INT] WALLET_ID
```
:::

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

{.divider}
<a href="#">top :arrow_heading_up:</a>

### wallet utxo-snapshot

:::{.cli}
```
cardano-wallet wallet utxo-snapshot [--port INT] WALLET_ID
```
:::

Gets a snapshot of the wallet's entire UTxO set, in JSON format.

Each entry in the list contains the following fields:

| Field | Description |
| -- | -- |
| `ada` | the actual ada quantity of this UTxO entry |
| `ada_minimum` | the minimum ada quantity permitted by the protocol |
| `assets` | quantities of all other assets included in this UTxO entry |

```json
{
    "entries": [
        {
            "ada_minimum": {
                "quantity": 1666665,
                "unit": "lovelace"
            },
            "ada": {
                "quantity": 15582575,
                "unit": "lovelace"
            },
            "assets": [
                {
                    "asset_name": "",
                    "quantity": 1503,
                    "policy_id": "789ef8ae89617f34c07f7f6a12e4d65146f958c0bc15a97b4ff169f1"
                },
                {
                    "asset_name": "4861707079436f696e",
                    "quantity": 4958,
                    "policy_id": "919e8a1922aaa764b1d66407c6f62244e77081215f385b60a6209149"
                }
            ]
        },
        ...
    ]
}
```

:::{.highlight-block}
⚠ This endpoint was intended to be used for debugging purposes. The
output format is subject to change at any time.
:::

{.divider}
<a href="#">top :arrow_heading_up:</a>

### wallet update name

:::{.cli}
```
cardano-wallet wallet update name [--port=INT] WALLET_ID STRING
```
:::

Updates name of a wallet given wallet id:

```
$ cardano-wallet wallet update name 2512a00e9653fe49a44a5886202e24d77eeb998f NewName
```

{.divider}
<a href="#">top :arrow_heading_up:</a>

### wallet update passphrase

:::{.cli}
```
cardano-wallet wallet update passphrase [--port=INT] WALLET_ID
```
:::

Interactive prompt to update the wallet master's passphrase (old passphrase required).

```console
$ cardano-wallet wallet update passphrase 2512a00e9653fe49a44a5886202e24d77eeb998f
Please enter your current passphrase: **********
Please enter a new passphrase: **********
Enter the passphrase a second time: **********
```

{.divider}
<a href="#">top :arrow_heading_up:</a>

### wallet delete

:::{.cli}
```
cardano-wallet wallet delete [--port=INT] WALLET_ID
```
:::

Deletes wallet with specified wallet id:

```console
$ cardano-wallet wallet delete 2512a00e9653fe49a44a5886202e24d77eeb998f
```

{.divider}
<a href="#">top :arrow_heading_up:</a>

### transaction create

:::{.cli}
```
cardano-wallet transaction create [--port=INT] WALLET_ID [--metadata=JSON] [--ttl=SECONDS] --payment=PAYMENT...
```
:::

Creates and submits a new transaction:

```console
$ cardano-wallet transaction create 2512a00e9653fe49a44a5886202e24d77eeb998f \
    --payment 22@Ae2tdPwUPEZ...nRtbfw6EHRv1D \
    --payment 5@Ae2tdPwUPEZ7...pVwEPhKwseVvf \
    --metadata '{ "0":{ "string":"cardano" } }'
```

This creates a transaction that sends 22 lovelace to `Ae2tdPwUPEZ...nRtbfw6EHRv1D` and 5 lovelace to `Ae2tdPwUPEZ7...pVwEPhKwseVvf` from wallet with id 2512a00e9653fe49a44a5886202e24d77eeb998f.

For more information about the `--metadata` option, see [[TxMetadata]].

{.divider}
<a href="#">top :arrow_heading_up:</a>

### transaction fees

:::{.cli}
```
cardano-wallet transaction fees [--port=INT] WALLET_ID [--metadata=JSON] [--ttl=SECONDS] --payment=PAYMENT...
```
:::

Estimates fee for a given transaction:

```console
$ cardano-wallet transaction fees 2512a00e9653fe49a44a5886202e24d77eeb998f \
    --payment 22@Ae2tdPwUPEZ...nRtbfw6EHRv1D \
    --payment 5@Ae2tdPwUPEZ7...pVwEPhKwseVvf \
    --metadata '{ "0":{ "string":"cardano" } }'
```

This estimates fees for a transaction that sends 22 lovelace to `Ae2tdPwUPEZ...nRtbfw6EHRv1D` and 5 lovelace to `Ae2tdPwUPEZ7...pVwEPhKwseVvf` from wallet with id 2512a00e9653fe49a44a5886202e24d77eeb998f.

{.divider}
<a href="#">top :arrow_heading_up:</a>

### transaction list

:::{.cli}
```
cardano-wallet transaction list [--port INT] WALLET_ID [--start TIME] [--end TIME] [--order ORDER] [--simple-metadata] [--max_count MAX_COUNT]
```
:::

List the transactions associated with a wallet.

```
$ cardano-wallet transaction list 2512a00e9653fe49a44a5886202e24d77eeb998f \
    --start 2018-09-25T10:15:00Z \
    --end 2019-11-21T10:15:00Z \
    --order ascending \
    --max_count 10
```

This lists max 10 transactions between `2018-09-25T10:15:00Z` and `2019-11-21T10:15:00Z` in `ascending` order.

{.divider}
<a href="#">top :arrow_heading_up:</a>

### transaction submit

:::{.cli}
```
cardano-wallet transaction submit [--port INT] BINARY_BLOB
```
:::

Submit transaction prepared and signed outside of the wallet:

```
$ cardano-wallet transaction submit 00bf02010200000...d21942304
```

Sends transaction identified by a hex-encoded BINARY_BLOB of externally-signed transaction.

{.divider}
<a href="#">top :arrow_heading_up:</a>

### transaction forget

:::{.cli}
```
cardano-wallet transaction forget [--port INT] WALLET_ID TRANSACTION_ID
```
:::

Forget pending transaction for a given wallet:

```
$ cardano-wallet transaction forget 2512a00e9653fe49a44a5886202e24d77eeb998f 3e6ec12da4414aa0781ff8afa9717ae53ee8cb4aa55d622f65bc62619a4f7b12
```

{.divider}
<a href="#">top :arrow_heading_up:</a>

### transaction get

:::{.cli}
```
cardano-wallet transaction get [--port INT] WALLET_ID TRANSACTION_ID
```
:::

Get a transaction with the specified id:

```
$ cardano-wallet transaction get 2512a00e9653fe49a44a5886202e24d77eeb998f 3e6ec12da4414aa0781ff8afa9717ae53ee8cb4aa55d622f65bc62619a4f7b12
```

{.divider}
<a href="#">top :arrow_heading_up:</a>

### address list

:::{.cli}
```
cardano-wallet address list [--port=INT] WALLET_ID [--state=STRING]
```
:::

List all known (used or not) addresses and their corresponding status.

```
$ cardano-wallet list addresses 2512a00e9653fe49a44a5886202e24d77eeb998f
```

{.divider}
<a href="#">top :arrow_heading_up:</a>

### address create

:::{.cli}
```
cardano-wallet address create [--port INT] [--address-index INDEX] WALLET_ID
```
:::

Create new address for random wallet.

```
$ cardano-wallet address create 03f4c150aa4626e28d02be95f31d3c79df344877
Please enter your passphrase: *****************
Ok.
{
    "state": "unused",
    "id": "2w1sdSJu3GVgr1ic6aP3CEwZo9GAhLzigdBvCGY4JzEDRbWV4HUNpZdHf2n5fV41dGjPpisDX77BztujAJ1Xs38zS8aXvN7Qxoe"
}
```
{.divider}
<a href="#">top :arrow_heading_up:</a>

### address import

:::{.cli}
```
cardano-wallet address import [--port INT] WALLET_ID ADDRESS
```
:::

Import address belonging to random wallet.

{.divider}
<a href="#">top :arrow_heading_up:</a>

### network information

:::{.cli}
```
cardano-wallet network information [--port=INT]
```
:::

View network information and syncing progress between the node and the blockchain.

```
$ cardano-wallet network information
```

{.divider}
<a href="#">top :arrow_heading_up:</a>

### network parameters

:::{.cli}
```
cardano-wallet network parameters [--port=INT] EPOCH_NUMBER
```
:::

View network parameters. EPOCH_NUMBER can be `latest` or valid epoch number (not later than the current one), ie., `0`, `1`, .. .

```console
$ cardano-wallet network parameters latest
```

{.divider}
<a href="#">top :arrow_heading_up:</a>

### network clock

:::{.cli}
```
cardano-wallet network clock
```
:::

View [NTP](https://en.wikipedia.org/wiki/Network_Time_Protocol) offset for cardano-wallet server in microseconds.

```
$ cardano-wallet network clock
Ok.
{
    "status": "available",
    "offset": {
        "quantity": -30882,
        "unit": "microsecond"
    }
}
```

:::{.highlight-block}
:warning: At this stage the command is not supported on Windows platform. Invoked on Windows will return `status: unavailable` in the response message.
:::

{.divider}
<a href="#">top :arrow_heading_up:</a>

### key from-recovery-phrase

Extract the root extended private key from a recovery phrase. New recovery phrases can be generated using <a href="#recovery-phrase-generate">`recovery-phrase generate`</a>.

:::{.cli}
```
Usage: cardano-wallet key from-recovery-phrase ([--base16] | [--base58] | [--bech32]) STYLE
  Convert a recovery phrase to an extended private key

Available options:
  -h,--help                Show this help text
  STYLE                    Byron | Icarus | Jormungandr | Shelley

The recovery phrase is read from stdin.
```
:::

Example:

```console
$ cardano-wallet recovery-phrase generate | cardano-wallet key from-recovery-phrase Icarus
xprv12qaxje8hr7fc0t99q94jfnnfexvma22m0urhxgenafqmvw4qda0c8v9rtmk3fpxy9f2g004xj76v4jpd69a40un7sszdnw58qv527zlegvapwaee47uu724q4us4eurh52m027kk0602judjjw58gffvcqzkv2hs
```

{.divider}
<a href="#">top :arrow_heading_up:</a>

### key child

Derive child key from root private key. The parent key is read from standard input.

:::{.cli}
```
Usage: cardano-wallet key child ([--base16] | [--base58] | [--bech32]) [--legacy] DERIVATION-PATH
  Derive child keys from a parent public/private key

Available options:
  -h,--help                Show this help text
  DERIVATION-PATH          Slash-separated derivation path.
                           Hardened indexes are marked with a 'H' (e.g. 1852H/1815H/0H/0).

The parent key is read from stdin.
```
:::

Example:

```console
$ cardano-wallet recovery-phrase generate | cardano-wallet key from-recovery-phrase Icarus > root.xprv
$ cat root.xprv | cardano-wallet key child 44H/1815H/0H/0
xprv13parrg5g83utetrwsp563w7hps2chu8mwcwqcrzehql67w9k73fq8utx6m8kgjlhle8claexrtcu068jgwl9zj5jyce6wn2k340ahpmglnq6x8zkt7plaqjgads0nvmj5ahav35m0ss8q95djl0dcee59vvwkaya
```

{.divider}
<a href="#">top :arrow_heading_up:</a>

### key public

Extract the public key of an extended private key. Keys can be obtained using <a href="#key-from-recovery-phrase">`key from-recovery-phrase`</a> and <a href="#key-child">`key child`</a>.

:::{.cli}
```
Usage: cardano-wallet-jormungandr key public ([--base16] | [--base58] | [--bech32])
  Get the public counterpart of a private key

Available options:
  -h,--help                Show this help text

The private key is read from stdin.
```
:::

Example:

```console
$ cardano-wallet recovery-phrase generate | cardano-wallet key from-recovery-phrase Icarus > root.xprv
$ cat root.xprv | cardano-wallet key public
xpub1le8gm0m5cesjzzjqlza4476yncp0yk2jve7cce8ejk9cxjjdama24hudzqkrxy4daxwmlfq6ynczj338r7f5kzs43xs2fkmktekd4fgnc8q98
```

{.divider}
<a href="#">top :arrow_heading_up:</a>

### key inspect

:::{.cli}
```
Usage: cardano-wallet-jormungandr key inspect
  Show information about a key

Available options:
  -h,--help                Show this help text

The parent key is read from stdin.
```
:::

{.divider}
<a href="#">top :arrow_heading_up:</a>

### stake-pool list

:::{.cli}
```
Usage: cardano-wallet stake-pool list [--port INT] [--stake STAKE]
  List all known stake pools.

Available options:
  -h,--help                Show this help text
  --port INT               port used for serving the wallet API. (default: 8090)
  --stake STAKE            The stake you intend to delegate, which affects the
                           rewards and the ranking of pools.
```
:::

{.divider}
<a href="#">top :arrow_heading_up:</a>

### version

:::{.cli}
```
cardano-wallet version
```
:::

Show the software version.

{.divider}
<a href="#">top :arrow_heading_up:</a>

## Bash Shell Command Completion

:gift_heart: For bash/zsh auto-completion, put the following script in your `/etc/bash_completion.d`:

```bash
# /etc/bash_completion.d/cardano-wallet.sh

_cardano-wallet()
{
   local CMDLINE
   local IFS=$'\n'
   CMDLINE=(--bash-completion-index $COMP_CWORD)

   for arg in ${COMP_WORDS[@]}; do
       CMDLINE=(${CMDLINE[@]} --bash-completion-word $arg)
   done

   COMPREPLY=( $(cardano-wallet "${CMDLINE[@]}") )
}

complete -o filenames -F _cardano-wallet cardano-wallet
```
