# API References

## Components

Component                            | Link
---                                  | ---
[cardano-wallet][cardano-wallet]     | https://input-output-hk.github.io/cardano-wallet/api/edge
[cardano-graphql][cardano-graphql]   | https://input-output-hk.github.io/cardano-graphql/
[cardano-rosetta][cardano-rosetta]   | https://github.com/input-output-hk/cardano-rosetta/tree/master/docs
[cardano-submit-api][cardano-submit-api]   |


{{<hint info>}}
_About cardano-wallet_

Cardano-wallet comes with a command-line interface that can be used as a quick alternative to cURL or wget to interact with a server running on localhost. Every endpoint of the API is mapped to a corresponding command which often offers a better user experience than directly interacting with the API as a human (API are for programs, command-lines are for humans).

For example, restoring a wallet goes normally through `POST /byron-wallets`, or can be done interactively with

```
$ cardano-wallet wallet create MyWallet
```

The command line also provides some useful helpers like a command to generate mnemonic sentences, or doing key derivation. For more details, see the wallet command-line user manual.
{{</hint>}}

## Libraries

Library                                          | Haskell                                                           | JavaScript
---                                              | ---                                                               | ---
[cardano-addresses][cardano-addresses]           | https://input-output-hk.github.io/cardano-addresses/haddock/      | https://input-output-hk.github.io/cardano-addresses/typescript/
[cardano-transactions][cardano-transactions]     | https://input-output-hk.github.io/cardano-transactions/haddock/   | _Soon available._
[cardano-coin-selection][cardano-coin-selection] | https://input-output-hk.github.io/cardano-coin-selection/haddock/ | _Soon available._
[bech32][bech32]                                 | https://input-output-hk.github.io/bech32/haddock/                 | See https://github.com/bitcoinjs/bech32

{{<hint info>}}
_About cardano-transactions_

In addition to the low-level library, cardano-transactions also provides a command-line interface (`cardano-tx`) to construct transactions directly in the terminal.
Check out the repository's documentation and examples to see example usage.
{{</hint>}}

[cardano-wallet]: https://github.com/input-output-hk/cardano-wallet
[cardano-submit-api]: https://github.com/input-output-hk/cardano-node/tree/master/cardano-submit-api
[cardano-graphql]: https://github.com/input-output-hk/cardano-graphql
[cardano-rosetta]: https://github.com/input-output-hk/cardano-rosetta

[cardano-coin-selection]: https://github.com/input-output-hk/cardano-coin-selection
[cardano-addresses]: https://github.com/input-output-hk/cardano-addresses
[cardano-transactions]: https://github.com/input-output-hk/cardano-transactions
[bech32]: https://github.com/input-output-hk/bech32
