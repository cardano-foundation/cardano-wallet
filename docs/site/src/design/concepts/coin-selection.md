# Coin Selection

## Reminder on UTxO

Cardano is a crypto-currency that is UTxO-based. UTxO stands here for _"Unspent
Transaction Output"_. In essence, UTxOs are very similar to bank notes and we
treat them as such. Hence, it is not possible to spend a single UTxO in more
than one transaction, and, in our current implementation, it's not possible to
split a single UTxO into multiple recipients of a transaction. Note that, we
also use the term _coin_ when referring to UTxO.

Contrary to a classic accounting model, there's no such thing as spending part
of a UTXO, and one has to wait for a transaction to be included in a block
before spending the remaining change. Similarly, one can't spend a $20 bill at
two different shops at the same time, even if it is enough to cover both
purchases — one has to wait for change from the first transaction before making
the second one. Having many available coins allow for greater concurrency
capacity. The more coins are available, the more transactions can be made at
the same time.


### What is Coin Selection

For every transaction, the wallet backend performs a coin selection. There are
many approaches to the problem and, many solutions. Moreover, there are a few
problematics we have to deal with when performing coin selection:

- A transaction has a limited size defined by the protocol. Adding inputs or
  outputs to a transaction increases its size. Therefore, there's a practical
  maximum number of coins that can be selected.

- As soon as coins from a given address are spent, that address is exposed to
  the public. From this follows privacy and security issues known about address
  re-use.

- In order to maintain good privacy, change outputs shouldn't be much discernible
  from the actual outputs.

- Because of the first point, a wallet needs to make sure it doesn't needlessly
  fragment available UTxOs by creating many small change outputs. Otherwise, in
  the long run, the wallet becomes unusable.

- Coin selection needs to remain fairly efficient to minimize fees as much as
  possible.

In Cardano, the coin selection works mainly in two steps:

1. Coins are selected randomly to cover a given amount, generating a change output that is nearly as big as the original output
2. The inputs and outputs are slightly adjusted to cover for fees

Note that in case the random coin selection fails (because we couldn't reach
the target amount without exceeding the transaction max size), we fallback to
selecting UTxO from the largest first. If this fails again, it means that the
UTxOs of the wallet is too fragmented and smaller transactions have to be sent.
In practice, this shouldn't happen much as the wallet tries to get rid of the
dust (by selecting randomly, if one has many small UTxOs, a random selection
has bigger chances to contain many small UTxOs as well).


### Multi-Output Transactions

Cardano only allows a given UTXO to cover at most one single transaction
output. As a result, when the number of transaction outputs is greater than the
number of available UTXOs, the API returns a 'UTXONotEnoughFragmented' error.

To make sure the source account has a sufficient level of UTXO fragmentation
(i.e. number of UTXOs), the state of the UTXOs can be monitored via following wallet endpoints:
 - [UTxO Statistics](https://cardano-foundation.github.io/cardano-wallet/api/edge/#operation/getUTxOsStatistics)
 - [UTxO Snapshot](https://cardano-foundation.github.io/cardano-wallet/api/edge/#operation/getWalletUtxoSnapshot)

The number of wallet UTXOs should be no less than the transaction outputs, and
the sum of all UTXOs should be enough to cover the total transaction amount,
including fees.


### Annexes

- [The Challenge of Optimizing Unspent Output Selection](https://medium.com/@lopp/the-challenges-of-optimizing-unspent-output-selection-a3e5d05d13ef)
- [Self Organisation In Coin Selection](https://iohk.io/blog/self-organisation-in-coin-selection/)
