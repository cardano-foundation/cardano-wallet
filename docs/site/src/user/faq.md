# FAQ

## Why aren't my unused addresses imported when I restore a wallet?

This is by virtue of the blockchain. An unused address is by definition unused. Meaning that is doesn't exist on the chain and only exists locally, in the context of the software that has generated it. Different software may use different rules to generate addresses. For example in the past, _cardano-sl_ wallets used a method called random derivation where addresses were created from a root seed and a random index stored within the address itself. Because these indexes were random, it was not possible to restore randomly generated addresses which hadn't been used on chain yet!

More recently, `cardano-wallet` has been using sequential derivation which follows a very similar principle with the major difference that indexes are derived in sequence, starting from 0. Following this method, wallets aren't allowed to pre-generate too many addresses in advance. As a consequence, it is now possible to restore a wallet across many machines while keeping a very consistent state.


## I’ve noticed that other blockchains create accounts for wallets?

There are two sides to this question. Either, you are referring to accounts as in Ethereum accounts, or you may refer to accounts of hierarchical deterministic wallets.

In the first scenario, assets in the form of accounts are only supported in the Shelley era of Cardano and only for a specific use-case: rewards. Rewards are indeed implicitly published on the blockchain to mitigate the risk of flooding the network at the end of every epoch with rewards payouts! Hence, each core node keeps track of the current value of each reward account in a Ledger. Money can be withdrawn from this account and is then turned as a UTxO. Please note that funds can never be manually sent to a reward account. The creation of a reward account is done when registering a staking key, via a specific type of transaction.

In the second case, please refer to the [hierarchical deterministic wallets](../concepts/hierarchical-deterministic-wallets.md) section in the _Key concepts_. Cardano wallets typically follow an HD tree of derivation as described in this section.


## It seems like I have to install and configure many APIs and libraries, what is the fastest and most simple way to do this at once?

🐳 [docker](https://docs.docker.com/) is your friend here! Every component is packaged as docker images. Releases are tagged and the very edge is always accessible. See the various docker guides on the components' repository, and also how to compose services using [docker-compose](https://docs.docker.com/compose/).


## Is there a reason why I would have to build from src?

If you intend to contribute to Cardano by making code changes to one of the core components, then yes. We recommend using [cabal](https://www.haskell.org/cabal/) for a better developer experience.

If you only intend to use the services as-is then, using either the pre-compiled release artifacts for your appropriate platform or a pre-packaged docker image is preferable.


## Where is the faucet and do I get test ADA?

- https://testnets.cardano.org/en/testnets/cardano/tools/faucet/




## Wallet Backend Specifications

<details>
  <summary>Where do the various notations come from?</summary>

  Like often in Maths, notations are described within the context of the paper with some a priori hypotheses. For the
Wallet Backend specifications, the notation is inspired from the [Z notation](https://en.wikipedia.org/wiki/Z_notation) in a slightly more lightweight form.
</details>

<details>
  <summary>What is <code>dom</code> from <strong>Lemma 2.1</strong></summary>

  There are multiple occurrences in the spec of expressions like: `(dom u ∩ ins) ◃ u`. The meaning of `dom u` isn't quite clearly defined anywhere but refers to the set of keys from the mapping defined by `u: txin ↦ txout`. Hence, `dom u` refers to all `txin` available in `u`.

  In Haskell, this translates to:

  ```hs
  newtype UTxO = UTxO (Map TxIn TxOut)

  dom :: UTxO -> Set TxIn
  dom (UTxO utxo) = Set.fromList $ Map.keys utxo
  ```
</details>

<details>
    <summary>How do I interpret <code>(Ix -> TxOut)</code> in the definition of <code>Tx</code> in fig. 1?</summary>

In the current wallet implementation it corresponds to <code>NonEmpty TxOut</code>.
</details>

<details>
  <summary>Are we going to update the formal specification?</summary>

  Some elements of the specification are written according to the current wallet implementation. Some parts could be simplified or removed, in particular the bits concerning a few metadata that we won't be implementing until a need for them is made clear. A few bits are also missing from the specifications (like the fact that answering `isOurs` is a stateful operation when dealing with BIP-44, or also, foreign transactions coming from ADA certificates redemption). In the long run, we do want to have the specification updated and proved.
</details>


## Address Derivation à la BIP-44

<details>
  <summary>Are we going to support the old Random derivation scheme forever?</summary>

  Short answer: yes. Though, we don't necessarily have to support a full set of features for wallets using an old derivation scheme in order to encourage users to migrate to the sequential scheme (a.k.a BIP-44). Most probably, we will forever have to support the old derivation scheme and a few features like tracking of the wallet UTxO and balance, and, allowing funds to be migrated to a wallet using the sequential scheme.
</details>

## Coin selection

<details>
  <summary>How many outputs can a single transaction have?</summary>

  It depends. To make a transaction, our implementation currently select UTxO from the available UTxO in the wallet in order to cover for the output requested in a transaction. For every output, the wallet actually creates two outputs:

  - The actual output to a target address
  - A change output to a change address of the source wallet

  Also, in practice, we strive to make these two outputs relatively equivalent in size, such that one cannot easily guess the change output from the actual one by looking at the transaction; enforcing therefore some privacy for users.

  Incidentally, we do consider every requested output in a transaction as an independent problem. This means that a single UTxO can only cover for one of the output (and will in practice, tend to be twice as big, such that we can generate an equivalent corresponding change output). As a consequence, in order to make a transaction to three target outputs, one needs to have at least three UTxOs that are big enough to cover all three outputs independently.

  Finally, it's important to notice that the fee calculation runs **after** the coin selection and is divvied across all change outputs. So in practice, the UTxOs only need to independently cover for outputs, but are considered together when adjusting for fees.

  A few examples to make this more concrete (in the scenario below, fees are ~`180000`):

  ```
  // Both UTxOs can separately cover fee and outputs
  Available UTxO:  [200000, 200000]
  Payment Distribution: [14, 42]
  Result: Ok

  // 2 UTxOs, each cannot separately cover fee and outputs, but jointly can
  Available UTxO: [100000, 100000]
  Payment Distribution: [14, 42]
  Result: Ok

  // Single UTxOs, big enough to cover for total requested amount & fee, but multiple outputs
  Available UTxO: [400000]
  Payment Distribution: [14, 42]
  Result: Error - UTxO not enough fragmented
  ```
</details>

<details>
  <summary>What is the security issue with re-using addresses?</summary>

  In practice, there's none.
</details>

## Miscellaneous

<details>
  <summary>How do I write a question in this FAQ?</summary>

  Use the `<details>` and `<summary>` html tags. The `<summary>` are nested inside the `<details>` tag
  and the question goes within the `<summary>` tag as a body. The answer goes below, and can contain any
  arbitrary markdown or HTML supported / allowed by GitHub. This produces a nice, readable output.

  e.g.
  ```html
    <details>
      <summary>What is love?</summary>

      Baby don't hurt me.
    </details>
  ```
</details>
