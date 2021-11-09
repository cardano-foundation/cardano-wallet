# Miscellaneous

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

# Wallet Backend Specifications

<details>
  <summary>Where do the various notations come from?</summary>

  Like often in Maths, notations are described within the context of the paper with some a priori hypotheses. For the
Wallet Backend specifications, the notation is inspired from the [Z notation](https://en.wikipedia.org/wiki/Z_notation) in a slightly more lightweight form.
</details>

<details>
  <summary>What is <code>dom</code> from <strong>Lemma 2.1</strong></summary>

  There are multiple occurrences in the spec of expressions like: `(dom u ∩ ins) ◃ u`. The meaning of `dom u` isn't quite clearly defined anywhere but refers to the set of keys from the mapping defined by `u: txin ↦ txout`. Hence,
`dom u` refers to all `txin` available in `u`.

  In Haskell, this translates to:

  ```hs
  class Dom a where
    type DomElem a :: *
    dom :: a -> Set (DomElem a)

  newtype UTxO = UTxO (Map TxIn TxOut)

  instance Dom UTxO where
    type DomElem UTxO = TxIn
    dom (UTxO utxo)   = Set.fromList $ Map.keys utxo
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


# Address Derivation à la BIP-44

<details>
  <summary>Are we going to support the old Random derivation scheme forever?</summary>

  Short answer: yes. Though, we don't necessarily have to support a full set of features for wallets using an old derivation scheme in order to encourage users to migrate to the sequential scheme (a.k.a BIP-44). Most probably, we will forever have to support the old derivation scheme and a few features like tracking of the wallet UTxO and balance, and, allowing funds to be migrated to a wallet using the sequential scheme.
</details>

# Coin selection

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