# Specification: Customer Deposit Wallet

## Synopsis

🚧 DRAFT 2023-10-18

This document specifies the core functionality of a **customer deposit wallet**,
or **deposit wallet** for short.

A customer deposit wallet allows you to track the origin of incoming funds:
Each customer is assigned a unique address belonging to the wallet;
a deposit made at this address is treated as originating from the customer.

Technically, each customer is represented by a numerical index (natural number).
Essentially, the deposit wallet manages a mapping between indices and addresses,
and tracks incoming funds for each known address.

# Setup

This document is a [literate Agda][lagda] file: It contains prose that
describes and explains the specification, but it also contains definitions
and logical properties that can be checked by the proof assistant [Agda][].

We use Agda because we plan to create a **machine-checked proof**
that our implementation adheres to this specification.
Specifically, we plan to implement the core functionality in Agda,
i.e. the functionality specificied in this document, and export
the code to Haskell using [agda2hs][] so that the core functionality
can be embedded in a full software application.

  [agda]: https://github.com/agda/agda
  [lagda]: https://agda.readthedocs.io/en/v2.6.4/tools/literate-programming.html
  [agda2hs]: https://github.com/agda/agda2hs

## Imports

In order to formulate the specification, we need to import standard vocabulary:

```agda
open import Haskell.Prelude
open import Relation.Nullary using (¬_)
open import Data.Product using () renaming (_×_ to both)
```

We also define a few conveniences:

A predicate `_∈_` that records whether an item is an element of a list

```agda
_∈_ : ∀ {a : Set} {{_ : Eq a}} → a → List a → Set
x ∈ xs = elem x xs ≡ True
```

The logical combinator "if and only if"

```agda
_⟷_ : Set → Set → Set
x ⟷ y = both (x → y) (y → x)
```

```agda
isJust : ∀ {a : Set} → Maybe a → Bool
isJust (Just _) = True
isJust Nothing = False
```

```agda
isSubsetOf : ∀ {a : Set} {{_ : Eq a}} → List a → List a → Bool
isSubsetOf xs ys = all (λ x → elem x ys) xs
```

# Specification

## Overview

This specification of a **customer deposit wallet**
amounts to the specification of an abstract data type `WalletState`,
which represents the entire state of such a wallet.

The goal of this document is to specify the operations
on this abstract data type and the logical properties that relate them.

We define a `module` `DepositWallet` which is parametrized by
several definitions from the Cardano ledger,
but also by the abstract data type `WalletState` that we wish to specify.

```agda
module
  DepositWallet
    (WalletState : Set)
    (Address : Set)
    {{_ : Eq Address}}
    (Slot : Set)
    (TxId : Set)
    (Tx : Set)
    (Value : Set)
    {{_ : Eq Value}}
    (PParams : Set)
  where
```

## Operations

We now list all auxiliary data types and all
operations supported by the abstract data type `WalletState`.
This list is meant for reference
— we will explain each of them in detail in the subsequent sections.

Auxiliary data types:

```agda
  Customer = Nat

  record ValueTransfer : Set where
    field
      spent    : Value
      received : Value

  open ValueTransfer

  TxSummary : Set
  TxSummary = Slot × TxId × ValueTransfer
```

Operations:

```agda
  record Operations : Set where
    field

      listCustomers : WalletState → List (Customer × Address)
      createAddress : Customer → WalletState → (Address × WalletState)

      availableBalance : WalletState → Value
      applyTx : Tx → WalletState → WalletState

      getCustomerHistory : WalletState → Customer → List TxSummary

      createPayment
        : List (Address × Value)
        → PParams → WalletState → Maybe Tx
```

## Properties

In subsequent sections, we will specify the properties that
the operations should satisfy.

The following record collects the properties:

```agda
  record Properties
      (O : Operations)
      : Set₁
    where
    open Operations O
```

(For some reason, it needs to be in `Set₁`.)

### Mapping between Customers and Address

The type `Customer` denotes a unique identier for a customer.
For reasons explained later, we choose to represent this type
as numerical indices, i.e. natural numbers:

    Customer = Nat

The mapping between customers and addresses will be queried and established with
the following operations:

    listCustomers : WalletState → List (Customer × Address)
    createAddress : Customer → WalletState → (Address × WalletState)

Here, `listCustomers` lists all customer/address pairs that have been mapped to each other so far.
In turn, `createAddress` adds a new customer/address to the mapping.

In order to express how these functions are related, we define

```agda
    knownCustomer : Customer → WalletState → Bool
    knownCustomer c = elem c ∘ map fst ∘ listCustomers

    knownCustomerAddress : Address → WalletState → Bool
    knownCustomerAddress address = elem address ∘ map snd ∘ listCustomers
```

Here, a `knownCustomer` is a `Customer` that appears in the result of `listCustomers`,
while `knownCustomerAddress` is an `Address` that appears in the result.
Note that a deposit wallet may own additional `Addresses` not included here,
such as change addresses — but these addresses are not customer addresses.

The two operations are related by the property

```agda
    field

      prop_create-get
        : ∀(c : Customer) (s0 : WalletState)
        → let (address , s1) = createAddress c s0
          in  knownCustomerAddress address s1 ≡ True
```

### Address derivation

For compatibility with hardware wallets and the [BIP-32][] standard,
we derive the `Address` of each customer from the root private key
of the wallet in a deterministic fashion:

```agda
      deriveAddress : WalletState → (Customer → Address)

      prop_create-derive
        : ∀(c : Customer) (s0 : WalletState)
        → let (address , _) = createAddress c s0
          in  deriveAddress s0 c ≡ address
```

Specifically, in the notation of [BIP-32][], we use

  	deriveAddress : WalletState → Nat → Address
	  deriveAddress s ix = rootXPrv s / 1857' / 1815' / 0' / 0 / ix

Here, `1857` is a new “purpose” identifier; we cannot reuse the [CIP-1852][] standard, because it behaves differently when discovering funds in blocks.

  [bip-32]: https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki
  [cip-1852]: https://cips.cardano.org/cips/cip1852/

This method of deriving addresses is also the reason why we choose
a concrete representation of `Customer` as a natural number.

### Applying transactions

TODO: Specification of total wallet funds.
Amounts to rewrite of the original wallet specification
by Edsko and Duncan in Agda. To be specified in a separate document.

    availableBalance : WalletState → Value
    applyTx : Tx → WalletState → WalletState

### Tracking incoming funds

Beyond assigning an address to a customer,
the new wallet state returned by `createAddress`
also tracks this address whenever new blocks are incorporated into the wallet state.
For this purpose of tracking, we introduce an operation

    getCustomerHistory : WalletState → Customer → List TxSummary

which returns a list of transaction summaries. For a given transaction, such a summary reports the total `Value` spend or received at a specific address.

    record ValueTransfer : Set where
      field
        spent    : Value
        received : Value

    open ValueTransfer

    TxSummary : Set
    TxSummary = Slot × TxId × ValueTransfer

Note that `Value` includes both native coins (ADA) and
user-defined assets, such as stablecoins NFTs.
Also note that the customer deposit wallet does not support
delegation and reward accounts, and the `spent` value
can only be spent from transaction outputs.

The function `getCustomerHistory` allows users to detect incoming
transfers by observing the `received` value.

The behavior of this function is best specified in terms of a function

```agda
      summarize : WalletState → Tx → List (Address × TxSummary)

    getAddressSummary
      : Address → List (Address × TxSummary) → List TxSummary
    getAddressSummary address =
      map snd ∘ filter (λ x → fst x == address)
```

which summarizes a single transaction. Specifically, the result of `getCustomerHistory` an aggregate of all previous transaction summaries.

```agda
    field
      prop_getAddressHistory-summary
        : ∀ (s : WalletState)
            (c : Customer)
            (address : Address)
            (tx : Tx)
        → (c , address) ∈ listCustomers s
        → getCustomerHistory (applyTx tx s) c
          ≡ (getAddressSummary address (summarize s tx))
              ++ getCustomerHistory s c
```

Importantly, we only track an address if and only if it is a `knownCustomerAddress`.

```agda
      prop_tx-known-address
        : ∀ (address : Address)
            (s : WalletState)
            (tx : Tx)
        → (knownCustomerAddress address s ≡ True)
        ⟷ (address ∈ map fst (summarize s tx))
```

### Creating transactions

Finally, we expose an operation

    createPayment
      : List (Address × Value)
      → PParams → WalletState → Maybe Tx

which constructs and signs a transaction that sends given values to given addresses.
Here, `PParams` are protocol parameters needed for computation the fee to
include in the `Tx`.

First, this function will succeed in creating a transaction if there are sufficient
funds available:

```agda

    field
      totalValue : List (Address × Value) → Value
      -- totalValue = mconcat ∘ map snd

      maxFee : Value -- maximum fee of a transaction
      exceeds : Value → Value → Set
      _<>_ : Value → Value → Value

      prop_createPayment-success
        : ∀ (s : WalletState)
            (pp : PParams)
            (destinations : List (Address × Value))
        → exceeds (availableBalance s) (totalValue destinations <> maxFee)
        → isJust (createPayment destinations pp s) ≡ True
```

TODO: The above statement cannot hold as written,
but it would be highly desirable to have something in this spirit.
(This would be part of a separate specification file
related to `balanceTransaction`.)
Aside from insufficient funds, reasons for failure include:

* Wallet UTxO is poor
  * Few UTxO which are too close to minimum ADA quantity
  * UTxO with too many native assets
* Destinations are poor
  * `Value` does not carry minimum ADA quantity
  * `Value` size too large (native assets, `Datum`, …)
* Combination of both:
  * Too many UTxO with small ADA amount
    that we need to cover a large `Value` payment.
    Example: "Have 1 million x 1 ADA coins, want to send 1 x 1'000'000 ADA coin."

Second, the transaction sends funds as indicated

```agda
    field
      outputs : Tx → List (Address × Value)

    field
      prop_createPayment-pays
        : ∀ (s : WalletState)
            (pp : PParams)
            (destinations : List (Address × Value))
            (tx : Tx)
          → createPayment destinations pp s ≡ Just tx
          → isSubsetOf (outputs tx) destinations ≡ True
```

Third, and most importantly, the operation `createPayment` never creates a transaction
whose `received` summary for any tracked index/address pair is non-zero.
In other words, `createPayment` uses change addresses that are distinct
from any address obtained via `createAddress`.

That said, `createPayment` is free to contribute to the `spent` summary of any address
— the deposit wallet spends funds from any address as it sees fit.

In other words, we have

```agda
    getAddress : (Address × Value) → Address
    getAddress = fst

    field
      prop_createPayment-not-known
        : ∀ (address : Address)
            (s : WalletState)
            (pp : PParams)
            (destinations : List (Address × Value))
            (tx : Tx)
        → knownCustomerAddress address s ≡ True
        → createPayment destinations pp s ≡ Just tx
        → ¬(address ∈ map getAddress (outputs tx))
```

## Derived Properties

TODO
From the properties above, one can prove various other properties.
However, this requires and induction principle on `WalletState`,
where we can be certain that other operations do not interfere
with the given ones.

```agda
{-
prop_getAddressHistory-unknown : Set
prop_getAddressHistory-unknown
  = ∀ (s : WalletState)
      (addr : Address)
  → knownAddress addr s ≡ False
  → getAddressHistory addr s ≡ []
-}
```

