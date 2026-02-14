# The UTxO Model

## UTxO vs Account model

Cardano uses the **UTxO** (Unspent Transaction Output) model, in contrast to
the account-based model used by Ethereum. In the UTxO model, value is stored
in discrete, indivisible outputs -- analogous to banknotes.

!!! info "Key property"
    A UTxO can only be spent **once**, in its **entirety**. You cannot spend
    part of a UTxO -- you must consume the whole thing and return any excess
    as **change**.

### Analogy with cash

| Cash                        | UTxO                                |
|-----------------------------|-------------------------------------|
| Pay with a $20 note         | Consume a UTxO as a transaction input |
| Receive change              | Create a change output              |
| Can't use the same note twice | A UTxO can only be spent once     |
| Can't split a note in two shops | Must wait for change before spending it |

### Consequences

Because each UTxO is indivisible:

- **Concurrency**: having many UTxOs allows more transactions to be made in
  parallel. With only one large UTxO, you can only send one transaction at a
  time (and must wait for change).

- **Privacy**: each time you spend from an address, that address and its
  spending key are exposed. Address reuse weakens privacy.

- **Fragmentation**: creating too many small UTxOs ("dust") makes future
  transactions more expensive, since more inputs are needed to reach the
  required amount.

## Why coin selection matters

For every transaction, the wallet must decide **which UTxOs to consume** as
inputs. This is the coin selection problem. A good algorithm must:

1. **Cover the required amount** -- the total value of selected inputs must be
   at least the total value of all outputs plus the transaction fee.

2. **Respect transaction size limits** -- the protocol limits the maximum
   transaction size, which in turn limits the number of inputs and outputs.

3. **Maintain a healthy UTxO set** -- the selection strategy should ensure that
   the wallet's UTxO distribution evolves over time to remain useful for future
   transactions.

4. **Preserve privacy** -- change outputs should not be easily distinguishable
   from payment outputs by an outside observer.

## Multi-asset UTxOs

On Cardano, a single UTxO can carry **ada** (lovelace) and any number of
**native tokens**. This makes coin selection significantly more complex than
on Bitcoin, where each UTxO only carries a single asset.

The coin selection algorithm must satisfy requirements for **all** assets
simultaneously -- it is not sufficient to handle each asset independently.

!!! warning "Minimum ada requirement"
    Every UTxO on Cardano must contain a minimum amount of ada, determined by
    the size of its token bundle. UTxOs carrying many different native tokens
    require more ada. The coin selection algorithm must account for this when
    generating change outputs.
