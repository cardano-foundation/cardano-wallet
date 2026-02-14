# Minting and Burning

Transactions on Cardano can **mint** (create) and **burn** (destroy) native
tokens. These operations interact with coin selection because they change the
value balance of the transaction.

## Minting as extra input

Minting tokens provides **input value** from "the void":

$$
\text{total input} = \sum \text{UTxO inputs} + \text{extra ada source} + \text{minted tokens}
$$

By minting tokens, we decrease the burden on the selection algorithm -- fewer
UTxO entries need to be selected.

## Burning as extra output

Burning tokens consumes **output value** to "the void":

$$
\text{total output} = \sum \text{user outputs} + \text{extra ada sink} + \text{burned tokens}
$$

By burning tokens, we increase the burden on the selection algorithm -- more
UTxO entries are needed.

## Integration with change generation

Minted and burned tokens are integrated into the change generation process for
**non-user-specified assets** (assets that were not in the original output set).

### Key properties

The change bundles must maintain these properties throughout minting and
burning operations:

1. The number of change bundles **equals** the number of user-specified outputs
2. The change bundles are in **ascending partial order**
3. The change bundles maximise the number of **large** bundles

### Adding minted values

Minted tokens are added to the **largest** change bundle (the last element in
the ascending-ordered list):

```
Before minting 4 of asset A:

    [ [          ("B",  7) ]
    [ [("A", 1), ("B",  8) ]
    [ [("A", 2), ("B",  9) ]
    [ [("A", 3), ("B",  9) ]
    [ [("A", 4), ("B", 12) ]   <-- add here

After:

    [ [          ("B",  7) ]
    [ [("A", 1), ("B",  8) ]
    [ [("A", 2), ("B",  9) ]
    [ [("A", 3), ("B",  9) ]
    [ [("A", 8), ("B", 12) ]   <-- increased by 4
```

Adding to the largest bundle trivially maintains the ascending partial order.

### Removing burned values

Burned tokens are removed from the **smallest** change bundles first,
traversing left to right:

```
Before burning 4 of asset A:

    [ [          ("B",  7) ]   <-- start here (already 0)
    [ [("A", 1), ("B",  8) ]   <-- reduce by 1
    [ [("A", 2), ("B",  9) ]   <-- reduce by 2
    [ [("A", 3), ("B",  9) ]   <-- reduce by 1
    [ [("A", 4), ("B", 12) ]

After:

    [ [          ("B",  7) ]   <-- unchanged (was already 0)
    [ [          ("B",  8) ]   <-- reduced by 1, eliminated
    [ [          ("B",  9) ]   <-- reduced by 2, eliminated
    [ [("A", 2), ("B",  9) ]   <-- reduced by 1
    [ [("A", 4), ("B", 12) ]
```

!!! note "Why smallest first?"
    Removing from the smallest bundles preserves the ascending partial order
    and removes the "least useful" bundles first, maximising the overall
    usefulness of the remaining change.

### Formal property: ascending partial order

For a list of token maps $[m_1, m_2, \ldots, m_n]$ to be in ascending partial
order, we require:

$$
\forall\, i < j: \quad m_i \leq m_j
$$

where $\leq$ is the partial order on `TokenMap` (every quantity in $m_i$ is
less than or equal to its counterpart in $m_j$).

Both the minting and burning operations preserve this property, as proven by
the following arguments:

- **Minting**: adding to the maximum element cannot violate the ordering of
  any pair $(m_i, m_n)$ since $m_n$ only increases.

- **Burning**: reducing from left to right reduces $m_i$ by at most $q_i$
  (its own quantity), so $m_i \leq m_{i+1}$ is maintained because $m_{i+1}$
  is reduced by a smaller or equal amount.
