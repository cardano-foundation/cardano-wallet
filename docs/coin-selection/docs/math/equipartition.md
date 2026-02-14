# Equipartition

An **equipartition** of a natural number $n$ into $k$ parts is a partition
where all parts differ by at most 1.

## equipartitionNatural

$$
\text{equipartitionNatural}(n, k) = [p_1, \ldots, p_k]
$$

where:

$$
p_i \in \left\{ \left\lfloor \frac{n}{k} \right\rfloor, \left\lceil \frac{n}{k} \right\rceil \right\}
\quad\text{and}\quad
\sum_{i=1}^{k} p_i = n
$$

The result is sorted in ascending order.

### Examples

```haskell
>>> equipartitionNatural 10 (() :| [(), ()])
3 :| [3, 4]

>>> equipartitionNatural 7 (() :| [(), ()])
2 :| [2, 3]
```

### Implementation

`equipartitionNatural` is implemented as a special case of
[`partitionNatural`](partitioning.md) where all weights are 1:

```haskell
equipartitionNatural n count =
    NE.reverse $ unsafePartitionNatural n (1 <$ count)
```

## equipartitionQuantitiesWithUpperBound

This function splits a `TokenBundle` into multiple bundles such that no
individual token quantity exceeds a given upper bound.

Given a bundle $b$ and maximum quantity $q_{\max}$:

$$
\text{equipartitionQuantitiesWithUpperBound}(b, q_{\max}) = [b_1, \ldots, b_m]
$$

where:

- For each asset $a$ and each $b_i$: $\text{qty}(b_i, a) \leq q_{\max}$
- $\sum_i b_i = b$ (the total is preserved)
- $m$ is minimised

The number of parts needed for a single asset with quantity $q$ is:

$$
m(q) = \left\lceil \frac{q}{q_{\max}} \right\rceil
$$

The overall number of parts is the maximum across all assets:

$$
m = \max_a \left\lceil \frac{\text{qty}(b, a)}{q_{\max}} \right\rceil
$$

## equipartitionAssets

Splits a `TokenBundle` into $k$ bundles by evenly distributing the **assets**
(not quantities) across the parts, then equipartitioning each asset's quantity.

This is used when a change output has too many distinct assets to fit in a
single transaction output. The bundle is repeatedly halved until each part
is within the size limit.

## Usage in coin selection

These functions are used during change generation to split oversized change
outputs:

1. **`splitBundlesWithExcessiveAssetCounts`** -- uses `equipartitionAssets`
   to split bundles that exceed the maximum token bundle size.

2. **`splitBundlesWithExcessiveTokenQuantities`** -- uses
   `equipartitionQuantitiesWithUpperBound` to split bundles containing
   quantities that exceed the protocol maximum.
