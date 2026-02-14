# Partial Orders

The `TokenMap` and `TokenBundle` types use **partial ordering** instead of
total ordering. This page explains why, and how partial orders are used in the
coin selection algorithm.

## Why no Ord instance?

Consider two token maps:

```haskell
p = fromFlatList [(assetA, 2), (assetB, 1)]
q = fromFlatList [(assetA, 1), (assetB, 2)]
```

Neither $p \leq q$ nor $q \leq p$ holds -- they are **incomparable**.

A total ordering (like lexicographic) could be defined, but it would not be
consistent with the arithmetic properties of token maps. For example,
under lexicographic ordering we might have $p > q$, but we could not conclude
that $p - q$ is non-negative (it would be $(1, -1)$, which is not a valid
token map).

!!! danger "Type error on Ord"
    Both `TokenMap` and `TokenBundle` generate a **compile-time type error**
    if you try to use them with `Ord`:

    ```
    Ord not supported for token maps
    Ord not supported for token bundles
    ```

    This prevents accidental misuse.

## The partial order

The `PartialOrd` instance defines:

$$
x \leq y \iff \forall\, a: \text{qty}(x, a) \leq \text{qty}(y, a)
$$

In Haskell:

```haskell
instance PartialOrd TokenMap where
    leq = MonoidMap.isSubmapOf `on` unTokenMap
```

### Examples

```haskell
x = fromFlatList [(assetA, 1)]
y = fromFlatList [(assetA, 2), (assetB, 1)]
-- x `leq` y  ==  True  (x is strictly less than y)

p = fromFlatList [(assetA, 2), (assetB, 1)]
q = fromFlatList [(assetA, 1), (assetB, 2)]
-- p `leq` q  ==  False (incomparable)
-- q `leq` p  ==  False (incomparable)
```

## TokenBundle partial order

For `TokenBundle`, which combines a `Coin` with a `TokenMap`:

$$
(c_1, m_1) \leq (c_2, m_2) \iff c_1 \leq c_2 \land m_1 \leq m_2
$$

```haskell
instance PartialOrd TokenBundle where
    b1 `leq` b2 =
        (&&)
            (coin b1 <= coin b2)
            (tokens b1 `leq` tokens b2)
```

## Lexicographic ordering (when needed)

When an arbitrary total ordering is needed (e.g., for use as a `Map` key or
`Set` element), both types provide a `Lexicographic` newtype:

```haskell
newtype Lexicographic a = Lexicographic { unLexicographic :: a }

instance Ord (Lexicographic TokenMap) where
    compare = comparing (toNestedList . unLexicographic)
```

## Usage in coin selection

The partial order is fundamental to the change generation algorithm:

- **Ascending partial order of change maps**: the non-user-specified change
  maps are maintained in ascending partial order, which ensures that when
  combined with user-specified change, the smallest maps are paired with the
  smallest.

- **Balance sufficiency check**: the algorithm checks whether the available
  UTxO balance is sufficient by verifying that the required balance is less
  than or equal (in the partial order) to the available balance.

- **Selection delta**: the surplus/deficit of a selection is computed using
  the truncated subtraction $(\backslash\!\!>)$, which is the monus operation
  on the partial order.

### The `inAscendingPartialOrder` predicate

The `Cardano.Numeric.Util` module provides:

```haskell
inAscendingPartialOrder :: (Foldable f, PartialOrd a) => f a -> Bool
```

This checks that consecutive pairs satisfy `leq`, and is used in property
tests to verify that change maps are correctly ordered.
