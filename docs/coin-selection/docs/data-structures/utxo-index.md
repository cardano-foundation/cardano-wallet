# UTxO Index

The `UTxOIndex` is the core data structure powering coin selection. It indexes
a UTxO set by asset identifier, enabling efficient lookup and random selection
of UTxOs containing a particular asset.

## Problem

Given a UTxO set with thousands of entries, the selection algorithm needs to
repeatedly find a UTxO containing a specific asset. A naive linear scan is
$O(n)$ per lookup, making the overall selection $O(n \cdot k)$ where $k$ is
the number of distinct assets.

## Solution

The `UTxOIndex` maintains several indices over the same UTxO set:

```haskell
data UTxOIndex u = UTxOIndex
    { indexAll        :: MonoidMap Asset (Set u)
    , indexSingletons :: MonoidMap Asset (Set u)
    , indexPairs      :: MonoidMap Asset (Set u)
    , balance         :: TokenBundle
    , universe        :: Map u TokenBundle
    }
```

| Field | Description |
|-------|-------------|
| `indexAll` | All UTxOs containing a given asset |
| `indexSingletons` | UTxOs containing *only* that asset (plus ada) |
| `indexPairs` | UTxOs containing that asset and exactly one other |
| `balance` | Total balance across all entries |
| `universe` | Complete mapping from UTxO id to value |

## Selection filters

The index supports three selection filters, tried in priority order:

```haskell
data SelectionFilter asset
    = SelectSingleton asset    -- UTxO with only this asset
    | SelectPairWith  asset    -- UTxO with this asset + one other
    | SelectAnyWith   asset    -- Any UTxO with this asset
```

### Why this priority?

Selecting a UTxO that contains only the desired asset avoids "collateral
damage" -- accidentally pulling in large quantities of unrelated assets.

| Filter | Collateral damage | Use case |
|--------|-------------------|----------|
| `SelectSingleton` | None | Ideal: clean, targeted selection |
| `SelectPairWith` | Minimal (one extra asset) | Good compromise |
| `SelectAnyWith` | Potentially high | Fallback when others fail |

## Random selection

```haskell
selectRandom
    :: MonadRandom m
    => UTxOIndex u
    -> SelectionFilter Asset
    -> m (Maybe ((u, TokenBundle), UTxOIndex u))
```

The function:

1. Looks up the set of UTxOs matching the filter
2. Randomly selects one from the set
3. Returns the selected UTxO and the index with that entry removed

Random selection is key to the self-organising property of the algorithm --
over time, it produces a UTxO distribution that mirrors the user's payment
patterns.

### selectRandomWithPriority

Tries a list of filters in order, returning the first successful selection:

```haskell
selectRandomWithPriority
    :: MonadRandom m
    => UTxOIndex u
    -> NonEmpty (SelectionFilter Asset)
    -> m (Maybe ((u, TokenBundle), UTxOIndex u))
```

## Operations

| Operation | Complexity | Description |
|-----------|------------|-------------|
| `fromMap` | $O(n \cdot a)$ | Build from a `Map u TokenBundle` |
| `insert` | $O(\log n \cdot a)$ | Insert a single entry |
| `delete` | $O(\log n \cdot a)$ | Delete a single entry |
| `lookup` | $O(\log n)$ | Look up a UTxO by id |
| `balance` | $O(1)$ | Total balance of all entries |
| `size` | $O(1)$ | Number of entries |
| `assets` | $O(1)$ | Set of all asset identifiers |
| `selectRandom` | $O(\log n)$ | Random selection with filter |

where $n$ is the number of UTxOs and $a$ is the average number of assets per
UTxO.

## Invariant

The index maintains an internal invariant ensuring consistency between the
universe (the complete map) and the various asset indices. This invariant is
checked by `checkInvariant` and is verified in the test suite after every
operation.

The invariant ensures:

1. Every UTxO in the universe appears in exactly the correct index entries
2. The balance equals the sum of all values in the universe
3. A UTxO appears in `indexSingletons` for asset $a$ iff it contains only
   asset $a$ and ada
4. A UTxO appears in `indexPairs` for asset $a$ iff it contains asset $a$
   and exactly one other non-ada asset
