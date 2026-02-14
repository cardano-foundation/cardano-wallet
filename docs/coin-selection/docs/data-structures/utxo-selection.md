# UTxO Selection

The `UTxOSelection` type represents the state of a coin selection in progress.
It tracks which UTxOs have been selected and which are still available.

## Structure

A `UTxOSelection` consists of two disjoint `UTxOIndex` sets:

```haskell
data State u = State
    { leftover :: UTxOIndex u  -- UTxOs not yet selected
    , selected :: UTxOIndex u  -- UTxOs already selected
    }
```

```mermaid
stateDiagram-v2
    state UTxOSelection {
        [*] --> Leftover: fromIndex
        Leftover --> Selected: select
    }
    state Leftover {
        note right of Leftover: UTxOs available\nfor selection
    }
    state Selected {
        note right of Selected: UTxOs already\nchosen as inputs
    }
```

## Types

The module provides two types:

| Type | Guarantee |
|------|-----------|
| `UTxOSelection u` | May have zero selected entries |
| `UTxOSelectionNonEmpty u` | Has at least one selected entry |

The `UTxOSelectionNonEmpty` type is used as evidence that the selection has
made progress, which is important for constructing the final transaction
(which requires at least one input).

## Construction

```haskell
-- All UTxOs in the leftover set, none selected
fromIndex :: UTxOIndex u -> UTxOSelection u

-- UTxOs matching the predicate are pre-selected
fromIndexFiltered :: Ord u
    => (u -> Bool) -> UTxOIndex u -> UTxOSelection u

-- Construct from explicit leftover/selected pair
fromIndexPair :: Ord u
    => (UTxOIndex u, UTxOIndex u) -> UTxOSelection u
```

## Core operation: select

The `select` function moves a single UTxO from the leftover set to the
selected set:

```haskell
select :: Ord u
    => u -> UTxOSelection u -> Maybe (UTxOSelectionNonEmpty u)
```

Key properties:

- Returns `Nothing` if the given UTxO is not in the leftover set
- Always returns `UTxOSelectionNonEmpty` (the selection is guaranteed
  non-empty after a successful select)
- The total available balance remains constant:

$$
\text{availableBalance}(s) = \text{availableBalance}(\text{select}(u, s))
$$

## Balances

| Function | Returns |
|----------|---------|
| `availableBalance` | `selectedBalance + leftoverBalance` (constant) |
| `selectedBalance` | Balance of selected UTxOs |
| `leftoverBalance` | Balance of unselected UTxOs |

## Sub-selection relation

A selection $s_1$ is a **sub-selection** of $s_2$ if $s_2$ can be reached
from $s_1$ by zero or more applications of `select`:

$$
s_1 \subseteq s_2 \iff \exists\, u_1, \ldots, u_k: \text{select}(u_k, \ldots \text{select}(u_1, s_1)\ldots) = s_2
$$

This relation is checked by `isSubSelectionOf` and is useful in property
tests to verify that selections are consistent.

## Usage in coin selection

The `UTxOSelection` is threaded through the entire selection process:

1. **Start**: `fromIndex` creates a selection with all UTxOs as leftover
2. **Round-robin**: each selection step calls `select` to move a UTxO from
   leftover to selected
3. **Change generation**: the selected set is used to compute the total input
   value
4. **Result**: the selected entries become the transaction inputs; the leftover
   set is returned to the wallet
