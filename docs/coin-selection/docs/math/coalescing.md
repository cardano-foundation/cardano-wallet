# Coalescing: padCoalesce

The `padCoalesce` function adjusts a list to match a target length while
preserving the total sum. It is used to distribute non-user-specified asset
quantities across change outputs.

## Definition

Given a source list $S$ and a target list $T$:

$$
\text{padCoalesce}(S, T) = S'
\quad\text{where}\quad
|S'| = |T|
\quad\text{and}\quad
\sum S' = \sum S
$$

The result $S'$ is always sorted in ascending order.

## Algorithm

The function first sorts the source list, then:

- If $|S| < |T|$: **pad** by inserting `mempty` until the lengths match
- If $|S| > |T|$: **coalesce** by merging the two smallest elements until
  the lengths match
- If $|S| = |T|$: return the sorted source unchanged

### Padding

Repeatedly insert `mempty` (zero) into the sorted list:

$$
[a_1, a_2, \ldots, a_k] \xrightarrow{\text{pad}}
[0, a_1, a_2, \ldots, a_k]
$$

Since $0 \leq a_i$ for all $i$, the ascending order is preserved.

### Coalescing

Repeatedly merge the two smallest elements:

$$
[a_1, a_2, a_3, \ldots, a_k] \xrightarrow{\text{coalesce}}
[a_3, \ldots, (a_1 + a_2), \ldots, a_k]
$$

The merged element $(a_1 + a_2)$ is re-inserted in sorted position. The
sum is preserved because $a_1 + a_2$ replaces both $a_1$ and $a_2$.

## Examples

```haskell
-- Padding: source shorter than target
>>> padCoalesce [Sum 1] (replicate 4 ())
[Sum 0, Sum 0, Sum 0, Sum 1]

-- Coalescing: source longer than target
>>> padCoalesce [Sum 8, Sum 4, Sum 2, Sum 1] (replicate 3 ())
[Sum 3, Sum 4, Sum 8]

>>> padCoalesce [Sum 8, Sum 4, Sum 2, Sum 1] (replicate 2 ())
[Sum 7, Sum 8]

>>> padCoalesce [Sum 8, Sum 4, Sum 2, Sum 1] (replicate 1 ())
[Sum 15]
```

### Worked example: coalescing [8, 4, 2, 1] to length 2

1. Sort: $[1, 2, 4, 8]$, need to reduce from 4 to 2
2. Coalesce smallest pair: $1 + 2 = 3$, insert: $[3, 4, 8]$
3. Coalesce smallest pair: $3 + 4 = 7$, insert: $[7, 8]$
4. Length matches target. Result: $[7, 8]$

Sum check: $1 + 2 + 4 + 8 = 15 = 7 + 8$

## Properties

1. **Length preservation**: $|\text{result}| = |\text{target}|$

2. **Sum preservation**: $\sum \text{result} = \sum \text{source}$

3. **Ascending order**: the result is sorted in ascending order

4. **Maximises large values**: coalescing the smallest pair at each step
   ensures that large values are preserved as much as possible

## Usage in coin selection

`padCoalesce` is used by `makeChangeForNonUserSpecifiedAsset` to distribute
the quantities of a non-user-specified asset across the correct number of
change outputs.

For example, if asset B was found in 9 selected UTxOs with quantities
$[9, 1, 8, 2, 7, 3, 6, 4, 5]$ and the user requested 5 outputs, then
`padCoalesce` reduces these 9 quantities down to 5 change values.
