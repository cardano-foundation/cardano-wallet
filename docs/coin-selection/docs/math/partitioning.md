# Partitioning Natural Numbers

The `partitionNatural` function from `Cardano.Numeric.Util` distributes a
natural number into parts proportional to a list of weights. It is used
throughout the coin selection algorithm to split quantities fairly.

## Definition

Given a target $n \in \mathbb{N}$ and weights $w_1, w_2, \ldots, w_k$ with
$W = \sum_i w_i > 0$:

$$
\text{partitionNatural}(n, [w_1, \ldots, w_k]) = [p_1, \ldots, p_k]
$$

where each $p_i$ is within unity of the ideal proportion:

$$
\left| p_i - n \cdot \frac{w_i}{W} \right| < 1
$$

## Algorithm

The algorithm computes the partition in five steps:

### Step 1: Compute ideal (rational) proportions

$$
q_i = n \cdot \frac{w_i}{W} \in \mathbb{Q}
$$

### Step 2: Attach indices

Associate each $q_i$ with its original position $i$ so the ordering can be
restored later.

### Step 3: Sort by fractional part

Sort the indexed proportions in **descending** order of their fractional parts
$\{q_i\}$, breaking ties by descending integral part $\lfloor q_i \rfloor$.

### Step 4: Apply roundings

Compute the shortfall:

$$
s = n - \sum_{i=1}^{k} \lfloor q_i \rfloor
$$

Round **up** the first $s$ elements (those with the largest fractional parts)
and round **down** the rest:

$$
p_{\sigma(i)} = \begin{cases}
\lceil q_{\sigma(i)} \rceil & \text{if } i \leq s \\
\lfloor q_{\sigma(i)} \rfloor & \text{if } i > s
\end{cases}
$$

where $\sigma$ is the permutation from step 3.

### Step 5: Restore original order

Sort by the original indices to produce the final result.

## Guarantees

1. **Length preservation**: $|\text{result}| = |\text{weights}|$

2. **Sum preservation**: $\sum_i p_i = n$

3. **Proportionality**: each $p_i$ is within 1 of the ideal proportion

4. **Non-negative**: all $p_i \geq 0$

## Examples

```haskell
>>> partitionNatural 9 (1 :| [1, 1])
Just (3 :| [3, 3])

>>> partitionNatural 10 (1 :| [])
Just (10 :| [])

>>> partitionNatural 30 (1 :| [2, 4, 8])
Just (2 :| [4, 8, 16])
```

## Usage in coin selection

The `partitionNatural` function is used in several places:

- **`makeChangeForCoin`**: distributes surplus ada across change outputs
  proportionally to the original output coin values.

- **`makeChangeForUserSpecifiedAsset`**: distributes surplus token quantities
  across change outputs proportionally to the original output quantities of
  that asset.

- **`equipartitionNatural`**: a special case where all weights are equal,
  producing an equipartition.
