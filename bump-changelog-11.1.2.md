# Ecosystem changelog — `cardano-node` 11.1.1 → 11.1.2

Companion to `bump-changelog-11.1.1.md` and `bump-changelog.md`.

## What the release is for

From the upstream release notes:

> Cardano-node `11.1.2` optimizes memory usage in time lock scripts.

and, under Known issues:

> There is a slight increase in memory usage compared to node version `11.0.1` when
> **syncing**, but significantly less than the regression in 11.1.0.

So this release targets a memory regression introduced in the 11.1.x line.
The upstream notes also state that benchmarks and system-testing results were
**not re-run** for 11.1.2; 11.1.1's results are carried over.

## `cardano-node` 11.1.1 → 11.1.2

```
fef83fed0 ledger: allegra bump
```

The whole diff is four files:

```
 cabal.project                                     | 2 +-
 cardano-node/cardano-node.cabal                   | 4 ++--
 cardano-node/src/Cardano/Node/Protocol/Cardano.hs | 2 +-
 flake.lock                                        | 6 +++---
 4 files changed, 7 insertions(+), 7 deletions(-)
```

Of which the substantive line is the ledger protocol version advertised by the node:

```diff
-                                           else ProtVer (natVersion @11) 0
+                                           else ProtVer (natVersion @11) 1
```

`pvMajor` is unchanged at 11, and only the major feeds `maxMajorProtVer`, so the
obsolete-node envelope check is unaffected. The minor bump is adoption signalling
stamped into headers minted by block producers. N2N and N2C are untouched:
`ouroboros-network` and `ouroboros-consensus` bounds are identical at both tags.

## `cardano-ledger` — allegra 1.10.0.0 → 1.10.1.0

Source revisions from CHaP `meta.toml`: `f649f975` → `0372ead6`.
Relative to the wallet's current pin set (which already carries
`cardano-ledger-shelley` 1.19.0.1 at `4c81e909`), the new commits are:

```
6dc6a4e9 Remove unnecessary memory overhead in Timelocks
9a8533f3 Simplify Eq and Ord instances for Timelock
0372ead6 Bump up the version
```

Files touched:

```
modified +2/-1 eras/allegra/impl/cardano-ledger-allegra.cabal
modified +21/-6 eras/allegra/impl/src/Cardano/Ledger/Allegra/Scripts.hs
```

No test was added or changed, and `cardano-ledger-allegra` 1.10.1.0 has no
CHANGELOG entry.

### Semantic change under review

`Timelock` lost its derived `Eq` (`mbBytes x == mbBytes y && mbRawType x == mbRawType y`)
in favour of comparing `mbHash` alone, and gained an `Ord` it did not have before.
In the same release, nested sub-scripts are decoded as `MemoBytes t mempty def`, where
`def = unsafeMakeSafeHash def` is a constant.

A reproducer is being run against 1.10.1.0 with 1.10.0.0 as control before this
bump is considered mergeable. See the PR for the outcome.
