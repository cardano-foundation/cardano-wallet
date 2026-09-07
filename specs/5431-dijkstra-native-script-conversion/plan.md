# Dijkstra native script conversion — plan

## Base

Branches from `feat/5429-dijkstra-converter-mirrors`, which sits on
`refactor/5422-delete-era-split`, which sits on `master`. Targets the branch
directly below it, never `master`.

Measured with `scripts/ci/dijkstra-stub-gate.sh` on the tree at the merge base
`d75c8365aa`, twice — once on this worktree and once in a separate detached
checkout of that commit: total **39**, declared ratchet **39**, with this
ticket's two sites contributing **one each** as read from the per-file
breakdown. `Convert.hs` is not in that breakdown today.

The target is **38** with the ratchet at 38: two counted stubs are deleted and
one is created in the shared conversion. Every commit below this branch is
docs-only, so the stack has not moved the count yet; re-measure if the base
moves again.

## Constraints

- `cardano-ledger-dijkstra` is pinned at `0.3.0.0` by `cabal.project`. Every
  claim about the Dijkstra script type is read from that version.
- `Script` and `KeyRole` belong to `cardano-addresses` and are not changed here.
- `AnyScript` and `AnyExplicitScript` are not changed here. They are serialized
  into the public REST API and the decision to widen them is taken elsewhere.
- The census script is not edited except for its ratchet declaration.
- No file claimed by the slice below is touched. That slice covers
  `Cardano/Wallet.hs`, `Shelley/Transaction.hs`, `Shelley/Transaction/Unsigned.hs`,
  `Pools.hs` and a local-cluster spec; this one covers `lib/primitive` only.

## Ordered slices

Both are bisect-safe.

1. **Widen the conversion.** `toWalletScript` loses its `Timelock` equality
   constraint and takes `NativeScript era`. Its unmatched branch becomes
   reachable for Dijkstra and names the era. Existing callers are unaffected
   because their `NativeScript era` is already `Timelock era`. This slice adds
   one to the census and removes none, so it is not landed alone.

2. **Route both readers through it.** Both Dijkstra arms call the shared
   conversion and their `error`s are deleted. The census falls by two here, for
   a net one across the pair, and the ratchet declaration moves with it in the
   same tree.

The two slices land together in one pull request. Slice 1 alone would raise the
count, which the census hard-fails, so splitting them across pull requests is
not available.

## Verification

The ticket gate carries one leg per failure class CI enforces — whitespace,
compile, format, hlint, the `cardano-wallet-primitive` suite, the census and
its negative control — read from `.github/workflows/`. It adds the class CI
does not enforce: the census total and the declared ratchet must be **equal**,
not merely ordered. CI hard-fails an added stub but exits 0 with a warning when
a stub is retired and the ratchet is left behind, so that half is otherwise
unchecked.

That leg was falsified in both directions before use: a retired stub with the
ratchet left at its old value exits 0 under the census alone and non-zero under
the gate.

**The census cannot see a rename.** It counts error literals that mention the
era, so a repair that merely stops naming Dijkstra lowers the count and leaves
the crash. Nothing mechanical catches that, which is why R1 says "by deletion"
and why it is checked by reading the diff.

Tests live in `lib/primitive/test/spec/Cardano/Wallet/Primitive/Ledger/ConvertSpec.hs`,
which already holds the `Timelock` roundtrip properties for `BabbageEra` and is
the Conway-side control for R3. That suite does **not** currently depend on
`cardano-ledger-dijkstra`; the dependency is added so a real Dijkstra native
script can be constructed rather than simulated.

## Live boundary

None. Both functions are pure decoders over a transaction already in memory.
The boundary that matters is the ledger type, and it is crossed by constructing
real `DijkstraNativeScript` values through the ledger's own class methods rather
than by mocking them.
