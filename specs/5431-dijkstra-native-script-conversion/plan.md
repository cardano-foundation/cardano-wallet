# Dijkstra native script conversion — plan

## Base

Branches from `feat/5429-dijkstra-converter-mirrors`, which sits on
`refactor/5422-delete-era-split`, which sits on `master`. Targets the branch
directly below it, never `master`.

Measured with `scripts/ci/dijkstra-stub-gate.sh` on the tree at the merge base
`d75c8365aa`, twice — once on this worktree and once in a separate detached
checkout of that commit: total **39**, declared ratchet **39**, with this
ticket's two sites contributing **one each** as read from the per-file
breakdown. The target is therefore **37** with the ratchet at 37 in the same
tree.

Every commit below this branch is docs-only, so the stack has not moved the
count yet. The planned end state of the slices below is lower than 39; this
plan records what the tree says rather than that projection, and re-measures
if the base moves again.

## Constraints

- `cardano-ledger-dijkstra` is pinned at `0.3.0.0` by `cabal.project`. Every
  claim about the Dijkstra script type is read from that version.
- `Script` and `KeyRole` belong to `cardano-addresses` and are not changed here.
- `AnyScript` and `AnyExplicitScript` are serialized into the public REST API.
  Any change to them carries the swagger regeneration step.
- The census script is not edited except for its ratchet declaration.

## Ordered slices

Each is bisect-safe on its own.

1. **Widen the conversion.** `toWalletScript` loses its `Timelock` equality
   constraint and takes `NativeScript era`. Existing callers are unaffected
   because their `NativeScript era` is already `Timelock era`. No behaviour
   changes in any era that exists today. This slice does not touch the two
   Dijkstra sites and does not move the census.

2. **Resolve the guard case.** Blocked on the open decision in `spec.md`. Gives
   `RequireGuard` its total outcome and, if the decision requires it, adds the
   corresponding case to `AnyScript` and `AnyExplicitScript` with the swagger
   regeneration that follows.

3. **Delete both stubs.** Both Dijkstra arms call the widened conversion. The
   census falls by two and the ratchet declaration moves with it in the same
   tree.

Slice 1 is available now. Slices 2 and 3 are not started before the decision,
because it determines the conversion's result type and therefore slice 1's
signature is the only part that is settled.

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

## Live boundary

None. Both functions are pure decoders over a transaction already in memory.
The boundary that matters is the ledger type, and it is exercised by
constructing real Dijkstra native scripts rather than by mocking them.
