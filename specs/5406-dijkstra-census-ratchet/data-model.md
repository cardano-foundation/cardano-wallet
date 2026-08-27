# Data model

No persistent data, no schema, no migration. Three values cross a boundary.

| name | type | home | validation / invariant |
|---|---|---|---|
| `MAX` | non-negative integer | `scripts/ci/dijkstra-stub-gate.sh`, `MAX=${DIJKSTRA_STUB_MAX:-44}` | monotonically **decreasing** over the life of #5209. Its terminal value `0` is #5209's acceptance criterion. Raising it is never a valid response to a red gate. |
| `DIJKSTRA_STUB_MAX` | integer, optional env | caller | overrides `MAX` for one invocation. Used by a child to preview a tightening, and by `DIJKSTRA_STUB_MAX=0` to check the terminal state. Never set in CI, so CI always measures against the committed ratchet. |
| `DIJKSTRA_STUB_STRICT` | `0`/`1`, optional env | caller | `1` turns ratchet slack (`total < MAX`) from a warning into exit 1. Not set in CI on this ticket: `master` must stay regression-only. |

## Census denominator — a state invariant, not a configuration knob

The set of files counted is defined by exactly one unconditional expression:

```sh
find "$lib" -name '*.hs' -type f
```

There is no exclude list, no path prune, no skip flag, and none is added. In
particular the five stubs in `lib/wallet/src/Cardano/Api/Extra.hs` remain
counted, though #5290 will eventually delete that shim. Carving them out would
make the criterion unable to fail on them, which is precisely the defect #5209
was re-cut to remove.

## Counted shapes

Both are matched **after** collapsing whitespace across the whole file, so a
Haskell string gap splitting a message across source lines is still one match:

| shape | example |
|---|---|
| `error "…Dijkstra…"` | `error "DijkstraEra not yet supported"` |
| `pendingWith "…Dijkstra…"` | `pendingWith "TODO: Dijkstra"` |

`error $ "…"` is matched too. A bare string mentioning Dijkstra that is neither
argument is **not** counted — that is the instrument's own negative control.
