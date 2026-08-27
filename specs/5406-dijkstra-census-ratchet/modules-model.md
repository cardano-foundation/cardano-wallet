# Modules model

## New components

| component | responsibility | consumes | emits |
|---|---|---|---|
| `scripts/ci/dijkstra-stub-gate.sh` | count Dijkstra `error` stubs and Dijkstra `pendingWith` under `lib/`, multi-line-aware; compare against the ratchet | a tree root (default `.`); `DIJKSTRA_STUB_MAX`, `DIJKSTRA_STUB_STRICT` | per-file counts, totals, verdict; exit 0 at/below ratchet, 1 above, 2 if its own controls fail |
| `scripts/ci/dijkstra-census-negative-control.sh` | prove the gate above is able to fail, by seeding exactly one throwaway Dijkstra stub into the tree and requiring exit 1 | the same tree root; invokes the gate | pass/fail of the falsification; exit 0 when the gate correctly went red |
| `.github/workflows/dijkstra-census.yml` | execute both, in that order, on every PR and every push to `master` | the checkout | the `Dijkstra Stub Census` check |

## Dependency direction

```
dijkstra-census.yml
  ├─→ dijkstra-census-negative-control.sh ─→ dijkstra-stub-gate.sh
  └─→ dijkstra-stub-gate.sh
```

The gate has no dependency on the negative control, and neither has a dependency
on anything else in the repository. The gate is the only component that knows
the ratchet value.

## Promotion / ownership

Nothing is promoted to a shared location. The instrument is owned by epic #5209
and dies with it: when the ratchet reaches `MAX=0` and #5209 closes, the census
becomes a permanent zero-assert or is retired by that closing ticket. That
decision belongs to #5209, not here.

## Role check — does each component receive what it claims to observe?

- The gate reads `lib/**/*.hs` from the tree it is given. That **is** the
  surface the criterion is about; there is no proxy layer between them.
- The negative control observes the gate's **exit status against the real
  tree**, not a synthetic fixture. A control that seeded into a scratch
  directory would prove the instrument counts — which the gate's own built-in
  controls already prove — and would not prove that this tree, at this ratchet,
  goes red on one more stub.
- The workflow observes nothing; it exists so the pipeline executes the other
  two. Its correctness is only visible in a run log, which is why acceptance
  requires the run log.
