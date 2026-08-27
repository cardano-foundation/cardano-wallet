# Functions model

## `scripts/ci/dijkstra-stub-gate.sh` — landed unchanged

Frozen artefact, `sha256:6304802d788cd8371fd0ec0214e23e083c77e892966b34a7884663e4e66ae79f`.
Its surface is fixed and is **not** authored by this ticket:

```
dijkstra-stub-gate.sh [tree-root]        # tree-root defaults to "." (repo root)
  exit 0  total <= MAX
  exit 1  total >  MAX          a stub was added
  exit 2  self-check failed     the instrument cannot count, or cannot return zero
```

Internal, unchanged: `count <file> <keyword>` → integer, where `<keyword>` is
`error` or `pendingWith`.

## `scripts/ci/dijkstra-census-negative-control.sh` — new

```
dijkstra-census-negative-control.sh [tree-root]     # tree-root defaults to "."
  exit 0  the gate went red on the seeded tree      the gate is able to fail
  exit 1  the gate did NOT go red, or the harness could not run
```

Constraints on its signature and effects:

- It takes no ratchet argument and hard-codes no count. It asserts "one more
  stub than this tree holds makes the gate exit 1", so it stays correct as
  children lower `MAX`.
- Its only write is one throwaway `.hs` file under `<tree-root>/lib/`, removed
  on every exit path including failure and signal.
- It must fail if that file already exists, rather than overwrite it.
- It exits non-zero on any gate exit status other than 1 — including 0 (the gate
  did not notice the stub) and 2 (the instrument is broken).
