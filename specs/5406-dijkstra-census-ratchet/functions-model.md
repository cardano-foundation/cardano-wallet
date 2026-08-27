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

---

## Version 2 — after audit submission 1 (`AUDIT-FINDINGS`, report `1b711708…5f68314f`)

Submission 1's control was accepted by its own success message rather than by a
measured effect. It seeded a file whose *explanatory comment* also contained a
counted shape, so the tree moved from `44/15` to `46/16` — the control proved
"two more stubs go red", never "one more". Separately it armed its cleanup trap
before its collision check, so a pre-existing file at the seed path was deleted
by the very command that said it refused to overwrite it.

Both are the same defect in different clothes: an assertion bound to what the
script *says* instead of to what it *did*. The revised signature makes the
effect observable from outside the script.

### `scripts/ci/dijkstra-census-negative-control.sh` — revised contract

```
dijkstra-census-negative-control.sh [tree-root]     # tree-root defaults to "."
  exit 0  the census counted exactly one more stub AND went red because of it
  exit 1  anything else
```

It must print these lines on **stdout**, one per line, machine-readable, in any
order:

```
seed=<seed path, relative to tree-root>
pristine_total=<integer>     total the census reports on the untouched tree
seeded_total=<integer>       total the census reports with the seed present
delta=<integer>              seeded_total - pristine_total
gate_exit=<integer>          the census's exit status on the seeded tree
```

Exit 0 is permitted **only** when all three hold:

- the pristine census run exited `0`;
- `delta == 1`;
- `gate_exit == 1`.

`delta != 1` is a failure of the control, not of the census: a control that
applies two mutations has not tested the ratchet's actual claim.

Constraints, unchanged from v1 unless restated:

- no count and no ratchet literal appears in the file;
- the seeded file introduces **exactly one** counted shape. No counted shape
  may appear in its comments, module header, or anywhere else in it. The
  instrument collapses whitespace across the whole file before matching, so a
  comment is not a hiding place;
- **ownership before cleanup.** The seed is created atomically and cleanup is
  armed only after that creation succeeds. On a rejected collision the
  pre-existing file is left byte-identical on every exit path, including
  `INT`, `TERM`, `HUP`, and a census that exits `2`;
- it exits non-zero on any `gate_exit` other than `1`, including `0` (the
  census did not notice) and `2` (the instrument is broken).
