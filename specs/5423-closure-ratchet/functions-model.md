# 5423 — Functions model

Signature-level contract only: names, arguments, and what each returns or emits.
No bodies, no algorithms.

## M-1 `scripts/ci/cardano-api-closure-gate.sh`

**Invocation:** `cardano-api-closure-gate.sh [tree-root]` — `tree-root`
defaults to `.`.

**Exit:** `0` at or below every ratchet · `1` any row above its ratchet ·
`2` a self-check failed, i.e. the instrument is not trustworthy and no count it
printed may be believed.

**Environment:** `CARDANO_API_CLOSURE_LIB_MAX`, `CARDANO_API_CLOSURE_ANY_MAX`,
`CARDANO_API_SUPPRESSIONS_MAX` — each an integer overriding that row's ratchet.

**Required stdout, machine-readable, one key per line.** These are the contract
M-2 and the ticket gate parse; their spelling is part of the interface.

| line | meaning |
|---|---|
| `packages = <n>` | size of the discovered `lib/*` population |
| `closure-lib = <n>   (MAX=<n>)` | row value and its ratchet |
| `closure-any = <n>   (MAX=<n>)` | row value and its ratchet |
| `suppressions = <n>   (MAX=<n>)` | row value and its ratchet |
| `licence closure-lib: ...` | what a zero on that row licenses |
| `licence closure-any: ...` | what a zero on that row licenses |
| `licence suppressions: ...` | what a zero on that row licenses |
| `witness cardano-wallet-read: in-closure-dependents=<n> closure-lib=<yes\|no> closure-any=<yes\|no>` | measured, reported, never asserted |
| `witness cardano-wallet-blackbox-benchmarks: closure-lib=<yes\|no> closure-any=<yes\|no>` | measured, reported, never asserted |
| `self-check: fixture=<PASS\|FAIL> population=<PASS\|FAIL> falsification=<PASS\|FAIL>` | the three self-check families |
| `GATE RED: <row> <n> > MAX=<n> — a <thing> was ADDED.` | printed once per rising row, before exit 1 |
| `RATCHET SLACK: <row> <n> < MAX=<n> ...` | printed once per fallen row; does not change the exit status |
| `GATE GREEN: ...` | printed only when no row rose |
| `NOTE: GATE GREEN does not mean the ratchet is current. ...` | printed on every green run |

**Internal functions** (names and arguments fixed so the auditor can address
them; bodies are the commit owner's):

| function | arguments | returns / emits |
|---|---|---|
| `discover_packages` | `tree_root` | one `name<TAB>cabal_path` line per `lib/*` package found |
| `extract_edges` | `cabal_path`, `package_name` | one `package<TAB>stanza_kind<TAB>field<TAB>dep` line per dependency, with `COMMON` imports already attributed to the importing stanza's kind |
| `closure_rows` | edge stream on stdin, `target_package` | `closure-lib` and `closure-any` counts and their member lists |
| `count_suppressions` | `tree_root` | occurrence count and one `path:line` per occurrence |
| `build_fixture` | `dir` | a self-contained package tree exercising: a library edge to the target; a non-library-only edge to the target; a package depended on by both that reaches the target by no path; a `common` stanza whose `build-depends` names the target, imported by a library; a name sharing the target's prefix; a 2-cycle; and a self-edge |
| `self_check` | `dir` | `PASS`/`FAIL` per self-check family; any `FAIL` exits 2 |

`build_fixture`'s tree is the permanent form of the exclusion, grammar and
termination demonstrations (INV-10..INV-13). `self_check` must be shown able to
return `FAIL`.

## M-2 `scripts/ci/cardano-api-closure-negative-control.sh`

**Invocation:** `cardano-api-closure-negative-control.sh [tree-root]`.

**Exit:** `0` only when the pristine run exited 0 **and** all three seeded runs
produced their expected measured per-row deltas **and** each seeded run exited
1 · `1` anything else, including a delta that is not the expected one — that is
this control failing, not the gate.

**Required stdout, one key per line, per seeded row `<r>`:**

| line | meaning |
|---|---|
| `pristine closure-lib=<n> closure-any=<n> suppressions=<n> exit=<n>` | the untouched tree, read from M-1's stdout |
| `seed <r> path=<path relative to tree-root>` | what it created |
| `seeded <r> closure-lib=<n> closure-any=<n> suppressions=<n> exit=<n>` | with the seed present |
| `delta <r> closure-lib=<n> closure-any=<n> suppressions=<n>` | seeded minus pristine |
| `verdict <r> = PASS\|FAIL <reason>` | bound to the measured deltas, not to the exit alone |

**Expected deltas, which the control asserts rather than assumes:**

| seed | `closure-lib` | `closure-any` | `suppressions` |
|---|---:|---:|---:|
| a new package whose **library** depends on the target | +1 | +1 | 0 |
| a new package whose **benchmark only** depends on the target | 0 | +1 | 0 |
| a new `*.hs` file carrying the pragma at line start | 0 | 0 | +1 |

The second row is what proves the closure rows are two computations rather than
one number printed twice (INV-3); a `closure-lib` delta of anything but 0 there
is a control failure.

**Seed ownership:** each seed path is refused if it already exists, created
without clobbering, owned only after a successful create, and removed on every
exit path including signals. No seed is placed under `lib/integration/`.
