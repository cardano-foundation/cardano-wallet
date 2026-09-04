# 5423 — Functions model

Signature-level contract only: names, arguments, and what each returns or emits.
No bodies, no algorithms.

## M-1 `scripts/ci/cardano-api-closure-gate.sh`

**Invocation:** `cardano-api-closure-gate.sh [tree-root]` — `tree-root`
defaults to `.`.

**Exit:** `0` at or below every ratchet · `1` any row above its ratchet ·
`2` a self-check failed, i.e. the instrument is not trustworthy and no count it
printed may be believed.

**Environment:**

- `CARDANO_API_CLOSURE_LIB_MAX`, `CARDANO_API_CLOSURE_ANY_MAX`,
  `CARDANO_API_SUPPRESSIONS_MAX` — each an integer overriding that row's ratchet.
- `CARDANO_API_CLOSURE_SELFTEST_BREAK` — `fixture` or `population`; forces that
  self-check family to FAIL so INV-15 can be demonstrated without editing the
  script. It can only turn a green run red, never the reverse, so it is not a
  bypass; a value it does not recognise is itself an error.

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
| `self-check: fixture=<PASS\|FAIL> population=<PASS\|FAIL>` | the two self-check families |
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

**Environment:** M-2 invokes M-1 in the ambient environment and never clears
or overrides the three ratchet variables. The ticket gate relies on this to
raise one row while it perturbs that row, so that a correct control is not
failed for a perturbation the probe itself introduced.

**Seed ownership:** each seed path is refused if it already exists, created
without clobbering, owned only after a successful create, and removed on every
exit path including signals. No seed is placed under `lib/integration/`.

## Population refusal (INV-14)

`cardano-api-closure-gate.sh <root>` where `<root>` has no `lib/`, or a `lib/`
containing no `*.cabal`, must exit **2** and must not print a green run over a
population of zero. A count computed over a silently shortened set is a lower
bound wearing the denominator's name; the refusal is what stops it being
reported as a zero. That refusal is itself falsified by the ticket gate, which
runs the gate against both shapes and requires exit 2 from each.

---

# Version 2 — after submission 1's audit

Version 1 said the pinned stdout lines are "the contract M-2 **and the ticket
gate** parse". That sentence is the defect. `./gate.sh` is untracked and
gitignored, so half of the contract was guarded by a checker that expires when
the ticket closes — and the audit demonstrated the consequence: a mutant gate
stripped of six contract lines, with the advisory fall branch inverted into a
hard failure, produced output from the shipped control that was **byte-identical**
to the real candidate's. The whole shipped CI surface could not tell the two
apart.

Version 2 moves those assertions into the change set. It supersedes the
corresponding Version 1 clauses; everything Version 1 says that is not
contradicted here still stands.

## V2-1 — M-2 asserts the whole published contract, not three lines of it

`cardano-api-closure-negative-control.sh` parses and asserts **every** line the
stdout contract pins, on the pristine run:

- the three `<row> = <n>   (MAX=<n>)` lines (already present);
- `packages = <n>`, non-empty;
- all three `licence <row>: ...` lines;
- both `witness ...` lines;
- the `self-check: ...` line, and that it reports `PASS`;
- the `NOTE: GATE GREEN does not mean the ratchet is current` line.

A missing or malformed line is a control failure with a named reason, exactly as
a wrong delta is. The ticket gate keeps its own copies of these checks; it is now
an **additional** checker, never the only one.

## V2-2 — M-2 exercises both branches of the exit contract

Version 1's control seeded three rises and required exit 1. It never ran the
fall branch, which is half of the stated contract and the half this repository
has already been burned by.

M-2 additionally, for each row, runs M-1 with that row's `MAX` raised by one and
requires:

- exit **0**, and
- a `RATCHET SLACK: <row> <n> < MAX=<n>` line naming **that** row, and
- no `GATE RED` line.

A control that goes red here is asserting the opposite of the intended
behaviour, so the leg must be shown able to fail in both directions.

## V2-3 — the RED line names the row that actually rose (ratifies CAND-1)

For each seeded rise, M-2 requires the emitted `GATE RED:` line to name **that
row**. Version 1 bound the exit status and the three row values but not the row
named in the message, so a gate that computes correctly and names the wrong row
shipped green.

## V2-4 — the suppression positive control instantiates the product

`build_fixture` instantiates one case per **(file class × spelling)** cell —
`*.hs`, `*.cabal`, `cabal.project*` crossed with `-Wno-deprecations` and
`-fno-warn-deprecations`, six cells — and the fixture self-check requires the
exact expected count, so dropping any single matcher or file class makes it
`FAIL`. Version 1's fixture instantiated two of the six, and three verified
point mutants survived it.

### The cell count is reported, so V2-4 is checkable without reading the source

A fixture that instantiates the product is invisible from outside, and a claim
only a reader can check is a lead rather than evidence. So the fixture derives
its cell set from the two lists — file classes and spellings — rather than
listing cases, verifies one seeded suppression per cell, and M-1 reports the
extent it actually covered:

```
self-check: fixture=<PASS|FAIL> population=<PASS|FAIL> cells=<n>
```

`<n>` is measured, never a literal. The fixture `FAIL`s if the derived product
is empty or shorter than the six cells the three file classes and two spellings
require — the quantifier must not range over a truncated set and report success
having tested nothing. M-2 asserts under V2-1 that `cells` is present and at
least 6.

This supersedes Version 1's `self-check:` line, which carried two fields.

This is INV-18. It is not a suppression-specific rule: `closure-lib` and
`closure-any` already satisfy it, which is why eight mutants against them were
killed and three against the suppression row were not.

## V2-5 — `build-tool-depends` exclusion is reported (ratifies CAND-2)

`data-model.md` requires the excluded tool-dependency edges to be "recorded and
reported separately so its exclusion is visible rather than silent"; Version 1's
stdout contract listed no such line, so the two documents disagreed. They agree
now. M-1 prints:

| line | meaning |
|---|---|
| `excluded build-tool-depends = <n>` | tool-dependency edges seen and deliberately not counted |

and M-2 asserts its presence under V2-1. There is no live instance in `lib/*`
today, which is precisely why the line has to exist before there is one.
