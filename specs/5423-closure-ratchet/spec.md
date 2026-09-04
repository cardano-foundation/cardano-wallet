# 5423 — CI ratchet over the `cardano-api` closure and its deprecation suppressions

Issue: cardano-foundation/cardano-wallet#5423 (parent #5237, milestone #113).
Base: `master` at `6b42c36b58`.

## Problem

Nothing observes the size of the `cardano-api` surface. A tenth `lib/*` package
can take a `cardano-api` dependency and land green, and so can a tenth
deprecation suppression. The removal effort has no instrument that can go red
when its own subject grows.

## Observable outcome

One CI check, running on every pull request and every push to `master`,
computes three counters over the checked-out tree and exits non-zero when any
of them rises above a ratcheted maximum.

| row | counts | what a **zero** licenses |
|---|---|---|
| `closure-lib` | `lib/*` packages whose **library** transitively depends on `cardano-api` | no production library depends on `cardano-api` |
| `closure-any` | `lib/*` packages reaching `cardano-api` through **any** stanza | the `cardano-api` pin can be deleted from `cabal.project` |
| `suppressions` | deprecation-suppression pragmas and `ghc-options:`, both spellings | no new suppression has landed unobserved |

Neither closure row is the other's restatement. `closure-lib` reaching 0 while
`cardano-wallet-benchmarks` still names `cardano-api` in `benchmark db` and
`benchmark api` would report the removal done while the dependency is still
built; `closure-any` reaching 0 would block acceptance of the `cardano-api`
removal on benchmark work that is not part of it. Two rows, each labelled with
what it licenses, say the true thing in both directions.

## Requirements

- **REQ-1** One CI check computes all three counters and exits 1 if any rises.
- **REQ-2** Each row prints, at runtime, what a zero on that row licenses.
- **REQ-3** Each `MAX` equals the value measured at land time, with that
  measurement shown in the PR. No row lands with slack.
- **REQ-4** A negative control seeds one violation **per row — all three** and
  requires exit 1 for each. It runs in CI beside the gate.
- **REQ-5** A positive control shows each counter counts what it claims,
  including both suppression spellings.
- **REQ-6** Each row demonstrates a case it correctly excludes (INV-10).
- **REQ-7** The computation terminates on cycles and self-edges.
- **REQ-8** Additive only. No file under `lib/integration/**`, and no edit to
  `#5407`'s three files.

## Rejection behaviour

- A row above its `MAX` → the run prints `GATE RED`, names the row and both
  numbers, and exits **1**.
- A row below its `MAX` with `MAX` unchanged → the run prints a per-row slack
  message naming the value to lower `MAX` to, and exits **0**.

That asymmetry is deliberate. A hard failure on slack turns a retirement made
on another branch into a red build for a branch that did nothing wrong, and
this repository has already produced that case. It is stated rather than
inferred: the run's own output must say that `GATE GREEN` means nothing was
added, not that the ratchet is current, and that tightening a fallen row is an
obligation of whoever lands the change, checked at review.

## Invariants

| ID | Statement | Fails when |
|---|---|---|
| INV-1 | `closure-lib` = `lib/*` packages with a **library-only** build-depends path to `cardano-api` | a direct-edge count is used; four packages are transitive-only today and reach 0 while the closure is non-empty |
| INV-2 | `closure-any` = `closure-lib` ∪ `lib/*` packages any of whose stanzas build-depends on a member of `closure-lib ∪ {cardano-api}` | a stanza-blind or fully-transitive-through-any-stanza reading is used |
| INV-3 | The two closure rows are two computations | a perturbation that moves `closure-any` alone also moves `closure-lib`, or vice versa |
| INV-4 | Suppressions are counted **by form**: `^{-# OPTIONS_GHC` in `*.hs`, anchored `ghc-options:` in `*.cabal` and `cabal.project*`, both spellings | one spelling is missed (three sites), or prose is excluded by discarding a file population instead of by form |
| INV-5 | A rise exits 1; a fall with `MAX` unchanged prints slack and exits 0 | either direction is inverted or collapsed |
| INV-6 | Each `MAX` equals the land-time measurement | a seeded violation passes because the row had slack |
| INV-7 | Every row prints its licence, and the run states that green ≠ current | a number is read as the criterion printed next to it |
| INV-8 | The negative control seeds each row independently, binds the measured per-row delta, requires exit 1, and runs in CI unconditional on any other job | a red unrelated to the seeded delta is accepted; or the control only ever ran on a laptop |
| INV-9 | Positive control: each counter counts what it claims, both spellings included | a counter that always returns its ratchet passes |
| INV-10 | Exclusions are demonstrated on a fixture the instrument builds: a package depended on by closure members but reaching `cardano-api` by no path is in neither row; a package reaching it only through a non-library stanza is in `closure-any` and not `closure-lib`; prose is not counted | proximity or co-occurrence is used instead of a directed-edge computation |
| INV-11 | The computation terminates on self-edges and cycles, proven on a fixture containing both | a naive recursive walk hangs or double-counts (`cardano-wallet-read` depends on itself) |
| INV-12 | Dependency names are compared as whole package names | `cardano-api-extra` is counted as `cardano-api` |
| INV-13 | `build-depends` carried by a `common` stanza is attributed to each stanza that imports it, with the importing stanza's kind | a library that inherits a `cardano-api` edge through `import:` is missed; two such `common` stanzas already exist and today feed only non-library stanzas, so the real tree cannot exercise this |
| INV-14 | The package population is read out of the tree; the computation refuses to report success over an empty or truncated population, and that refusal is itself falsified | a gate that must be edited to keep covering its subject silently stops covering it |
| INV-15 | Every control is demonstrated red before its green is believed | a control that has stopped being able to fail |
| INV-16 | Additive fence holds (REQ-8) | a sibling lane's file is touched |
| INV-17 | The change set contains no Haskell, Cabal, `cabal.project` or Nix input, asserted mechanically | the "no build needed" claim is asserted rather than measured |

## Land-time measurement (planning-time value, to be re-measured)

Measured independently at `6b42c36b58`, reproducing the issue's populations
exactly: `closure-lib = 12`, `closure-any = 13`, `suppressions = 9`;
`cardano-wallet-blackbox-benchmarks` is the sole member of
`closure-any \ closure-lib`; `cardano-wallet-read` is in neither row while
in-closure packages depend on it. **These are not the values to ship.** The
commit owner re-measures at land time and shows that measurement.
