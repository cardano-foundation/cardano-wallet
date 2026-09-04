# 5423 — Plan

## Strategy

Copy the shape of the sibling instrument merged in #5407
(`scripts/ci/dijkstra-stub-gate.sh`, `scripts/ci/dijkstra-census-negative-control.sh`,
`.github/workflows/dijkstra-census.yml`). Do not edit those files; duplicating
two short shell scripts is cheap, and coupling this work to a consolidation
branch is not. Sharing them is a later ticket.

The instrument is plain POSIX-ish `bash` with `awk`, `grep`, `find` and `sort`
over the checked-out tree. It invokes no `cabal`, needs no network, and does not
build. The full stanza-aware transitive computation over all `lib/*` packages
was measured at ~0.31s during planning.

## Decision: standalone workflow, not a job in `ci.yml`

Taken on the sibling's three reasons, each verified against this repository at
`6b42c36b58` rather than assumed:

1. **Concurrency.** `ci.yml:9-11` declares `group: ci-${{ github.ref }}` with
   `cancel-in-progress: true`. A job folded into `ci.yml` can be cancelled by a
   later push; a separate workflow carries its own group and cannot be.
2. **`needs:`.** Every candidate host job in `ci.yml` carries one — the shell
   lint matrix that would be the natural home is `quality-checks` at
   `ci.yml:686-688`, `needs: build-gate-quality`. An unrelated build failure
   would skip this check.
3. **A skipped check looks green.** Combined with (2), that is the exact failure
   this ticket exists to make impossible.

All three transfer without qualification, so the gate ships as
`.github/workflows/cardano-api-closure.yml`. This is additive, which also keeps
the additive-only fence (INV-16) true by construction; that consequence did not
drive the decision.

## Live boundary

The instrument's real boundary is **CI**, not a laptop. A control that only ever
ran locally is a claim (INV-8), so the negative control is a workflow step
beside the gate, unconditional on any other job, and ordered **before** it — a
gate that has stopped being able to fail must be caught before any of its green
is believed.

The instrument's other boundary is the **`.cabal` grammar**. The real tree
cannot exercise every shape the grammar permits, so the shapes it cannot reach
(INV-13 `import:` inheritance into a library, INV-11 cycles, INV-12 name
prefixes) are exercised on a fixture tree the instrument constructs and then
discards. A fixture-based control is permanent; a control pinned to today's
package names retires itself the moment the migration succeeds.

## Why the exclusion demonstrations are fixture-based

Acceptance requires each row to demonstrate a case it correctly excludes. Two of
the three named witnesses are real packages: `cardano-wallet-read` (in neither
row, while in-closure packages depend on it) and
`cardano-wallet-blackbox-benchmarks` (in `closure-any`, out of `closure-lib` —
the only package where the rows disagree, and therefore the only real case that
proves they are two computations).

Pinning a hard assertion to either name makes the instrument fail when the
migration succeeds: when `closure-any` reaches 0, `blackbox-benchmarks` leaves
it. So the **falsifiable** demonstrations live on the fixture, where they cannot
rot, and the real-tree witnesses are **measured and reported** on every run —
the count of closure members depending on `cardano-wallet-read`, and the
computed membership of both witnesses. A reported witness that has retired is
information; a hard-coded one is a future red build for a branch that did
nothing wrong.

## Ratchet transport

Three maxima, one per row, each overridable by environment variable for the
controls and for a future tightening ticket. The negative control contains no
ratchet value and no count, so lowering a `MAX` later cannot invalidate it.

## Slices

One slice. The gate script, its controls and the workflow are one instrument:
landing the script without the workflow ships a check nothing runs, which is the
defect class this ticket exists to close. Splitting them is not bisect-safe in
the sense that matters here — it is bisect-safe and *useless* at the midpoint.

- **S-1** `cardano-api` closure and suppression ratchet: gate script, negative
  control, workflow, and the land-time measurement recorded in the PR body.

## Verification

No Haskell, Cabal, `cabal.project` or Nix input changes, so no build is
required — and that is asserted mechanically (INV-17) rather than claimed. The
ticket gate runs:

- `./scripts/shellcheck.sh` over the two new scripts (CI runs the same script
  over all of `scripts/**/*.sh`, measured at ~5s locally through its `nix-shell`
  shebang);
- the gate on the pristine tree, requiring exit 0 and the three measured rows;
- the negative control, requiring exit 0 (it exits 0 only when all three seeded
  rows went red for the measured reason);
- the advisory direction per row: `MAX` set one above the measured value,
  requiring the slack message and exit **0**;
- a YAML parse of the new workflow and a check that the paths it names exist and
  are executable;
- the fence: no `lib/integration/**` path and none of #5407's three files in the
  diff.

Every exit status is read immediately, never after a pipe, and every probe
prints both the classification and the status so a probe that lies can be caught
contradicting itself.

## Out of scope

- Lowering any `MAX` below its land-time measurement.
- Consolidating with #5407's scripts.
- Any change under `lib/integration/**` (#5412's fence) or to `specs/5419-*`.
