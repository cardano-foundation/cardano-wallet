# Plan — land the Dijkstra census ratchet in CI

## Strategy

Land an already-written, already-falsified instrument and wire it so the
pipeline executes it. Nothing about the instrument is redesigned here.

## Placement decisions

### Instrument → `scripts/ci/dijkstra-stub-gate.sh`

`scripts/ci/` is where this repository keeps CI-invoked shell checks
(`check-local-test-targets.sh`, `check-pr-body-closing-link.sh`,
`check-docker-boot-sync-cleanup.sh`). Those take the same `root=${1:-.}` shape,
so the instrument's existing `[tree-root]` argument needs no change and it runs
from the repository root with no arguments.

Landing there also puts it under two checks that already exist:

- `scripts/shellcheck.sh` lints every `scripts/**/*.sh` (verified clean against
  `shellcheck 0.11.0 -e SC1090 --external-sources`, exit 0);
- `scripts/enforce-eol.sh` requires a trailing newline (present).

### Wiring → a new `.github/workflows/dijkstra-census.yml`, not `ci.yml`

Two reasons, in order of weight:

1. **It must not be skippable.** Every quality check in `ci.yml` sits behind
   `needs: build-gate-quality`. A census that does not run when an unrelated nix
   build fails is a census that reports nothing exactly when the tree is
   churning. The whole point of this child is that the pipeline executes the
   criterion, so the job carries no `needs:` and depends on no other job.
2. **`ci.yml` is secret-bearing.** Its `attic-cache` job reads
   `secrets.ATTIC_TOKEN`. The commit-owner seat for this ticket is barred from
   any work touching production secrets or secret-bearing configuration. A
   separate, secret-free workflow file keeps that bar clean rather than keeping a
   barred seat "just out of frame" from a credential.

The workflow gets its own `concurrency` group so it cannot cancel `ci.yml` runs.
It uses the same `nix-enabled-runners` label as the rest of the repository, but
runs no nix: the instrument is bash + `find` + `perl`.

### Ratchet value → one line, in the instrument

`MAX=${DIJKSTRA_STUB_MAX:-44}` in `scripts/ci/dijkstra-stub-gate.sh` is the
single home of the ratchet. A child of #5209 that retires stubs edits that one
literal in the same PR. Nothing else in the repository repeats the number — in
particular the negative control does not, because it asserts "one more than
whatever the tree holds is red", not "45 is red".

## The falsification is permanent, not a one-off paste

The instrument's built-in positive and negative controls prove the
**instrument** can count and can return zero. They do **not** prove the **gate
fails when it should**. Those are different claims.

So CI seeds one throwaway Dijkstra stub into the real checked-out tree, runs the
gate, and requires exit 1 — on **every run**, before the real census. This
follows the precedent already in this repository: the `delegation-agda` job
requires the same Agda command to reject one deliberate mutation per named
#5350 law, so that check is proven able to fail on every run.

The seeded file is created and removed inside the CI step, under a `trap`. The
committed tree contains no Haskell change; the checkout is disposable.

Ordering is deliberate: negative control first, so a gate that has stopped being
able to fail is caught before its green is believed.

## Live boundary

The boundary that matters here is **the pipeline**, not the script. A gate the
build compiles but the pipeline does not run is the exact failure this child
exists to prevent. Acceptance therefore requires the job visible in the run log
of the PR head, not the job present in the workflow file.

## Slices

One bisect-safe slice.

**SL-1 — land the instrument, its falsification harness, and the workflow.**
Adds three files, changes none. `master` is green at 44 before and after, so the
slice is safe at every point of a bisect.

## Constraints

- No `lib/**/*.hs` change, and no stub retired.
- No exclusion mechanism, path prune, or skip flag added to the census; the file
  selection stays `find "$lib" -name '*.hs' -type f`, unconditional.
- `MAX` is not lowered below 44.
- The instrument is copied byte-for-byte; its sha256 is verified after landing.
