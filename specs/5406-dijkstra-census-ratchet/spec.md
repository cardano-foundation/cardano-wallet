# Spec — CI: enforce the Dijkstra stub census ratchet on every PR

**Issue:** #5406 (child 0 of epic #5209)
**Branch:** `fix/5209-dijkstra-census-gate`
**Base:** `origin/master` `0a7332482c49537d8169a7a3424a6ce72329c6d5`

## Observable outcome

The Dijkstra stub census runs in CI on every pull request and on every push to
`master`, is **green on `master` today**, and goes **red the moment anyone adds
a Dijkstra stub** under `lib/`.

## Why

Epic #5209's acceptance criterion is a census reaching zero. Until CI executes
that census, the criterion is a script in a runtime directory. A check the
pipeline does not run reports green whatever its assertions say. This ticket is
deliberately first in the epic: it is the instrument every other child's claim
is measured with.

Its predecessor criterion — `grep -rn "DijkstraEra not yet supported" lib/` —
passed with 38 of 44 stubs live, because that string is one of 29 distinct stub
literals and 4 stubs split their message across source lines with Haskell string
gaps. This ticket lands a counting, multi-line-aware instrument in its place.

## Requirements

- **REQ-1** The census instrument is present in the repository and runnable
  with no arguments from the repository root by a person who has just cloned.
- **REQ-2** The instrument is byte-identical to the frozen artefact
  `sha256:6304802d788cd8371fd0ec0214e23e083c77e892966b34a7884663e4e66ae79f`.
  It is landed, not redesigned.
- **REQ-3** CI executes it on `pull_request` and on `push` to `master`,
  unconditionally — not behind another job's success.
- **REQ-4** CI proves on **every run** that the gate can fail, by seeding one
  throwaway Dijkstra stub into the real checked-out tree and requiring exit 1.
- **REQ-5** The ratchet value `MAX=44` lives in exactly one place and is
  lowered by a child's PR in one obvious line.
- **REQ-6** No Haskell source changes. No stub is retired here.
- **REQ-7** The new CI wiring references no secret and no credential.

## Rejection behaviour

- Adding a Dijkstra `error` stub or a Dijkstra `pendingWith` under `lib/`
  raises the total above 44 → gate exits 1, CI red.
- Breaking the instrument so it can no longer count, or so it can no longer
  return zero → gate exits 2, CI red. A broken instrument reports as broken,
  never as clean.
- Removing or neutering the CI wiring → the negative control no longer runs and
  the workflow no longer appears in the checks list.

## Explicit non-goals

- **Not** lowering `MAX` below 44. That is done by the children of #5209 that
  actually retire stubs, in the same PR that retires them.
- **Not** narrowing the census. There is no exclusion mechanism of any kind and
  none is added. The five stubs in `lib/wallet/src/Cardano/Api/Extra.hs` stay in
  the denominator even though #5290 will eventually delete that shim: a
  criterion that shrinks its own denominator when the denominator is
  inconvenient is worse than the grep it replaced, because it looks rigorous.
- **Not** closing #5209. Merging this closes #5406 only. 44 stubs are still live
  on `master` the moment after it merges. #5209 closes when the ratchet reaches
  `MAX=0`.

## Observable success

```sh
./scripts/ci/dijkstra-stub-gate.sh
# total = 44 across 15 files   (ratchet MAX=44)
# GATE GREEN: 44 stubs, at or below the ratchet.        exit 0

./scripts/ci/dijkstra-census-negative-control.sh
# seeds one stub, requires exit 1, removes it            exit 0

DIJKSTRA_STUB_MAX=0 ./scripts/ci/dijkstra-stub-gate.sh
# GATE RED: 44 Dijkstra stubs > ratchet MAX=0           exit 1   (correct today)
```

plus a green **"Dijkstra Stub Census"** check on the PR, with both the seeded
RED and the tree GREEN visible in its run log.
