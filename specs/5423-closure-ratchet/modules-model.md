# 5423 — Modules model

Three new files. No existing file is modified.

| M-ID | Path | Responsibility | Depends on |
|---|---|---|---|
| M-1 | `scripts/ci/cardano-api-closure-gate.sh` | Compute the three counters over a tree root, print each with its licence, compare each against its ratchet, exit 1 on any rise. Owns its own positive/negative self-checks and the fixture that carries the grammar-shape and exclusion demonstrations. | nothing in-repo |
| M-2 | `scripts/ci/cardano-api-closure-negative-control.sh` | Falsify M-1 once per row: seed one violation, read M-1's own reported per-row counts before and after, require the measured delta on that row and exit 1 from M-1, then restore. Contains no ratchet value and no count. | M-1, by path, as an external process |
| M-3 | `.github/workflows/cardano-api-closure.yml` | Run M-2 then M-1 on every pull request, every push to `master`, and on demand. Own concurrency group, no `needs:`, no secrets reference. | M-1, M-2 |

## Dependency direction

`M-3 → M-2 → M-1`. M-1 knows nothing of M-2 or M-3: it reads a tree root and
reports. That direction is what lets M-2 bind its verdict to M-1's own stdout
rather than to a reimplementation of the count, and what lets a later
consolidation ticket replace M-3 without touching M-1.

## Promotion

None. The counting logic is not promoted to a shared library with #5407's
scripts in this ticket; that is a named later consolidation, and coupling this
work to another branch is a cost with no acceptance benefit here.

## Boundary the modules model owns

M-1 is one process producing one report. It does not read the ratchet from a
file another lane edits, does not call `cabal`, does not touch the network, and
writes nothing outside a private temporary directory it creates and removes.
M-2 is the only component permitted to write inside the tree, and only the seed
paths it owns, only after proving each is absent, and it removes them on every
exit path.
