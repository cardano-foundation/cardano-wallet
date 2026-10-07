# Step 7b — install-plan diff: `origin/master` (base) vs `chore/node-11.1.3` (branch)

Method: in both trees — `/code/cardano-wallet-1113-basecheck` (detached at
`origin/master` `4209dcccdfeb`) and `/code/cardano-wallet-node-11.1.3` (branch
`chore/node-11.1.3` at `4e22ac0957`) — resolved
`nix develop --accept-flake-config -c cabal build all --enable-tests --enable-benchmarks --dry-run`
(exit 0 both sides), then diffed every `pkg-name`/`pkg-version` pair from
`dist-newstyle/cache/plan.json`.

## Counts

| metric | value |
|---|---|
| packages in base plan | 611 |
| packages in branch plan | 610 |
| downgrades | **0** |
| upgrades | 6 |
| removed from plan | 1 |
| added to plan | 0 |

## Upgrades (6) — exactly the moved pin set

| package | base | branch |
|---|---|---|
| cardano-slotting | 0.2.1.0 | 0.2.2.0 |
| cardano-ledger-binary | 1.9.0.0 | 1.9.1.0 |
| cardano-ledger-allegra | 1.10.0.0 | 1.10.1.0 |
| plutus-core | 1.65.0.0 | 1.70.0.0 |
| plutus-tx | 1.65.0.0 | 1.70.0.0 |
| plutus-ledger-api | 1.65.0.0 | 1.70.0.0 |

## Removal (1)

- `deriving-aeson 0.2.10.0.0.0.0.1` (a GHC global-DB instance, type
  `pre-existing`) is in the base plan only: base `plutus-core 1.65.0.0`
  depended on it, `plutus-core 1.70.0.0` no longer does. Consequence of the
  plutus upgrade; nothing to pin, nothing to fix.

## Control (instrument proven able to fire)

A fabricated older version of a real package (`text 2.1.3` -> `2.0.2`) was
injected into a copy of the branch list and the same comparator was re-run: it
fired exactly once, `DOWN text: 2.1.3 -> 2.0.2`. CONTROL-FIRED (1).

## Verdict

No dependency moved backwards. Gate 7b: PASS.
