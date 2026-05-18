# Quickstart: verify script-witness parity locally

This recipe lets a reviewer reproduce the parity proof inside the
`5288-script-witnesses` worktree.

## Prerequisites

- `nix develop` working in the worktree.
- `dist-newstyle` either populated locally or symlinked from a
  neighboring worktree (the orchestrator already symlinks
  `dist-newstyle` from `/code/cardano-wallet-5285/dist-newstyle`).

## One-shot gate

```bash
cd /code/cardano-wallet-5288   # adjust to your worktree path
./gate.sh
```

The gate runs:

- `scripts/ci/check-code-format.sh` (fourmolu / cabal-fmt /
  nixfmt)
- `cabal build cardano-wallet:lib:cardano-wallet
   cardano-wallet-unit:unit -O0 -v0`
- `cabal test cardano-wallet-unit:unit -O0 -v0 \
    --test-options '--match="Cardano.Wallet.Shelley.TransactionLedger"'`
- `hlint lib/wallet lib/unit/test/unit/Cardano/Wallet/Shelley`

A green run is the parity proof for this PR.

## Just the new scenarios

```bash
nix develop --quiet -c cabal test cardano-wallet-unit:unit \
    -O0 -v0 \
    --test-options \
        '--match="Cardano.Wallet.Shelley.TransactionLedger script-witness"'
```

Expect six `it`-cases under
`describe "ledger script-witness parity"` to be green.

## How a failure reads

Each `it` case compares CBOR bytes of:

- the legacy `mkUnsignedTx`-built body, after passing through
  `Read.Tx` to land in a ledger `Tx Conway`, and
- the new `buildLedgerTx`-built body.

When they diverge, the test reports the first body lens whose
value differs (inputs, outputs, reference inputs, withdrawals,
certs, mint, witness-set scripts). The
[data-model](./data-model.md) "Body-field map" table is the
lookup: each row names which legacy code line should match which
ledger lens.

## Untouchable areas

- `lib/integration/**` — not edited by this PR; integration tests
  are run unchanged as signal only.
- `lib/wallet/src/Cardano/Wallet/Shelley/Transaction.hs`
  (`mkUnsignedTx`, `mkUnsignedTransaction`) — must match
  `origin/master` byte-for-byte. The parity proof relies on it as
  the reference implementation.

## After-the-fact integration signal (optional)

The full unit suite is wider than the parity surface but cheap:

```bash
nix develop --quiet -c cabal test cardano-wallet-unit:unit -O0 -v0
```

Use this if you want to confirm no orthogonal regression slipped
in alongside the parity work.
