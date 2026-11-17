# Tasks: cardano-node 11.2 pre-release

- [x] T001 Verify the upstream pre-release tag, revision and release notes.
- [x] T002 Record specification and implementation plan before editing pins.
- [x] T003 Update `flake.nix` and regenerate the runtime input in `flake.lock`.
- [x] T004 Build node and CLI and verify their reported versions.
- [x] T005 Check wallet protocol/config compatibility and run local-cluster validation.
- [x] T006 Review the final diff and record validation evidence and limitations.

## Validation evidence

- `nix build --quiet --no-link --print-out-paths .#cardano-node .#cardano-cli`
  succeeded; the binaries report node 11.2.0 and CLI 11.6.0.0.
- `nix flake metadata --quiet --json --no-write-lock-file` confirms runtime
  revision `dac0d7ab2dfdee1f9eda901589e3c6ac8eaa1899`.
- Compared root input lock records against HEAD: no changes outside the
  runtime root input. Its transitive dependencies follow the upstream lock.
- Source review: the pinned consensus package supports V23 and the wallet's
  stake-distribution query already uses `GetStakeDistribution2`.
- `cardano-node config resolve --config configs/cardano/preview/config.json`
  exits 1 because the legacy node parser and cardano-config represent
  snapshot/query-batch defaults differently. It also warns about ignored
  `hasEKG`/`hasPrometheus` keys. Upstream describes parser divergence as a
  non-fatal warning during normal flat-config startup. No config migration
  has been made; this diagnostic does not establish runtime failure.
- `nix develop --quiet --command bash -c 'fourmolu --mode check
  lib/primitive/lib/Cardano/Wallet/Primitive/Ledger/Shelley.hs && hlint
  lib/primitive/lib/Cardano/Wallet/Primitive/Ledger/Shelley.hs'` passed.
- Built `local-cluster` and `test-local-cluster-exe`; ran the latter with
  `--match withNetwork --jobs 1` using node 11.2.0 and CLI 11.6.0.0.
  All three examples failed at node startup: the existing Dijkstra genesis
  lacks required key `minPoolMargin`. Logs are under
  `/tmp/cardano-wallet-node-11.2-cluster-logs`. The user subsequently approved
  updating the cluster genesis and rerunning.
- The wallet, local-cluster and test-runner Nix build completed successfully.
- Replaced the cluster Dijkstra genesis with node 11.2.0's upstream testnet
  template. With that configuration, all three `withNetwork` examples pass
  against 11.2.0 (63.56 seconds); logs confirm `NodeToClientV_23` acceptance.
- The same wallet client passes the connection/tip-query example against
  node 11.1.3 and CLI 11.2.3.0 (21.89 seconds). That release also accepts V23;
  the wallet still advertises every version from V16 through V23. The old
  runtime was obtained with `--override-input` and `--no-write-lock-file`.
- Full local-cluster suite against node 11.2.0 completed with two jobs:
  40 examples, 1 failure. The failure is `ResourceBusy` opening
  `withNetwork-can-get-collect-the-incoming-blocks-process.log`: the existing
  first-block and first-balance tests share that log name. All other tests,
  including faucet submission, passed. Output is
  `/tmp/cardano-wallet-node-11.2-full-suite.log`. The user approved rerunning
  sequentially; no harness changes have been made.
- Full sequential local-cluster suite against node 11.2.0: **40 examples,
  0 failures**, 162.31 seconds. Output is
  `/tmp/cardano-wallet-node-11.2-full-sequential.log`. This uses the Nix-built
  wallet, cluster, test runner, node and CLI, and reads the updated genesis
  through `LOCAL_CLUSTER_CONFIGS` from this checkout.
- Final lock review: all 13 changed/added lock nodes belong to the old or new
  runtime dependency graph. `git diff --check` passes. The cluster genesis
  checksum matches the upstream 11.2.0 testnet template exactly.

## Limits

Validation covers Linux and the existing Conway local cluster. Cross-platform
builds, preprod E2E tests and Dijkstra-era wallet behavior were not exercised.
Wallet library pins remain unchanged. The preview-config parser diagnostic
described above still reports its default-representation discrepancy.
