# Spec: Drain wallet database workers on shutdown

Issue: https://github.com/cardano-foundation/cardano-wallet/issues/5326

Base: `origin/master` at `346786a112534347564242ac77c58c32c2788d16`

Artifact ceiling: 180 lines / 12 KiB

## P1 user story

As a wallet operator, when I stop `cardano-wallet serve`, every loaded
wallet database worker releases its acquired resource before the process
exits.

## Current defect

`Cardano.Wallet.Registry.register` forks each worker around a bracketed
resource. Explicit deletion calls `unregister`, but `serveWallet` creates the
Byron, Icarus, Shelley, and multisig API layers without a matching shutdown
release. The shipped POSIX path converts SIGTERM to `UserInterrupt` on the
main thread; sibling wallet workers are therefore abandoned unless the main
lifecycle drains their registries.

Current locations, re-derived on the bound base:

- registry membership and worker fork: `lib/wallet/src/Cardano/Wallet/Registry.hs`;
- explicit deletion: `Server.hs:1942-1960`;
- API-layer worker startup: `Server.hs:5842-5884`;
- four API layers and server lifetime: `Application.hs:377-468`;
- SIGTERM conversion: `lib/launcher/src/Cardano/Startup/POSIX.hs:43-49`.

## Requirements

- **R-5326-01 Registry drain.** The registry exposes an idempotent drain
  operation. It terminates every worker registered for the drain, tolerates
  workers exiting concurrently, and returns with the registry empty.
- **R-5326-02 Completion barrier.** Drain does not return until every drained
  worker's resource release and worker finalization actions have completed.
  The proof covers multiple workers and is demonstrated RED against an
  intentionally incomplete implementation that only removes entries or only
  signals workers without awaiting completion.
- **R-5326-03 API-layer lifetime.** `serveWallet` brackets all four wallet API
  layers—Byron, Icarus, Shelley, and multisig—so their registries drain on
  normal return and asynchronous shutdown.
- **R-5326-04 Shipped signal smoke.** A bounded smoke starts at least two
  wallet workers, enters the shipped SIGTERM handling path, and proves both
  close callbacks complete before the tested process exits. A timeout is
  finite and a pass reports the callback count.
- **R-5326-05 Compatibility.** Explicit deletion unregisters only the selected
  wallet. Existing exit-code and SIGTERM/SIGINT semantics do not change.

## Invariants

All severities are `ADVISORY` under the role taxonomy: none directly changes
chain state, money, or a signature. They remain mandatory ticket criteria.

- **INV-5326-EMPTY (ADVISORY):** after drain returns, lookup cannot observe a
  worker that belonged to the drained registry; repeated drain is harmless.
- **INV-5326-FINALIZERS (ADVISORY):** every acquired resource in a drain has
  completed release and worker-after actions before drain returns. Its
  negative control must kill a deliberately non-waiting implementation.
- **INV-5326-RACE (ADVISORY):** worker self-exit concurrent with drain cannot
  deadlock, throw a membership error, or leave a registry entry.
- **INV-5326-FOUR-LAYERS (ADVISORY):** every wallet API-layer registry has the
  same lifetime as the server scope on normal and exceptional return.
- **INV-5326-LIVE-SIGNAL (ADVISORY):** the real SIGTERM-to-`UserInterrupt`
  path observes two completed resource releases before process exit, under a
  finite deadline.
- **INV-5326-DELETE-ONE (ADVISORY):** deleting one wallet does not terminate a
  different wallet worker.
- **INV-5326-SIGNALS (ADVISORY):** process exit codes and SIGTERM/SIGINT
  translation remain unchanged.

## Non-goals

- Removing or redesigning `cardano-api`.
- Changing SQLite schema, migrations, retry policy, or database engine.
- Redesigning wallet-worker responsibilities or startup concurrency.
- Draining node, stake-pool, NTP, UI, or DRep services.
- Any work for issue #5350 / PR #5363 or any secret-bearing configuration.

## Observable completion

The committed proof suite covers every invariant, the final ignored gate is
green, the focused live smoke executes rather than skips, the exact accepted
SHA is green in GitHub CI, and the PR remains open for Pawel's review.
