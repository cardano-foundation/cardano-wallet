# Tasks: Drain wallet database workers on shutdown

Artifact ceiling: 80 lines / 5 KiB

## Slice `shutdown-drain` — OWNER

- [x] T5326-S1 Add the Registry drain contract with idempotent, race-safe,
      empty-on-return semantics.
- [x] T5326-S2 Prove with multiple workers that drain waits for every resource
      release and worker-finalization action; freeze RED evidence against an
      intentionally non-waiting implementation before GREEN.
- [x] T5326-S3 Preserve selected-wallet unregister behavior and prove a second
      wallet worker remains registered/alive.
- [x] T5326-S4 Bracket the Byron, Icarus, Shelley, and multisig API layers in
      `serveWallet` so normal and asynchronous exit drain all four registries.
- [x] T5326-S5 Ship a finite-timeout smoke/integration proof that starts at
      least two workers, triggers the shipped SIGTERM path, and observes every
      close callback before process exit.
- [x] T5326-S6 Preserve existing exit-code and SIGTERM/SIGINT semantics and
      leave unrelated service lifetimes unchanged.
- [x] T5326-S7 Pass the immutable focused gate, ticket gate, fresh audit, exact
      final-tree proof, relevant local CI commands, and GitHub CI.
- [x] T5326-S8 Final commit subject `fix: drain wallet workers on shutdown`
      with a `Tasks:` trailer listing T5326-S1 through T5326-S8.
