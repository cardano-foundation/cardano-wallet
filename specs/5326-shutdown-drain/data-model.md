# Data model: Shutdown drain

Artifact ceiling: 80 lines / 5 KiB

## DM-5326-REGISTRY

`WorkerRegistry key resource` contains the currently registered workers for
one wallet flavor.

Relationships and invariants:

- one key identifies at most one registered worker;
- a registered worker exposes its thread identity, acquired resource, and a
  completion observation covering resource release and worker finalization;
- worker self-exit removes its own key safely;
- drain may race with self-exit without requiring an entry to remain present;
- drain return establishes an empty registry and completed cleanup for every
  worker selected by that drain;
- repeated drain observes the same empty state without additional effects.

No SQLite handle type, schema, retry state, or API-layer type is added to this
abstraction.

## DM-5326-APPLICATION-LIFETIME

One `serveWallet` invocation owns four wallet API-layer resources:

- Byron / random wallet layer;
- Icarus wallet layer;
- Shelley sequential wallet layer;
- shared/multisig wallet layer.

Each layer owns one `DM-5326-REGISTRY`. Normal return and asynchronous unwind
both cross the same release boundary. Node, stake-pool, DRep, NTP, and UI
resources are outside this relationship.

## DM-5326-SMOKE-EVIDENCE

The live smoke's passing observation contains:

- the tested process identity and bounded timeout;
- proof that the shipped SIGTERM handler ran;
- an acquired-worker count of at least two;
- a completed-release count equal to the acquired count before process exit;
- a non-zero failure on timeout, skip, or count mismatch.
