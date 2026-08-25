# Functions model: Shutdown drain

Artifact ceiling: 70 lines / 5 KiB

## FUN-5326-DRAIN

```text
drain
  :: Ord key
  => WorkerRegistry key resource
  -> IO ()
```

Signature constraints and effects:

- exported from `Cardano.Wallet.Registry`;
- idempotent;
- terminates every worker belonging to the drain operation;
- returns only after their resource release and worker-finalization actions;
- returns with the registry empty;
- safe when a selected worker exits concurrently.

The exact internal synchronization representation is implementation-owned.

## FUN-5326-UNREGISTER

Existing signature remains unchanged. Its observable scope remains one key;
the proof must show a different registered worker stays alive.

## FUN-5326-SERVE-WALLET

`serveWallet` keeps its existing public signature and exit result. Its effect
contract changes so the Byron, Icarus, Shelley, and multisig API-layer
registries are released through `FUN-5326-DRAIN` on normal and asynchronous
scope exit.

Any helper introduced solely to express the API-layer resource scope is
application-owned and must not widen the public application API without a
contract challenge.
