# Data model

- **D1 — Effective delegation**
  - `active`: the currently active `WalletDelegationStatus`.
  - `next`: ordered scheduled `WalletDelegationNext` entries.
  - `effective`: `status` of the final `next` entry when one exists, otherwise
    `active`.

State invariant: earlier `next` entries and a superseded `active` value are
history for duplicate-vote comparison; they remain available for all other
wallet semantics.
