# Data model

- **D1 — Effective delegation**
  - `active`: the currently active `WalletDelegationStatus`.
  - `next`: ordered scheduled `WalletDelegationNext` entries.
  - `effective`: `status` of the final `next` entry when one exists, otherwise
    `active`.

State invariant: earlier `next` entries and a superseded `active` value are
history for duplicate-vote comparison; they remain available for all other
wallet semantics.

- **D2 — Formal duplicate-vote observation**
  - `target`: requested DRep.
  - `effectiveDRep`: the DRep, if any, carried by D1 `effective`.
  - `decision`: `same_vote` exactly when `target = effectiveDRep`; otherwise
    the request is different.
  - `history`: D1 `active` plus every non-final `next` entry when `next` is
    non-empty.

Formal invariant: changing D2 `history` while preserving D1 `effective` cannot
change D2 `decision`. The Agda model abstracts epochs but preserves the ordered
list boundary used by `WalletDelegation`; the Haskell mirror supplies concrete
`WalletDelegationNext` epochs and statuses.

Equality reliance: D2's target comparison uses decidable DRep equality. The
Agda backend takes reflexivity and soundness as explicit parameters because
`DRep` is abstract there. The Haskell mirror compares the actual `Eq DRep`
result with an independently pattern-matched structural oracle over predefined,
key-hash, and script-hash representatives.
