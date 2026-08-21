# Feature Specification: Byron Random Wallets Cannot Sign For Pre-2018 Addresses

**Feature Branch**: `011-byron-random-address-signing`
**Created**: 2026-08-11
**Status**: Draft
**Input**: Byron random-derivation wallets discover, display and offer for spending certain addresses
whose recorded derivation index does not identify the key the address was built from. Signing derives
at the recorded index and produces a key that does not correspond to the address, so the node rejects
every transaction spending those UTxOs with `MissingVKeyWitnessesUTXOW`. Balances are reported as
spendable and no error is surfaced until submission.

## Background

Byron random-derivation addresses carry their derivation path inside the address, encrypted under a
key derived from the wallet's root public key. `addressToPath` recovers that path and `isOwned`
derives a signing key from it, without ever checking that the derived key reproduces the address it
is about to sign for.

For addresses created by `cardano-sl` before commit `4ebc883d`
(*[CSL-1955] Updated generateUnique to always generate hardened addresses*, 2017-11-28, released
between March and June 2018), the index recorded in the address is **not** the index the key was
derived at. Such addresses were already known to exist: issue #1041 documented that Byron addresses
may carry indexes outside the hardened domain, and #1042 introduced `Index 'WholeDomain` so that they
would be *discovered*. Neither issue considered whether they could be *spent*.

The result is that affected wallets restore correctly, show a correct balance, and fail only at
submission — silently, and for the entire lifetime of the wallet.

The funds are not lost. Signing with the key at the hardened form of the recorded index produces a
valid witness, which has been confirmed on a test network. What is missing is for the wallet to try
that derivation, and to check the key it produces before using it.

Note that a soft index alone does not imply the defect. An address whose key genuinely is at the soft
index it records signs correctly. The fault is the mismatch between recorded index and actual key,
which correlates with soft indexes only because pre-2018 addresses are the ones affected.

## Clarifications

### Session 2026-08-11

- Q: When an address decrypts as ours but no candidate derivation reproduces it, what should the
  wallet do beyond returning no signing key? → A: Nothing further. The input contributes no witness,
  and the transaction is rejected at submission as it is today. No new local error and no new API
  error surface; the change stays confined to key resolution.
- Q: How far does automated test coverage go for the network-level criteria? → A: Both user stories
  get an automated integration scenario against a local cluster — a mixed affected and unaffected
  spend (SC-001, SC-003) and a migration (SC-004) — in addition to unit coverage of key resolution.
- Q: How is the performance criterion verified, given that no existing benchmark exercises Byron
  signing? → A: It is restated as a structural bound that a unit test can assert — one derivation and
  one reconstruction per input on the recorded path — rather than a latency measurement. No
  benchmark is added.

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Spending From An Affected Address (Priority: P1)

A user restores a Byron wallet created before 2018. The wallet syncs, discovers its addresses and
reports a balance that matches the chain. When the user sends any amount that selects a UTxO on one
of the affected addresses, the node rejects the transaction. Repeating the attempt fails identically,
and no part of the interface indicates why.

**Why this priority**: These funds are currently unspendable through the wallet, indefinitely, with
no diagnostic. Any wallet holding a pre-2018 address is affected regardless of how much it holds.

**Independent Test**: Construct an address whose payload records index *i* while its root is derived
from the key at *i* hardened, fund it, and attempt to spend. Fails before the change; succeeds after.
Confirmed on a test network — such an address was funded, found unspendable, and then recovered by
signing at the hardened index. Automated as an integration scenario against a local cluster, spending
an affected and an unaffected UTxO in one transaction.

**Acceptance Scenarios**:

1. **Given** a Byron random wallet owning an address whose key is at the hardened form of its
   recorded index, **When** the user submits a transaction spending a UTxO on that address,
   **Then** the transaction is accepted by the node.
2. **Given** the same wallet, **When** `isOwned` is asked for that address's signing key,
   **Then** the returned key reproduces the address exactly.
3. **Given** a wallet owning both affected and unaffected addresses, **When** a transaction spends
   UTxOs from both in one transaction, **Then** every bootstrap witness validates.

---

### User Story 2 - Migrating An Affected Wallet (Priority: P2)

A user with a legacy wallet uses the existing migration endpoint to move all funds to a Shelley
wallet. The migration plan includes UTxOs on affected addresses, so the migration transaction is
rejected and the wallet cannot be emptied by the supported route.

**Why this priority**: Migration is the recommended path off legacy wallets and the primary interface
users are directed to. It fails for exactly the users least able to diagnose it. Depends on the same
fix as User Story 1 but is independently observable and independently valuable.

**Independent Test**: Run `POST /v2/byron-wallets/{id}/migrations` against a wallet containing at
least one affected address and confirm the migration completes. Automated as an integration scenario
against a local cluster.

**Acceptance Scenarios**:

1. **Given** a Byron wallet containing affected addresses with funds, **When** a migration plan is
   created, **Then** the plan includes those UTxOs.
2. **Given** that plan, **When** the migration is executed, **Then** the transaction is accepted and
   the wallet balance reaches zero.

---

### User Story 3 - No Silent Wrong Keys (Priority: P3)

Any Byron random address for which no derivable key reproduces the address is reported as not owned,
rather than yielding a key that will fail at submission.

**Why this priority**: The severity of this defect comes from its silence — a wrong key is
indistinguishable from a right one until the node rejects it. Refusing to return an unverified key
confines any future occurrence of this class to one testable boundary: `isOwned` returns a key that
provably matches the address, or none. Such an input then carries no witness and the transaction is
still rejected at submission; the wallet raises no new local error. Valuable even if no further
variants exist.

**Independent Test**: Present the wallet with an address that decrypts to a valid path but whose root
matches no candidate derivation, and confirm no signing key is produced.

**Acceptance Scenarios**:

1. **Given** an address whose payload decrypts under the wallet's passphrase but whose root matches
   no candidate derivation, **When** `isOwned` is called, **Then** it returns `Nothing`.
2. **Given** an address belonging to a different wallet, **When** `isOwned` is called, **Then** it
   returns `Nothing`, as today.

### Edge Cases

- **Both candidates reproduce the address.** Cannot occur: distinct derivation indexes yield distinct
  public keys, and the address root commits to the public key. The first match is unambiguous.
- **Account index also outside the hardened domain.** #1041 records that account indexes were
  produced by the same defective function. The account level is therefore covered as well as the
  address level (FR-007). No wallet with an affected account index has been observed, but the
  affected population cannot be enumerated, so this is handled rather than assumed away.
- **Two addresses recording the same path.** Legitimate: an affected and an unaffected address can
  record identical paths and differ only in the key used. Classification must be per address, by
  comparison, never by path alone.
- **Index already hardened.** Where a candidate coincides with one already tried, it must not be
  derived twice. Candidates should be de-duplicated before evaluation.
- **Addresses that decrypt but belong to no derivable key.** Must not produce a signing key
  (User Story 3). This class cannot arise today, because ownership and key resolution currently test
  the same condition; it exists only once resolution is verified. The input then contributes no
  witness and the transaction is rejected by the node, as it is today.
- **Performance.** `isOwned` runs per input during signing. Verification is unconditional, so every
  input costs one public-key computation and one address reconstruction. Only the fallback path costs
  further key derivations, so a wallet holding no affected addresses stays at one derivation and one
  reconstruction per input (SC-006).

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: The system MUST verify that a derived signing key reproduces the address it was derived
  for before that key is used to sign.
- **FR-002**: When the key derived at an address's recorded index does not reproduce the address, the
  system MUST attempt derivation at the hardened form of that index.
- **FR-003**: The system MUST return no signing key for an address that no candidate derivation
  reproduces, rather than returning an unverified key.
- **FR-004**: The system MUST NOT change behaviour for addresses whose recorded index already
  identifies the correct key; these must continue to resolve on the first attempt.
- **FR-005**: Address discovery MUST remain unchanged. Ownership determination is out of scope; only
  the key returned for an owned address changes.
- **FR-006**: The fix MUST apply to every path that signs for Byron random wallets, including
  ordinary payments and wallet migration, without changes at those call sites.
- **FR-007**: The system MUST consider candidates at the account level as well as the address level.
  Issue #1041 records that account indexes were produced by the same defective function and are
  "subject to the same rule". `cardano-wallet` is a general-purpose backend with an unknown set of
  downstream consumers, so the affected population cannot be enumerated and must not be assumed to
  match any single client's behaviour. Covering both levels costs one further derivation on a path
  that only executes once an earlier candidate has already failed.
- **FR-008**: Candidate evaluation MUST stop at the first candidate that reproduces the address, so
  that the common case — an address whose recorded path is correct — performs exactly one derivation
  and one comparison.
- **FR-009**: The absence of a signing key MUST NOT introduce a new error surface. Signing paths keep
  their types, callers keep their behaviour for an unresolved address, and no API error is added.
  Verification is observable at the `isOwned` boundary and nowhere else.

### Key Entities

- **Recorded derivation path**: the account and address indexes encrypted into a Byron random
  address. Recovered from the address itself; for pre-2018 addresses it does not necessarily identify
  the key the address was built from.
- **Candidate derivation**: a path at which the signing key may lie for a given address. The recorded
  path, and variants of it with the address index hardened, the account index hardened, or both.
  Evaluated in that order, most likely first.
- **Address reconstruction**: deriving the key at a candidate path, computing the address it produces,
  and comparing with the target address. The only reliable test of which candidate is correct.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: A wallet owning an address whose key is at the hardened form of its recorded index can
  spend that address's UTxOs; the node accepts the transaction.
- **SC-002**: A wallet owning only unaffected addresses shows no change in behaviour, in the keys
  returned, or in the addresses it discovers.
- **SC-003**: A single transaction spending UTxOs from both affected and unaffected addresses is
  accepted, with every bootstrap witness validating.
- **SC-004**: `POST /v2/byron-wallets/{id}/migrations` empties a wallet containing affected addresses.
- **SC-005**: No signing key is ever returned that does not reproduce the address it is for; an
  address matching no candidate yields no key.
- **SC-007**: An address whose account index, rather than its address index, fails to identify its
  key is resolved by the same mechanism.
- **SC-006**: For an address whose recorded path identifies its key, resolution performs exactly one
  key derivation and one address reconstruction. A wallet holding no affected addresses performs no
  additional derivation, on any input.

## Assumptions

- The affected population is wallets whose addresses were created before the `cardano-sl` fix
  described above, and only those that drew an index outside the hardened domain. Neither the number
  of such wallets nor the sums involved are known.
- Reproducing an address from a candidate key can be done using the attributes carried by the target
  address itself, so no network identifier needs to be threaded into `isOwned`. This avoids adding a
  constraint that would propagate to the wallet-flavour dispatch and its call sites.
- Hardening an index recovers the correct key for affected addresses. This has been confirmed on a
  test network: an address of this shape, unspendable by current code, was swept successfully by
  signing with the key at the hardened form of its recorded index, in a transaction that also
  carried a healthy address's input under its own witness. Should some affected address match no
  candidate, FR-003 ensures it fails visibly rather than silently, and the candidate set can be
  extended without changing the surrounding design.
- The set of clients that have created Byron random wallets against this backend is unknown. No
  requirement here assumes a particular client's index-generation behaviour.
- Discovery is already correct for these addresses — they are found and their balances reported. Only
  signing is defective.
- No database migration is required. Nothing persisted changes; the correct key is recomputed on
  demand from material already available.
