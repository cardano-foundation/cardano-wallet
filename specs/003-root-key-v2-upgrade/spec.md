# Feature Specification: Root Key Protection Upgrade to V2

**Feature Branch**: `003-root-key-v2-upgrade`
**Created**: 2026-05-06
**Status**: Draft
**Input**: User description: "Upgrade root-key storage to v2 envelope (Argon2id+XChaCha20-Poly1305) with opportunistic V1-to-V2 migration on wallet unlock, and expose encryption_method in GET /v2/wallets/{id} passphrase info so frontends can detect V1 keys and prompt a passphrase-change dialog if desired."

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Automatic Key Protection Upgrade on Unlock (Priority: P1)

A wallet user (or node operator running an automated service) has an existing wallet
whose root key is protected by an older scheme (V1). The next time they perform any
operation that requires their passphrase — such as signing a transaction or listing
addresses — the system silently upgrades the stored key to the latest protection scheme
(V2) without requiring any additional user interaction. The user's passphrase itself does
not change; only how it is stored is improved.

**Why this priority**: This is the core security improvement. Without it, existing wallets
remain on weaker protection indefinitely. It must work invisibly to avoid friction for the
large installed base of V1 wallets.

**Independent Test**: Can be fully tested by creating a wallet, verifying it reports V1
scheme in the API, signing a transaction, and confirming it now reports V2 scheme — with
no change to the passphrase or wallet funds.

**Acceptance Scenarios**:

1. **Given** a wallet whose passphrase info reports protection scheme `scrypt` or
   `pbkdf2-hmac-sha512`, **When** the user sends a transaction (or any other operation
   requiring passphrase verification), **Then** the wallet's stored key is upgraded to
   `argon2id-v2` and subsequent reads of the wallet info reflect the new scheme.

2. **Given** a wallet already using `argon2id-v2`, **When** any passphrase operation is
   performed, **Then** the key is left unchanged and no unnecessary re-encryption occurs.

3. **Given** a wallet with a V1 key and an **incorrect** passphrase supplied, **When** an
   operation is attempted, **Then** the operation is rejected with an authentication
   error and the key remains at V1 (no partial or corrupt migration state is written).

4. **Given** a wallet that has just been restored from its seed phrase and has never
   been unlocked since, **When** the user first unlocks it, **Then** the migration to
   V2 occurs at that point.

---

### User Story 2 - Frontend Detection of V1 Wallets (Priority: P2)

A frontend application developer wants to show users a prompt encouraging them to
re-enter their passphrase when their wallet is still using an older protection scheme.
The developer needs a stable, machine-readable field in the wallet info API response to
detect this condition programmatically.

**Why this priority**: Without this, frontends have no reliable way to prompt users
proactively. The opportunistic migration (P1) is sufficient for security, but the UI
affordance improves visibility and gives users agency.

**Independent Test**: Can be fully tested by querying `GET /v2/wallets/{id}` on wallets
in each scheme state and asserting that `passphrase.encryption_method` returns the
correct stable identifier, independent of any migration logic.

**Acceptance Scenarios**:

1. **Given** any wallet (Shelley sequential, Shelley random, or shared), **When** `GET /v2/wallets/{id}` is
   called and a passphrase was set, **Then** the response includes
   `passphrase.encryption_method` with one of the defined scheme identifiers.

2. **Given** a wallet using a V1 protection scheme, **When** the wallet info is fetched,
   **Then** `passphrase.encryption_method` returns the V1 identifier, allowing the
   frontend to display an upgrade prompt.

3. **Given** a wallet that has just been migrated to V2 (by P1 story), **When** the
   wallet info is fetched immediately after, **Then** `passphrase.encryption_method`
   returns the V2 identifier.

4. **Given** a wallet with no passphrase set (e.g., hardware wallets),
   **When** wallet info is fetched, **Then** the `passphrase` object is absent (no
   `encryption_method` field present), consistent with existing behaviour.

---

### User Story 3 - Explicit Upgrade via Passphrase Change (Priority: P3)

A user who has been prompted by their wallet frontend can explicitly trigger the upgrade
to V2 by going through the standard passphrase change flow — either changing to a new
passphrase or providing the same one. After this, their wallet reports V2 protection.

**Why this priority**: The explicit path is a convenient complement to the opportunistic
migration, particularly for wallets that are mainly used for receiving funds (no outbound
transactions) and would otherwise stay at V1 for a long time.

**Independent Test**: Can be fully tested by calling the change-passphrase endpoint on
a V1 wallet and confirming the `encryption_method` switches to V2, all without any
balance or address change.

**Acceptance Scenarios**:

1. **Given** a wallet reporting `scrypt` or `pbkdf2-hmac-sha512`, **When** the user
   submits a passphrase change (new passphrase may equal the old one), **Then** the
   wallet now reports `argon2id-v2`.

---

### Edge Cases

- What happens when the wallet database is read by an older version of the software that
  does not understand V2 keys? The stored format must be self-describing so older
  software can fail gracefully rather than silently corrupting data.

- How does the system handle a crash or power loss mid-migration? The upgrade write must
  be atomic: either the full V2 record is committed or the original V1 record remains
  intact.

- What happens when the same wallet is concurrently unlocked from two processes? The
  migration must be idempotent: two simultaneous upgrades must not produce a corrupt or
  inconsistent stored key.

- What happens when a wallet is imported from another software (e.g., Daedalus) that
  used the legacy scrypt scheme? The import must succeed, and the first unlock must
  trigger migration to V2.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: The system MUST upgrade a wallet's root-key protection scheme to V2 the
  first time the user successfully authenticates with their passphrase after the feature
  is deployed, without requiring any explicit user action beyond normal authentication.

- **FR-002**: The system MUST NOT change the user's passphrase during an automatic
  upgrade; only the storage format of the existing passphrase-protected key changes.

- **FR-003**: The wallet info endpoint MUST include a stable, machine-readable
  `encryption_method` field within the `passphrase` object, identifying which protection
  scheme currently secures the wallet's root key.

- **FR-004**: The `encryption_method` field MUST use one of three defined identifiers:
  `scrypt`, `pbkdf2-hmac-sha512`, or `argon2id-v2`. No other values are permitted.

- **FR-005**: The system MUST continue to accept and unlock wallets using any supported
  protection scheme (V1 or V2) without requiring manual re-keying or database migration
  scripts.

- **FR-006**: The migration from V1 to V2 MUST be atomic: the stored key record is
  either fully updated to V2 or left fully intact as V1. Partial writes are not
  permitted.

- **FR-007**: Following a successful V1-to-V2 migration, the wallet metadata MUST
  reflect the new scheme so that subsequent reads of wallet info return `argon2id-v2`.

- **FR-008**: The system MUST reject any passphrase-authenticated operation if the
  passphrase verification fails, regardless of the key protection scheme in use, and
  MUST NOT modify the stored key as a result of a failed attempt.

- **FR-009**: All Shelley wallet types (Shelley sequential, Shelley random, shared
  multisig) MUST be subject to the same upgrade path.

- **FR-010**: New wallets created after deployment MUST use V2 protection by default.

### Key Entities

- **Wallet**: Has a root key, passphrase info, and balance. The root key's protection
  scheme is an attribute of how the key is stored, not of the wallet itself.

- **Passphrase info**: Records when the passphrase was last changed and which protection
  scheme currently secures the stored root key. This is the entity extended by
  `encryption_method`.

- **Key protection scheme**: A versioned cryptographic envelope that wraps the wallet's
  root key. V1 schemes use PBKDF2 or scrypt for key derivation; V2 uses a stronger
  modern construction. The scheme identifier is human-readable and stable across
  software versions.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: All wallets with a V1 protection scheme are migrated to V2 within one
  successful passphrase operation after deployment, requiring zero additional steps from
  the user beyond what they would normally do.

- **SC-002**: The wallet info response for every Shelley wallet with a passphrase includes
  `passphrase.encryption_method`, with 100% coverage across all Shelley wallet types
  (sequential, random, shared).

- **SC-003**: Zero wallets experience loss of access to funds or addresses as a result
  of the migration. All funds remain spendable and all addresses remain derivable after
  V2 upgrade.

- **SC-004**: The migration adds no perceptible latency to wallet unlock operations
  from the user's perspective (target: indistinguishable from a normal unlock).

- **SC-005**: Existing integration tests continue to pass unchanged, demonstrating that
  the upgrade is fully backward-compatible with the current API contract.

## Deployment Warning: Irreversible Migration

> [!WARNING]
> **This upgrade is one-way and permanent. Once any wallet in a database has been
> migrated to V2, that database requires a binary that understands the V2 format.**
>
> - **Do not downgrade** the `cardano-wallet` binary after wallets have migrated.
>   An older binary that encounters a V2 key column will either crash the wallet
>   worker process or permanently lock the wallet (funds become inaccessible until
>   the binary is upgraded again).
> - **Back up wallet databases** before deploying this version in production
>   environments where rollback may be needed.
> - Migration is triggered silently on the first successful passphrase operation
>   per wallet; there is no opt-out and no bulk pre-migration step.

## Assumptions

- Wallets already using V2 protection (created after a previous deployment of this
  feature) are unaffected and will not be re-encrypted unnecessarily.

- The upgrade is one-directional: V1 → V2 only. Downgrading a V2 wallet to V1 is not
  a supported operation.

- The `encryption_method` field is additive to the existing `passphrase` object and
  does not break clients that ignore unknown fields (standard REST API forward
  compatibility).

- Byron (random/Icarus) and hardware/watch-only wallets are out of scope; they are not
  subject to the V2 upgrade path and continue to have no `passphrase.encryption_method`
  in the API response.

- The feature targets the `cardano-wallet` backend service; companion changes to any
  specific frontend are out of scope for this specification.

- The existing wallet database schema can represent V2 keys in the same columns used
  for V1 keys, distinguished by the format of the stored data; no schema migration is
  required.
