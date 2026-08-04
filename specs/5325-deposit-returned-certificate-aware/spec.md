# Spec: Make `depositReturned` certificate-aware instead of a numeric heuristic

Issue: https://github.com/cardano-foundation/cardano-wallet/issues/5325
(ADP-2298) Deposit_returned falsely reported on some incoming transactions

## P1 user story

As a wallet API consumer, when I fetch a transaction, `depositReturned`
is non-zero only when the transaction actually contains a certificate
that returns a stake-key deposit — never as a side effect of an
ordinary payment whose input/output gap happens to be small.

## Root cause (orchestrator-verified against current master)

`reclaimIfAny` (`lib/api/src/Cardano/Wallet/Api/Http/Shelley/Server.hs`,
`mkApiTransaction`'s `where` clause, ~line 5458) classifies an incoming
tx as a deposit reclaim by **numeric coincidence**:
```haskell
reclaimIfAny
    | tx ^. (#txMeta . #direction) == W.Incoming =
        if (totalIn > 0 && totalOut > 0 && totalOut > totalIn)
            && (totalOut - totalIn <= depositValue)
        then depositValue else 0
    | otherwise = 0
```
No certificate is consulted. Any ordinary incoming tx whose
`totalOut - totalIn` gap happens to fall at or under the protocol's
current stake-key deposit (`depositValue = tx ^. #txDeposit`, itself
just `W.stakeKeyDeposit pp` — a protocol parameter, not tx-specific) is
mislabeled.

**The real signal already exists and is silently discarded.** Ledger
CBOR-parsing (`lib/primitive/lib/Cardano/Wallet/Primitive/Ledger/Read/Tx/Features/Certificates.hs:169-170`)
already computes the exact refund amount per certificate straight from
the ledger's own `Ledger.UnRegDepositTxCert cred coin`:
```haskell
Ledger.UnRegDepositTxCert cred coin ->
    mkDelegationNone (Just $ fromLedgerCoin coin) cred
```
which becomes `W.CertificateOfDelegation (Just refundCoin) (W.CertDelegateNone cred)`
— a real, ledger-accurate refund amount, per certificate, in
`W.Certificate` (`lib/primitive/lib/Cardano/Wallet/Primitive/Types/Certificates.hs:209`:
`CertificateOfDelegation (Maybe Coin) DelegationCertificate`).

The `Maybe Coin` alone is not enough to identify a refund. Conway and
Dijkstra registration forms also carry their exact deposit as
`CertificateOfDelegation (Just depositCoin) (CertVoteAndDelegate ...)`.
Only the combination of `Just coin` and `CertDelegateNone` denotes a
deregistration refund. Matching `CertificateOfDelegation (Just coin) _`
would replace the original false positive with a new one for registration
transactions.

This value reaches `mkApiTransaction`'s local scope via
`parsedValues <- traverse parseTxCBOR $ tx ^. #txCBOR` (already
computed earlier in the same `do` block, `Server.hs` ~line 5368) —
`parsedValues :: Maybe ParsedTxCBOR`, and
`ParsedTxCBOR.certificates :: [W.Certificate]`
(`lib/api/src/Cardano/Wallet/Api/Http/Server/Handlers/TxCBOR.hs:97`).
It is thrown away only when certificates are later mapped to the
API-facing `ApiAnyCertificate` type
(`lib/api/src/Cardano/Wallet/Api/Types/Certificate.hs:280`:
`W.CertificateOfDelegation _ delCert -> toApiDelCert ...` — the `Maybe
Coin` is discarded with `_`). `reclaimIfAny`, being a `where`-bound
value in the same function, can reference `parsedValues` directly
without any signature change.

## Functional requirements

- FR1: `reclaimIfAny` sums the `Just coin` refund amounts from every
  `W.CertificateOfDelegation (Just coin) (W.CertDelegateNone _)`
  certificate in `parsedValues`'s `certificates` list, instead of the
  totalIn/totalOut heuristic. Registration and registration+delegation
  certificates with `Just coin` must contribute `0`.
- FR2: When `parsedValues` is `Nothing` (no CBOR stored), or a legacy-era
  deregistration is represented as `CertificateOfDelegation Nothing
  (CertDelegateNone _)`, `reclaimIfAny = 0`. A missing exact amount must
  never trigger a guess — the whole point of this fix is eliminating false
  positives, and a false negative (silently under-reporting when the exact
  amount is absent) is less harmful than mislabeling an ordinary payment.
- FR3: `depositIfAny` / `depositTaken` (the outgoing-tx deposit-taken
  heuristic) is UNCHANGED — out of scope, issue is about
  `depositReturned` only.
- FR4: The pure certificate-summing logic is extracted into a small,
  independently unit-testable top-level function (`mkApiTransaction` /
  `Handler` itself is not practically unit-testable — needs a DB/wallet
  layer harness this ticket doesn't build).
- FR5: Preserve the existing era parser, primitive `Certificate` type,
  API-facing certificate mapping, JSON codecs, and HTTP schema. This is a
  consumer-side calculation only: Conway and Dijkstra explicit refunds are
  consumed when present; older eras and missing CBOR remain compatible by
  contributing `0` rather than consulting current protocol parameters.

## Success criteria (from the issue's own repro table)

Given a certificate-summing function `depositReturnedFromCertificates
:: Maybe [W.Certificate] -> Natural`:
- Case A (ordinary incoming tx, gap 1,500,000 ≤ 2,000,000 deposit,
  no deregistration certificates): `Nothing` or `Just []` → `0`, with no
  input/output/direction arithmetic available to reintroduce the heuristic.
  **This is the bug this ticket fixes.**
- Case B (genuine stake-key deregistration refund, one
  `CertificateOfDelegation (Just 2_000_000) (CertDelegateNone _)`):
  → exactly `2_000_000`.
- Registration negative control: a
  `CertificateOfDelegation (Just 2_000_000) (CertVoteAndDelegate ...)`
  contributes `0`, even though it carries a coin.
- Legacy/missing-amount negative control:
  `CertificateOfDelegation Nothing (CertDelegateNone _)` contributes `0`.
- Pool, governance, and ordinary delegation certificates contribute `0`.
- Multiple explicit deregistration refunds sum exactly while interleaved
  non-refund certificates contribute nothing.
