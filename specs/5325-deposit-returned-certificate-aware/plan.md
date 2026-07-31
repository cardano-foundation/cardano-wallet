# Plan: Make `depositReturned` certificate-aware

One slice. Confined to `lib/api/src/Cardano/Wallet/Api/Http/Shelley/Server.hs`
plus a new/extended unit test file.

## Slice A — replace the heuristic, add a directly-testable pure function

1. In `Server.hs`, add a top-level pure function (exported or not, but
   placed so it's importable from a test module):
   ```haskell
   depositReturnedFromCertificates :: Maybe [W.Certificate] -> Natural
   depositReturnedFromCertificates = maybe 0 (sum . mapMaybe refund)
     where
       refund
         (W.CertificateOfDelegation (Just c) (W.CertDelegateNone _)) =
           Just (fromIntegral (unCoin c))
       refund _ = Nothing
   ```
   (Exact naming/shape is the driver's call — the behavior in spec.md
   FR1/FR2 is the contract, not this literal snippet.)
2. Change `reclaimIfAny` in `mkApiTransaction`'s `where` clause to call
   this function with `certificates <$> parsedValues` (or equivalent),
   dropping the `totalIn`/`totalOut`/`direction` arithmetic entirely.
3. Leave `depositIfAny`/`depositTaken` untouched.
4. Unit test: extend `lib/unit/test/unit/Cardano/Wallet/Api/ServerSpec.hs`
   with direct tests of `depositReturnedFromCertificates` (or whatever
   it's named) covering every case in spec.md's Success Criteria, including
   the load-bearing negative control where a registration certificate has
   `Just coin` but must contribute `0`. This does NOT require standing up
   `mkApiTransaction`'s full Handler harness (DB layer, wallet layer, time
   interpreter); it tests the extracted pure function directly with
   hand-built `[W.Certificate]` lists.
5. Verify the wiring remains consumer-only: `reclaimIfAny` reads
   `ParsedTxCBOR.certificates`; no primitive era reader, certificate type,
   API-facing mapping, JSON codec, or schema file changes.

Owned files:
- `lib/api/src/Cardano/Wallet/Api/Http/Shelley/Server.hs`
- `lib/unit/test/unit/Cardano/Wallet/Api/ServerSpec.hs`

Forbidden scope: `depositIfAny`/`depositTaken`, `Certificate.hs`
(`mkApiAnyCertificate` mapping — do not change what's exposed over the
API, only what feeds `reclaimIfAny`), the CBOR-parsing/ledger-read
modules (`Certificates.hs` in `lib/primitive` — already correct, this
is a consumer-side fix only), any `.cabal` file.
