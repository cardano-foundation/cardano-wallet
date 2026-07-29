# Tasks: Make `depositReturned` certificate-aware

## Slice A — replace the heuristic, add a directly-testable pure function

- [ ] T5325-S1 Add a pure `Maybe [W.Certificate] -> Natural` function
      summing `CertificateOfDelegation (Just coin) _` refunds
- [ ] T5325-S2 `reclaimIfAny` calls it with `parsedValues`'s
      certificates instead of the totalIn/totalOut heuristic;
      `depositIfAny` untouched
- [ ] T5325-S3 Unit tests in `ServerSpec.hs` covering: zero certs (Case
      A — the false positive this fixes), one refund-bearing cert
      (Case B), a non-refund cert, multiple refund certs summing
- [ ] T5325-S4 `./gate.sh` passes (build, focused unit tests, fmt,
      hlint)
- [ ] T5325-S5 Commit: `fix: make depositReturned certificate-aware instead of a numeric heuristic` with `Tasks: T5325-S1, T5325-S2, T5325-S3, T5325-S4` trailer
