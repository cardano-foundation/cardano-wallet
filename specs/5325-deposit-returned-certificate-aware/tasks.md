# Tasks: Make `depositReturned` certificate-aware

## Slice A — replace the heuristic, add a directly-testable pure function

- [ ] T5325-S1 Add a pure `Maybe [W.Certificate] -> Natural` function
      summing only explicit `CertDelegateNone` deregistration refunds
- [ ] T5325-S2 `reclaimIfAny` calls it with `parsedValues`'s
      certificates instead of the totalIn/totalOut heuristic;
      `depositIfAny` untouched
- [ ] T5325-S3 Unit tests in `ServerSpec.hs` covering: missing/zero certs
      (Case A — the false positive this fixes), one exact refund (Case B),
      a registration carrying `Just coin` (must be zero), legacy
      deregistration with `Nothing` (must be zero), unrelated certs, and
      multiple explicit deregistration refunds summing exactly
- [ ] T5325-S6 Preserve primitive era readers, `Certificate`, API mapping,
      JSON codecs, and schema; verify the change is consumer-only
- [ ] T5325-S4 `./gate.sh` passes (build, focused unit tests, fmt,
      hlint)
- [ ] T5325-S5 Commit: `fix: make depositReturned certificate-aware instead of a numeric heuristic` with `Tasks: T5325-S1, T5325-S2, T5325-S3, T5325-S4, T5325-S6` trailer
