# Tasks: Reseed Byron RndState.gen from CSPRNG on checkpoint load

## Slice A — reseed on load + adjust the roundtrip test

- [X] T5303-S1 Add a local `secureSeed`-style CSPRNG helper (or reuse an
      equivalent) in `Store.hs`, matching the `genSalt`-based pattern
      from `Server.hs` (#5300 / PR #5307)
- [X] T5303-S2 `loadPrologue` for `RndState` builds `gen` from that
      fresh seed via `liftIO`, instead of the DB column value; DB
      column/codec and `insertPrologue` unchanged
- [X] T5303-S3 Update the RndState roundtrip test in `StoreSpec.hs` so
      it no longer asserts `gen` survives load byte-identical, while
      still proving every other field round-trips
- [X] T5303-S4 `./gate.sh` passes (build, focused unit tests, fmt,
      hlint)
- [X] T5303-S5 Commit: `fix: reseed Byron RndState.gen from CSPRNG on checkpoint load` with `Tasks: T5303-S1, T5303-S2, T5303-S3, T5303-S4` trailer
