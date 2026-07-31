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

## Slice B — negative-control-proven reseed assertion (B1)

Production reseed on PR head is accepted as sound. The suite still does
not prove reseed: `prop_rnd_prologue_roundtrip` normalizes `gen` away.

- [ ] T5303-S6 Add a permanent assertion that reseed happens on load
      (e.g. two loads of the same on-disk checkpoint yield different
      `gen`, and/or loaded `gen` differs from persisted `gen`), without
      weakening non-`gen` structural roundtrip coverage
- [ ] T5303-S7 Negative control: show the new assertion fails when
      reseed is reverted or made constant (record commands + exit codes);
      restore production reseed after the control
- [ ] T5303-S8 Focused unit proof green for RndState prologue / reseed
      properties; structural non-`gen` coverage retained
- [ ] T5303-S9 Commit with Tasks trailer covering T5303-S6, T5303-S7,
      T5303-S8 (and any gate-adjacent notes)
