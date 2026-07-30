# Tasks — #5063 published delta packages

## Slice A — published-delta

- [ ] T5063-S1 delete `lib/delta-types`, `lib/delta-store`, `lib/delta-chain`, `lib/delta-table`
- [ ] T5063-S1 remove four `packages:` lines and four `package delta-*` stanzas from `cabal.project`
- [ ] T5063-S1 add `constraints: delta-types ==1.0.0.0, delta-store ==1.0.0.0` and `allow-newer: delta-store:io-classes`
- [ ] T5063-S1 bound `delta-types` / `delta-store` in consumer `.cabal` files (`cardano-wallet`, `cardano-wallet-primitive`, `cardano-wallet-unit`)
- [ ] T5063-S1 rename all unit `import Test.Store` → `import Test.Data.Store`
- [ ] T5063-S1 drop four names from `nix/project-package-list.nix`
- [ ] T5063-S1 drop `unit-delta-*` and windows delta test entries from `flake.nix`
- [ ] T5063-S1 drop `delta-*:unit` from `justfile` unit list
- [ ] T5063-S1 drop delta unit matrix entries from `.github/workflows/{ci,windows,macos-unit-tests}.yml`
- [ ] T5063-S1 prove RED (build/import failure or gate absence checks fail on pre-GREEN tree)
- [ ] T5063-S1 prove GREEN: freeze/plan shows Hackage 1.0.0.0; no local path leakage; focused Store unit tests pass
- [ ] T5063-S1 prove instruments can fail (seeded local path / package stanza detected by gate greps)
- [ ] T5063-S1 run `./gate.sh` (runtime copy) and commit with subject `chore: use published delta-types and delta-store` + `Tasks: T5063-S1`
