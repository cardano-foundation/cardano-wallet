# Tasks — #5063 published delta packages

## Slice A — published-delta

- [x] T001 Specify the Hackage-only dependency decision, compatibility risks,
  owned files, and acceptance mapping
- [x] T002 Delete `lib/delta-types`, `lib/delta-store`, `lib/delta-chain`, and
  `lib/delta-table`, and remove their local `cabal.project` entries and stanzas
- [x] T003 Constrain `delta-types` and `delta-store` to 1.0.0.0, add the narrow
  `delta-store:io-classes` relaxation, and bound the three wallet consumers
- [x] T004 Remove the four packages' nix, flake, justfile, workflow, and Attic
  cache wiring; prove `delta-chain` and `delta-table` have no consumers
- [x] T005 Rename eleven `Test.Store` imports to `Test.Data.Store`, apply only
  formatter-required changes, and prove no test body or assertion changed
- [x] T006 Compare the reviewed Hackage archives with the vendored base and
  document every executable or export-surface difference relevant to wallet
- [x] T007 Account for deleted package tests versus retained wallet consumer
  coverage; prove the Store selection runs 39 examples with zero failures
- [x] T008 Prove Hackage plan resolution, positive and negative absence
  controls, the release artifact, commit signatures, and a conflict-free
  rebase onto current master before starting exact-head CI

## Evidence snapshot

- Hackage archives:
  `delta-types sha256:0fdb3b6572487c6a4ec407cb7c09b4688343b8979168c846f23d0890cbbb0bed`;
  `delta-store sha256:d8048e15430e6e6eb667fd4d935ce7f17907f3449ce3935e5272d06fc8c504a8`
- Plan: both packages resolve as non-local version `1.0.0.0`
- Focused consumer suite: 39 examples, 0 failures
- Release gate: `nix build .#ci.artifacts.linux64.release` succeeded
- Test diff: import rename and Fourmolu ordering only
- Rebase target: master `ac390d1ae4d207e2e1d22d9623dec9f536e937b5`
- Exact-head CI remains the external merge-handoff gate; historical Conway
  stake-pool timeouts are diagnosed as unrelated but are not silently waived
