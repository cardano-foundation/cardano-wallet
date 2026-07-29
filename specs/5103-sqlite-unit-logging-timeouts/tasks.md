# Tasks — #5103 SQLite unit-test diagnostics

## Slice 1 — observable SQLite test execution

- [ ] T510301 Add negative controls for timeout diagnostics and SQLite
  progress events.
- [ ] T510302 Implement the Hspec SQLite progress formatter and diagnostic
  timeout helper.
- [ ] T510303 Apply 60-second state-reporting timeouts to blocking Delete and
  Migration SQLite examples.
- [ ] T510304 Prove the helper tests, target SQLite modules, and full gate are
  green.
- [ ] T510305 Land one signed bisect-safe commit with the required task
  trailer.
