# Tasks: Align wallet protocol-parameter size fields with ledger types

## Slice A — align the primitive domain and preserve boundaries

- [ ] T5252-S1 Prove and align maximum value size to `Word32` and primitive
      minimum collateral percentage to `Word16`; retain maximum collateral
      input count as `Word16`
- [ ] T5252-S2 Prove recent-era ledger-to-wallet conversion preserves all
      three values exactly and removes their obsolete conversion shims
- [ ] T5252-S3 Preserve safe transaction-size behavior with widening only at
      explicit downstream consumer boundaries
- [ ] T5252-S4 Run the `ApiNetworkParameters` JSON proof and keep its golden
      file and observable numeric representation unchanged
- [ ] T5252-S5 Prove the target fields are not persisted; leave SQLite schema
      and migration files unchanged
- [ ] T5252-S6 Pass the frozen focused/type/search gate plus relevant builds,
      primitive and wallet tests, format check, and HLint
- [ ] T5252-S7 Produce final commit
      `refactor: align protocol parameter size fields with ledger types` with
      `Tasks: T5252-S1, T5252-S2, T5252-S3, T5252-S4, T5252-S5, T5252-S6, T5252-S7`
