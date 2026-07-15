# Research: Bound Unit Test Memory

## Decision 1: Treat the list-monad property as the root cause

The failing test is
`prop_applyNM_iterate @[] @Int` in
`lib/unit/test/unit/Control/Monad/UtilSpec.hs`.

Its inputs combine an arbitrary non-negative iteration count with an arbitrary
list-valued generated function. Repeated list bind multiplies the output width
at every iteration. `withMaxSuccess 10` limits the number of QuickCheck cases,
but it does not bound either iteration depth or branching width.

Evidence:

- The complete suite with Hspec forced to `-j1`, RTS `-M2G -N4`, exited 251
  after 13:28. Maximum residency was 2,087,833,440 bytes and peak RSS was
  3,505,408 KiB. Sequential execution therefore reduces concurrency but does
  not fix the defect.
- A bounded `-hT` profile with `-M1G -j1` exited 251 after 11:26. The last
  complete heap sample contained 994,653,312 bytes of `GHC.Types.:` list cons
  cells. Two seconds earlier that category held 532,751,616 bytes, identifying
  a single rapidly expanding list rather than gradual retention of completed
  spec results.
- The focused property with `--seed=1 -j1 +RTS -M256M -N1` exited 251 in 2.28
  seconds with 259,256,280 bytes maximum residency and zero completed examples.
- The neighboring fixed-depth list property with the same seed and cap passed
  in 0.84 seconds with 73,016,944 bytes maximum residency.

The focused seed-1 command is the RED regression proof.

### Chosen test-data shape

Keep list-monad coverage, but generate a function whose result has at most two
elements and choose iteration counts from 0 through 5. This still exercises
empty, single-result, and branching list behavior while limiting the largest
result to 32 elements per path.

### Alternatives considered

- **Force Hspec `-j1`**: rejected by the complete sequential run above.
- **Raise the heap ceiling**: rejected because it hides an exponentially sized
  generated case and preserves the host-level failure risk.
- **Delete the list-monad property**: rejected because `applyNM` is used with
  branching generators and list semantics are meaningful coverage.
- **Only reduce QuickCheck success count**: rejected because the existing count
  of 10 already reproduces the OOM; input size, not case count, is the problem.

## Decision 2: Preserve four-way test execution

The implementation keeps the Hspec parallel hook and the `-N4` runtime
default. The successful fix must pass the monolithic suite under `-M2G -N4`
without relying on `-j1`.

## Decision 3: Restore both intended RTS defaults in six suites

Six Cabal test stanzas repeat `-with-rtsopts`. GHC retains only the last value,
so compiled executables currently report only `-N4`; the intended `-M2G` safety
ceiling is absent.

Each stanza will use one quoted option:

```text
"-with-rtsopts=-M2G -N4"
```

The proof is inspection of each compiled test executable's `+RTS --info`
output after rebuilding it.

## Decision 4: Keep the reported assertion failure separate

The issue also reports an `UnexpectedPrefix` assertion. No diagnostic evidence
links it to the list explosion. It remains visible as an independent test
failure and is outside this PR's scope.
