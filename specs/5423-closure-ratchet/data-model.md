# 5423 — Data model

## Entities

### Package
Read out of the tree, never hard-listed (INV-14).

| field | source | validation |
|---|---|---|
| `name` | the `name:` field of a `lib/*/*.cabal` file | non-empty; unique across the population |
| `cabal_path` | the file it was read from | exists and is readable |
| `stanzas` | see below | at least one |

The population is every `lib/*/*.cabal` in the tree root. Its size is reported.
A population of zero, or one smaller than the number of `.cabal` files found, is
an instrument failure, not a zero count.

### Stanza

| field | values | note |
|---|---|---|
| `kind` | `LIB`, `ANY`, `COMMON` | `library` and `library <name>` are both `LIB`; `test-suite`, `benchmark`, `executable`, `foreign-library` are `ANY`; `common <name>` is `COMMON` and carries no membership of its own |
| `imports` | names of `COMMON` stanzas | a stanza's inherited `build-depends` take the **importing** stanza's kind (INV-13) |
| `deps` | package names from `build-depends` | version constraints stripped; `pkg:sublib` and `pkg:{a,b}` normalised to `pkg`; comparison is whole-name equality (INV-12) |

`build-tool-depends` is **not** a `deps` source. A tool dependency is recorded
and reported separately so its exclusion is visible rather than silent.

Dependencies inside `if`/`else` blocks are counted unconditionally. A ratchet
that under-counts a conditional edge would go green on a dependency the build
takes on some platform; over-approximating is the conservative direction.

## Derived sets

Let `T` = `cardano-api`.

- `L` (`closure-lib`) — least set with `T ∈ L` and `P ∈ L` whenever some `LIB`
  stanza of `P` has a dep in `L`. The reported row is `|L ∩ Packages|`, so `T`
  itself never counts.
- `A` (`closure-any`) — `(L ∩ Packages)` ∪ `{ P : some stanza of P has a dep in L }`.
  `A` takes exactly one arbitrary-stanza hop and then only library hops:
  depending on a package does not build that package's test-suite, so a further
  `ANY` hop would over-count.
- `S` (`suppressions`) — occurrences, not files: lines matching `^{-# OPTIONS_GHC`
  in `*.hs`, plus lines matching an anchored `ghc-options:` in `*.cabal` and
  `cabal.project*`, that name `-Wno-deprecations` or `-fno-warn-deprecations`.

## State invariants

- `L ∩ Packages ⊆ A`. Any perturbation raising `closure-lib` also raises
  `closure-any`; the converse is false, and INV-3 is proved by a perturbation
  that raises `closure-any` alone.
- Computation of `L` is a fixpoint over a finite population, so it terminates on
  self-edges and cycles (INV-11). `cardano-wallet-read` is a live self-edge.
- Every counter is a non-negative integer parsed from the gate's own stdout by
  the negative control; an unparseable count is a control failure, never a pass.

## Ratchet

| row | env override | land-time value |
|---|---|---|
| `closure-lib` | `CARDANO_API_CLOSURE_LIB_MAX` | measured at land time |
| `closure-any` | `CARDANO_API_CLOSURE_ANY_MAX` | measured at land time |
| `suppressions` | `CARDANO_API_SUPPRESSIONS_MAX` | measured at land time |

Planning-time measurement at `6b42c36b58`: 12 / 13 / 9. Recorded as the value to
**reproduce or supersede**, never to copy.
