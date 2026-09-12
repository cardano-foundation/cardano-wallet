# Dijkstra native script conversion — spec

Child of #5209 (items 20 and 21 of the partition). Issue #5431.

## Problem

Two readers in `lib/primitive` decode the scripts carried by an observed
transaction. Their Dijkstra arms are `error` stubs sharing one literal,
`error "TODO: DijkstraNativeScript conversion"`:

| module | function |
|---|---|
| `Cardano/Wallet/Primitive/Ledger/Read/Tx/Features/Mint.hs` | `fromLedgerScriptToAnyScriptDijkstra` |
| `Cardano/Wallet/Primitive/Ledger/Read/Tx/Features/Scripts.hs` | `dijkstraAnyExplicitScript` |

Any Dijkstra transaction carrying a native script therefore crashes the reader,
including the six script shapes the wallet has always been able to represent.

## User-visible outcome

A wallet observing a Dijkstra transaction whose native scripts use the shapes
shared with earlier eras reports those scripts over the API — as the policy
script of a mint or burn, and in the witness count — instead of crashing.

A Dijkstra *guard* script still fails, loudly and by name. That is a smaller
outcome than "Dijkstra native scripts work", and it is the honest one: see
"Guard scripts" below.

## Why one function and not two era arms

Both sites hand their result to a field of type `Script KeyHash`, so both need
the same conversion. `Convert.toWalletScript` already contains it. Its body
matches only pattern synonyms that are era-polymorphic over `NativeScript era`
(`RequireSignature`, `RequireAllOf`, `RequireAnyOf`, `RequireMOf` from
`ShelleyEraScript`; `RequireTimeStart`, `RequireTimeExpire` from
`AllegraEraScript`), and `DijkstraEra` has both instances. Only the signature's
`NativeScript era ~ Timelock era` equality excludes Dijkstra.

The change is therefore a widening of one existing function, not a new era arm,
and the eras whose `NativeScript` already *is* `Timelock` keep compiling and
behaving unchanged.

## Guard scripts

`Timelock` has six shapes and `toWalletScript` ends in a catch-all that is
genuinely unreachable for it. Dijkstra's native script has a seventh,
`RequireGuard`, carrying a guard credential, with its own CBOR tag and its own
evaluation rule — a guard is satisfied by membership in the transaction's guard
set, not by a witness.

`Script` from `cardano-addresses` has no constructor that can hold it, and no
lossy mapping is available: a guard credential may be a script hash rather than
a key hash, and there is no guard key role to label the key-hash form with.
Mapping a guard to `RequireSignatureOf` would misreport the script to API
consumers and to the witness counter, which is worse than crashing because it
is silent.

**So the guard case remains an explicit failure naming the era.** Making the
conversion total would need a case on `AnyScript` and `AnyExplicitScript`, both
of which are serialized into the public REST API; that is a separate decision
about a published interface and is not taken here.

## Requirements

- R1 — Both `error` stubs at the two call sites are gone, by deletion. A
  widened catch-all, a message that stops naming the era, or a test made
  pending in another form does not satisfy this.
- R2 — One conversion serves both call sites; neither carries its own copy.
- R3 — A Dijkstra native script built from the six shapes shared with `Timelock`
  converts to the same wallet script its Conway counterpart converts to.
- R4 — A Dijkstra guard script fails, and the failure names the era so the
  census counts it. It is one failure, in the shared conversion, not one per
  call site.
- R5 — The eras already served by the conversion keep their present behaviour.
- R6 — The count reported by `scripts/ci/dijkstra-stub-gate.sh` on the tree
  falls by **one** from the merge base, and the declared ratchet moves by one
  in the same tree.

## Rejection behaviour

A guard script reaching the conversion raises an error whose message names the
Dijkstra era and says that the script has no wallet representation. It does not
claim to know which future shape it caught: the conversion is polymorphic over
eras, so its unmatched branch is not evidence that the value was a guard.

## Observable success

- The census script on the tree reports a total one lower than the merge base's,
  and a declared ratchet equal to it.
- The conversion is exercised by a test whose input is a real Dijkstra native
  script. A test that only shows Conway still works has not exercised it.
- The property over the shared shapes is shown to generate Dijkstra native
  scripts, including nested ones, rather than being satisfiable by a population
  that never contains the feature.
- A Dijkstra guard script is shown to be constructible and to reach the failure.
