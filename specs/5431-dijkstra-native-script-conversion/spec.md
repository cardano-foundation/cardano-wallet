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

Any Dijkstra transaction carrying a native script therefore crashes the reader.

## User-visible outcome

A wallet observing a Dijkstra transaction that carries a native script reports
that script over the API — as the policy script of a mint or burn, and in the
witness count — instead of crashing.

## Why one function and not two era arms

Both sites hand their result to a field of type `Script KeyHash`, so both need
the same conversion. `Convert.toWalletScript` already contains it. Its body
matches only pattern synonyms that are era-polymorphic over `NativeScript era`
(`RequireSignature`, `RequireAllOf`, `RequireAnyOf`, `RequireMOf` from
`ShelleyEraScript`; `RequireTimeStart`, `RequireTimeExpire` from
`AllegraEraScript`), and `DijkstraEra` has both instances. Only the signature's
`NativeScript era ~ Timelock era` equality excludes Dijkstra.

The change is therefore a widening of one existing function, not a new era arm,
and the eras whose `NativeScript` already *is* `Timelock` keep compiling
unchanged.

## The case that makes this more than a widening

`Timelock` has six shapes and `toWalletScript` ends in a catch-all that is
genuinely unreachable for it. Dijkstra's native script has a seventh:
`RequireGuard`, carrying a guard credential, with its own CBOR tag and its own
evaluation rule — a guard is satisfied by membership in the transaction's guard
set, not by a witness.

Widening the signature without deciding `RequireGuard` would move an
unreachable branch into a live crash path while removing the two counted stubs.

`Script` from `cardano-addresses` has no constructor that can hold it, and no
lossy mapping is available: a guard credential may be a script hash rather than
a key hash, and there is no guard key role to label the key-hash form with.
Making the conversion total therefore requires a case on `AnyScript` and
`AnyExplicitScript`, both of which are serialized into the public REST API.

**That decision is open, and it determines the conversion's result type.**
Implementation does not start before it is settled.

## Requirements

- R1 — Both `error` stubs are gone, by deletion. A widened catch-all, a message
  that stops naming the era, or a test made pending in another form does not
  satisfy this.
- R2 — One conversion serves both call sites; neither carries its own copy.
- R3 — A Dijkstra native script built from the shapes shared with Timelock
  converts to the same wallet script its Conway counterpart converts to.
- R4 — A Dijkstra guard script has an explicit, total outcome. An `error`
  reachable from an observed transaction is not one.
- R5 — The eras already served by the conversion keep their present behaviour.
- R6 — The count reported by `scripts/ci/dijkstra-stub-gate.sh` on the tree
  falls by two from the base, and the declared ratchet moves with it in the same
  tree.

## Rejection behaviour

R4's outcome is whatever the open decision settles. Until then this section is
deliberately unwritten rather than guessed.

## Observable success

- The census script on the tree reports a total two lower than the base's, and
  a ratchet equal to it.
- The Dijkstra conversion is exercised by a test whose input is a real Dijkstra
  native script. A test that only shows Conway still works has not exercised it.
- Where the exercise is a property, its generated population is shown to contain
  a Dijkstra native script, including a guard script. A generator that cannot
  produce the feature is blind to it at any sample size.
