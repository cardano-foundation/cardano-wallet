# Plan

## Constraints

- Branch is cut from `refactor/5422-delete-era-split` and the pull request
  targets that branch. A further slice stacks on this one, so the head moves
  deliberately and is pushed early.
- Local `nix build` is unavailable in this lane. `nix develop -c cabal` is the
  sanctioned build path; the gate uses it.
- The census script is the only admissible source of the stub count. Its
  positive and negative controls run before the count is believed.

## What the tree already provides

`cardano-ledger-dijkstra` (pinned at the version in `cabal.project`) defines
`type TxOut DijkstraEra = BabbageTxOut DijkstraEra`. The Babbage output
constructor, the address conversion and the value conversion the Conway path
uses are all available at the Dijkstra era without new representation work.

The Praos block-producer accessor in
`Cardano/Wallet/Primitive/Ledger/Shelley.hs` is already
`ShelleyBlock (Praos StandardCrypto) era -> PoolId`, and the Babbage and Conway
copies of it are byte-identical. A Dijkstra block is accepted by it unchanged.

## Strategy

Prefer one definition over a new per-era copy wherever every caller of the
definitions being replaced is inside this ticket's fence. Where that is not
true — the output converters are also re-implemented in two `Read/Tx/Features`
modules and once more as local helpers — leave the wider family alone. That
consolidation is a separate piece of work with its own fence; doing half of it
here would leave two names for one idea.

The block-producer duplicates are the case where consolidation *is* in fence:
their only callers are in `Pools.hs`, which is the site being changed.

## Slices

One bisect-safe slice. The five sites share one subject — the shape of a
Dijkstra output and a Dijkstra block — and the acceptance criterion is a single
census delta that cannot be split without leaving an intermediate commit whose
ratchet does not match its count.

**S1 — close the five Dijkstra converter stubs.**

Proof first: properties that fail because the Dijkstra behaviour is absent, run
and observed failing before any production change. Then the production change.
Then the ratchet, lowered by the measured delta in the same commit.

## Live boundary

None of the five paths is reached by the unit suites through a live node. The
block-producer path in `Pools.hs` observes real chain blocks in production, and
the test proof for it is therefore a property over constructed Praos headers,
not evidence about a running node. That limit is stated rather than papered
over: the property proves the accessor agrees across eras on the same header,
which is what the change alters, and nothing about node connectivity.

## Verification

The frozen gate runs, in this order: format check, hlint over `lib`, the
census script with its controls and with ratchet-tightening made fatal,
`cabal build` of the touched packages with tests enabled, and the focused
suites covering the changed converters. Cheap and static legs run before
expensive ones.

The census is run against the merge base as well as against this tree, so the
delta is a measured difference between two runs of one instrument rather than a
single number compared against a remembered one.
