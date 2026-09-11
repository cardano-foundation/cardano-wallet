# Tasks

## S1 — close the five Dijkstra converter stubs

- [x] T001 — properties covering the requirements of `spec.md`, written and
      observed failing because the Dijkstra behaviour is absent, with the
      failure attributable to that absence rather than to setup.
- [x] T002 — coverage assertions inside those properties showing the generated
      population actually contains the Dijkstra case.
- [x] T003 — Dijkstra output and UTxO conversion available from
      `…Ledger.Convert`, stated once over the era.
- [x] T004 — the Praos block-producer accessor stated once, and `Pools.hs`
      handling the Dijkstra block through it.
- [x] T005 — the Dijkstra arms of the five sites, each delegating rather than
      converting inline.
- [x] T006 — the local-cluster spec's Dijkstra output case.
- [x] T007 — `DIJKSTRA_STUB_MAX` lowered by the measured census delta, with the
      before-and-after runs of the census script recorded as the evidence.
