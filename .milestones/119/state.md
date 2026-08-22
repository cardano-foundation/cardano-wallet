State — Updated: 2026-08-22
Legend: ✅ done · 🟡 active/next · ⏳ queued · 👤 external review · ⏸ paused · ⛔ blocked · ❓ unknown

```mermaid
flowchart LR
  R["✅ Release v2026-08-21 SHIPPED<br/>(all 4 badge fixes aboard)"] --> Z["outcome audit"]
  subgraph s["S tickets — 3/4"]
    S3["✅ #5103 #5196 #5246"] --> S4["⛔ #5115 parked"]
  end
  subgraph burn["burn-down 6/27 — need 8 more"]
    Q1["🟡 #5326 next"] --> Q2["⏳ #5334 #5146 #5108 #5094"] --> Q3["⏳ #5252, test-perf cluster"]
  end
  subgraph auto["post-release automation"]
    A1["🟡 #5387/PR #5384 manual merge-back<br/>(stale-payload snag)"] --> A2["🟡 #5385/PR #5386 automation<br/>(spec-first, draft)"]
  end
  X["⛔ #5381 cardano-addresses 4.0.8<br/>blocked on crypto-class ecosystem bump<br/>(relaxation disproven empirically)"] --> Z
  D["⛔ decisions: #5330 · #5115 · #5370 · #5384 retrigger"] --> Z
  s --> Z
  burn --> Z
  auto --> Z
```

- **Release v2026-08-21 shipped 2026-08-22** — the badge-green drive's product
  outcome. cardano-addresses stayed 4.0.2 by ruling; the requested upstream
  bound relaxation was then **empirically disproven** (memory→ram coupling,
  7 type errors at crypton 1.0.6) — #5381 now waits on a
  cardano-crypto-class ecosystem bump.
- New tech debt admitted to the milestone: #5385 (auto release merge-back).
- Desk ACTIVE; one operator-driven lane running (#5385/#5384 window); all
  desk-dispatched lanes archived.

GitHub milestone #113 "Drop cardano-api" is separate and out of scope.
