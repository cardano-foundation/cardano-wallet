State — Updated: 2026-08-19
Legend: ✅ done · 🟡 active/next · ⏳ queued · 👤 external review · ⏸ paused · ⛔ blocked · ❓ unknown

```mermaid
flowchart LR
  subgraph badges["CI-green amendment — MET ✅"]
    B1["✅ #5373/#5374 Windows EOF"] --> B2["✅ #5375/#5376 benchmarks + gate"]
    B2 --> B3["✅ #5377/#5378 env-var case"]
    B3 --> B4["✅ #5379/#5380 Mithril sync 34m"]
  end
  subgraph s["S tickets — 3/4"]
    S1["✅ #5103"] --> S2["✅ #5196"] --> S3["✅ #5246"] --> S4["⛔ #5115 parked"]
  end
  subgraph backlog["27-issue burn-down — 6/27, need 14"]
    Q1["🟡 #5326 shutdown drain — next"] --> Q2["⏳ #5334, #5146, #5108, #5094"]
    Q2 --> Q3["⏳ #5252 (spec drafted), test-perf cluster"]
  end
  D1["⛔ #5330 alerting choice"] --> Z["outcome audit"]
  D2["👤 #5370 external PR — CI approval + review"] --> Z
  badges --> Z
  s --> Z
  backlog --> Z
```

- **Badges: all 9 green** at master `6761259ee9` — the 08-05 amendment is met.
  Mithril Sync (0% pass rate for a month, silent ~13h replay fallback +
  broken timeout cleanup) fixed and verified on a real 34m20s run.
- **Honest recount 2026-08-19**: the numeric burn-down is at **6/27** (the
  badge drive closed newly-filed issues, which don't count toward the
  starting set). 8 more closures needed. Queue paved — see ledger.
- #5341 closed as completed-by-#5380 (it was the pre-existing Mithril
  timeout ticket).
- Desk ACTIVE, no lanes in flight, no open PRs owned by this desk.

GitHub milestone #113 "Drop cardano-api" is separate and out of scope.
