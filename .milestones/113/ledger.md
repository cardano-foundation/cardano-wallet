# M1 — Drop cardano-api (GitHub milestone #113) — ledger

Home repo: `cardano-foundation/cardano-wallet`
Milestone owner: pane `%122`, window `wallet:6 ms1-drop-cardano-api`
Runtime root: `/tmp/projects/cardano-wallet/ms1-drop-cardano-api/`
Parent: Cardano Wallet **project owner**, pane `%107`, `0-projects:cardano-wallet`,
root `/tmp/projects/cardano-wallet/owner/`
Family/model: `claude` / `claude-opus-5[1m]`
Seated: 2026-08-28T11:25Z under `/tmp/machine/pausa/RELEASE-2026-08-28T1120Z-cardano-wallet.md`

**Supersedes `.milestones/1/`** — a 2026-07-29/30 desk that carried the `ms1`
name but acted at project altitude (all-milestone priority table, repo-wide
backlog drive, serial merge queue). That role is now the project owner's. Read
`.milestones/1/` as history only; its runtime roots (`/tmp/ms-cw-1/`), lanes and
priority table are dead.

---

## Outcome and its observable test

> **No production library under `lib/` depends on `cardano-api`, verified in the
> dependency closure, with transaction bytes and database compatibility
> preserved.**

**The test is the instrument, not a burn-down — and it is a CONJUNCTION of rows,
not one row.** Corrected 2026-08-28 under `A-004`: row 1 covers only the
**production** library closure (4 of the 9 `lib/*.cabal` packages that list
`cardano-api`), so **row 1 = 0 is reachable with five packages still listing it**
and #5290 unsatisfied. The criterion is:

> **row 1 = 0**, AND **row 2's allowlist has no `M1`, no `MIXED` and no
> `UNKNOWN` entries** -- a residual of `PERMANENT` and `FOREIGN` entries each
> with an established cause, **not zero** -- AND **row 3's allowlist reduced to
> its documented test-only residual**, each entry naming its follow-up.

**No single row is the criterion, and only row 1's target is zero.** Corrected a
second time on 2026-08-28 under `A-005`: the earlier claim that row 2 = 0 was
also wrong, and for the same reason. Closing six issues is not the
outcome. A grep over imports is not the test — #5290's own AC says *closure*.

Verified 2026-08-28 against `/code/cardano-wallet@346786a112` (read-only):
production **library** stanzas declaring `cardano-api` in `build-depends` are
`lib/wallet`, `lib/api`, `lib/primitive` (library stanza, line 51 — its
test stanza at 239 is a separate population), `lib/network-layer`, plus the
`cardano-api-extra` package itself. Test/bench/integration consumers
(`lib/unit`, `lib/benchmarks`, `lib/integration`, `lib/local-cluster`) are a
different population and the row must demonstrate excluding them.

**Take no number from this ledger into a gate.** Every probe applied to this
surface has revised it upward. The instrument measures at land time and shows
the measurement that produced its own MAX.

## Milestone artifact

**NONE — this is an open blocker and it is mine.** M1 ships no forkable
executable of its own; its outcome is a *subtraction* from the dependency
closure, which is visible in the build, not in a binary a stranger can obtain.
Whether that excuses M1 from the artifact rule, or whether the closure ratchet's
public CI result *is* the obtainable artifact, is a project-level question.
Recorded as parked decision P-3 rather than silently omitted.

## Units

| unit | GH | state | owner | note |
|---|---|---|---|---|
| epic | #5237 | OPEN, unassigned | none yet | parent epic |
| epic | #5243 | OPEN, unassigned | none yet | transaction-layer remaining work |
| ticket | #5288 | OPEN, unassigned | none yet | script-witness support in `Transaction.Ledger`. "blocks #5285" is historical — #5285 is CLOSED/COMPLETED |
| ticket | #5289 | OPEN, unassigned | none yet | rewrite Shelley signing without cardano-api. "follows #5285" historical |
| ticket | #5241 | **RULED close as `not planned`** — awaiting operator execution | project owner routed it | outward action; this desk does not close it and does not comment on it |
| ticket | #5290 | OPEN, unassigned | none yet | final sweep. **Fence 2 interlock lives here** |
| instrument | not filed | **project review PASSED; at the operator gate** | milestone owner | 3-row closure ratchet; `TICKET-DRAFT-closure-instrument.md` |
| PR | **#5413** | draft, stacked on `chore/issue-5397-node-11-1-0`, base SHA in first commit | ticket owner | populations measured on the branch: 7 pre-existing + 5 bump-widened = 12, matching #5399's declared `MAX 7 -> 12` from the other side |
| ticket | **#5411** | **LANE LIVE** — `wallet:7`, `%127`, `START` 13:28:59Z | ticket owner `claude` | deprecations → ledger. Operator-cleared 18:40Z. Spec: `TICKET-DRAFT-M1-deprecations-to-ledger-v2.md` |
| ticket | **#5412** | FILED, queued | none yet | `U-5` — the `lib/integration` suppression, fenced out of #5411 per #5290's own policy |

## Priority order, with reasons

1. **Closure instrument (row 1)** — durable acceptance criterion; no external
   dependency; the only unit that can consume a lane today.
2. **Deprecations ticket** — time-boxed by #5399 merging, but its *next* step is
   a review queue (project → operator), not a lane. Runs concurrently on a
   different resource. See D-2.
3. **#5241 ruling** — cheap, removes a unit, unblocks nothing but the close.
4. #5243 / #5288 / #5289 — after the instrument exists, so their effect is
   observable.
5. **#5290 last** — fence 2; cannot land without a ruling naming M6's state.

## Decisions taken (D)

- **D-1 · #5241 → CLOSE `not planned`. RULED by `%107` 2026-08-28T11:55Z.**
  Verified premise:
  `lib/primitive/lib/Cardano/Wallet/Primitive/Types/Tx/SealedTx.hs:120` already
  carries `unsafeReadTx :: EraValue Read.Tx`; the module imports
  `Cardano.Ledger.*` / `Cardano.Wallet.Read` and **zero `Cardano.Api`**. #5236
  did that work. What remains is deleting two cached fields (`valid`,
  `serialisedTx`) around a payload that is already the target type — bearing on
  the closure criterion **nil**.
  **Execution is outward and routed to the operator by `%107`. This desk does
  not close #5241 and does not comment on it.**
  *What the ruling releases:* the deprecations front was held partly on the
  premise *"M1 has already decided how to model that boundary per #5241"* — a
  premise with no decision behind it. The decision now exists and it is *"there
  is nothing to decide."* The deprecations ticket's fence — **introduce no new
  type to model the transaction-body boundary** — is unchanged and still binding.

- **D-2 · sequencing = concurrent, not ordered.** Endorsed by `%107`. The two
  candidates consume different resources: parent/operator attention vs a lane.

- **D-3 · no ticket grows a PR before review.** Standing gate: draft → project
  review → operator review → branch and lane. Binds the deprecations ticket
  **and** the instrument ticket. Both are drafted and held, neither is filed.

- **D-4 · #5290 vs M6 #5209 sequencing is CLOSED; neither desk reopens it.**
  M6 implements the five `Cardano/Api/Extra.hs` stubs; M1 deletes them later.
  Four of five sit on live production call paths (`Cardano/Wallet.hs:2251`,
  `:2202`, `:2950`; `Shelley/Transaction.hs:360`, `:362`, `:804`, `:828`;
  `Server.hs:3177`, `:3650`, `:3872`); only `cardanoEraFromRecentEra` is
  test-only. Sequencing M6 behind M1 would ship a wallet that throws on Dijkstra.

- **D-5 · RULED (A-001): the PR that changes the count changes MAX, in the same
  PR.** Mechanises M6 `NOTE-011` `RULING 1` condition 4. **Row 1 and row 2 both
  land on `master`** — no stack layer added, and the stack is already three deep.
  Row 2 `MAX` = the count measured at land time; #5399 raises it to `7 + N` in
  its own diff; the deprecations ticket lowers it in its own diff. Green at every
  step, nothing ever unobserved. The instrument-first rule held; my proposal to
  invert it for row 2 was unnecessary once this third option was visible.

- **D-6 · the all-open-PR-heads precondition is adopted for BOTH rows** — green
  on `master` and on the head of every open PR, re-enumerated at land time.
  Amended by **FINDING-002** with one clause: a PR that itself causes the rise
  and has a named owner for its raise is expected red until it carries that
  raise.

- **D-7 · no harness unification with #5407.** Ruled by `%107`: two ratchets that
  work beat one unification that must be right the first time, and #5407 is
  DRAFT under an operator hold — coupling M1's durable criterion to a held
  artifact is the wrong trade. Revisit only with a real precedent to point at.

- **D-8 · the `cardano-api-extra` package is test-only and out of row 1**, as a
  **demonstrated** exclusion. Accepted by `%107`: an exclusion that cannot be
  shown working cannot be defended at review.

- **D-9 · the same-class red-proof is BINDING, not aspirational.** The transitive
  `build-depends` injection with no source import is constructible, so *"I could
  not construct one"* is **not available** to the lane. Accepted by `%107` as
  closing a hole in the original condition.

- **D-10 · RULED (A-002): my landing clause is adopted, with three conditions**
  that turn the carve-out into an owed unit rather than a hole. A PR is exempt
  from the all-open-PR-heads precondition only if (1) it has declared its raise
  — number, direction, causing diff; (2) the declared number has been
  **independently confirmed against that branch by the desk landing the
  instrument**, not accepted from the PR body; (3) the raise has a named owner
  and a named trigger. For #5399: (1) satisfied (body line 100, `MAX 7 -> 12`,
  `N = 5`); (3) owner **M6 desk**, trigger **row 2 landed on `master`**, action
  **edit the MAX file at #5399's next `master` sync**; (2) satisfied on the
  master-side base by `REPORT-003`. M6's body-only declaration is accepted
  because this desk **re-measures rather than trusts the body** — declared and
  trusted are different things, and only the second needs a mechanism.
  M6 reached the same structural diagnosis independently, 15 minutes apart.

- **D-11 · RULED (A-003): condition 2 restated, because as written it was
  unexecutable.** It required the desk landing the instrument to confirm the
  declared number *against the other branch* — satisfiable only by crossing the
  fence that forbids reading M6's tree. Restated: the **base** of the delta is
  confirmed with controls by the desk landing the instrument; the **branch-side
  count** by the branch's own desk; the **project owner holds both**, and
  neither desk reads the other's tree. Satisfied today in exactly that form —
  base `0a7332482c` = 7 (this desk, 2 pos + 2 neg controls, blob-compared
  against `346786a112`), branch `33ccafc` = 12 (M6). **`N = 5`, `MAX 7 -> 12`
  CONFIRMED, not provisional.** Third clause caught by a child today; `%107`
  recorded the pattern as its own rather than smoothing it over.

- **D-12 · RULED (NOTE-002): the precondition gets a SECOND carve-out.** It is
  that **row N's own check** is green on `master` and on every open PR head —
  **not that those PRs are green.** A pre-existing unrelated failure is noted
  and does not block; only a red *caused by the instrument under test* blocks.
  Verified independently by this desk rather than accepted: #5407 has one commit
  (`docs: plan the Dijkstra stub census ratchet`), rollup 57 SUCCESS / 1
  FAILURE, the failure being `Conway Integration Tests` completed
  2026-08-27T09:14:35Z, a check that is SUCCESS today on #5399, #5402 and
  #5364. A docs-only diff cannot cause SQLite contention.
  **Neither carve-out narrows the rows themselves** — a carve-out governs what
  blocks landing, never what is counted.

- **D-13 · the precondition cannot be read from anyone's CI.** Row N's check
  does not exist on those branches — the same structural cause as `FINDING-002`.
  It is executed by **computing row N locally over each fetched head** and
  tabling the per-head numbers in the PR body. The open-PR list is
  **re-enumerated at land time**, never inherited.

- **D-14 · RULED (A-004) + CHOSEN (REPORT-004): a THIRD ROW, ratcheting a
  NAMED SET rather than a count.** `%107` found that this desk's headline claim
  exceeded row 1's evidence — #5290's own criterion is *"no `lib/*.cabal`
  package lists `cardano-api` in `build-depends`"*, all of `lib/`, and row 1
  covers 4 of 9. Mechanism is mine and I chose a third row on the same
  instrument: a separate ticket costs a lane and a review cycle for a one-line
  measurement and delays the criterion gating the close; a column on row 1 mixes
  two populations and breaks this ticket's own class separation.

  **Not the proposed count-based shape.** Executed at `0a7332482c`: removing
  `cardano-api` from `lib/benchmarks` and adding it to `lib/numeric` leaves the
  **count unchanged at 9** — a count-based row is GREEN while `cardano-api`
  migrates out of test packages into production ones. Row 3 therefore compares
  an **observed set** against a checked-in **allowlist** (each entry carrying a
  one-line reason) and is RED on a difference in **either** direction; the
  "allowlisted but no longer observed" direction is its fall, under the same
  same-PR rule. Its final state — only documented test-only packages, each
  naming its follow-up — is **#5290's documentation criterion made mechanical
  rather than promised**.

  **Rows 1 and 2 stay counts, deliberately.** At a target of 0 the count *is*
  the set. Row 2 additionally has its raise declared numerically across the
  fence (`MAX 7 -> 12`, `N = 5`, held by `%107`), and reshaping it would reopen
  a settled cross-milestone item to buy a property worth far less at 0.

- **D-15 · project rulings recorded so they are not re-litigated at review:**
  `lib/local-cluster` is **non-production** (dev/test harness) — out of row 1,
  in row 3. The **module** `lib/wallet/src/Cardano/Api/Extra.hs` is fence 2 and
  in scope via `lib/wallet`; the **package** `lib/cardano-api-extra/` is out of
  row 1 and **on row 3's allowlist**.

- **D-16 · PROJECT-WIDE RULE, derived here and bound by `%107` beyond this
  milestone:** *at a target of 0 a count is a set -- nothing can be substituted
  into a set that must be empty; at any nonzero target a count admits
  substitution silently, and the row must ratchet the named set.*

- **D-17 · RULED (A-005): ROW 2 IS A NAMED SET TOO, and my exemption of it was
  wrong.** I derived D-16 from evidence and then exempted row 2 on an inherited
  premise -- that its contents were all about `cardano-api` -- which I had
  never read. Verified in my own tree at `0a7332482c`: **four of seven have
  documented non-`cardano-api` causes** (faucet = `cardano-addresses` Byron,
  **permanent by design and it says so**; `Logging.hs` and `ApiSpec.hs` = Wai
  `requestBody`; `Sqlite.hs` = a bare `ADP-2841` TODO, a **pointer, not a
  cause**) and **three carry no reason at all** -- the Shelley transaction
  modules. **The repository can explain four of its seven suppressions.**

  Row 2 therefore ratchets `path | spelling | cause | disposition`.
  `spelling` is required so an alias swap is visible in the diff. `cause` must
  be **established by observation** -- remove the pragma, build under the
  sanctioned path, record what warns. `disposition` in
  `{M1, PERMANENT, FOREIGN, UNKNOWN}`, and **`UNKNOWN` is not an accepted
  terminal state**: a residual that is named but not understood is the same
  defect one layer up.

  **#5399 is refined, not reopened** -- an allowlist of 7 entries has
  cardinality 7, so `MAX 7 -> 12`, `N = 5` stands; it names its five files
  rather than asserting `+5`. My "reshaping row 2 reopens a settled
  cross-milestone item" argument is **WITHDRAWN**; I should have tested it
  before using it as a reason. The gain: the owed unit *"remove the deprecation
  suppressions"* today answers **"which ones?"** only by re-deriving a diff.

- **D-18 · procedural correction adopted: an entry I have COUNTED is not an
  entry I have READ.** Row 2's specification now requires reading as a
  precondition of listing. Cheap to fix here because nothing is filed.

- **D-19 · RULED (A-006): `OBSOLETE` added**, and its justification is
  **exhaustiveness, not likelihood** — a disposition set must cover every
  outcome of the procedure that assigns it. The procedure has three outcomes
  and the set had two; that is a defect whether or not the third ever occurs.
  `OBSOLETE` is the only outcome that **reduces the set for free** — a dead
  line is deleted, no migration, no upstream. Without it a lane finding no
  warning writes `PERMANENT` with an invented cause, and **a documented dead
  suppression is worse than an undocumented one, because it now has a paper
  trail arguing for its own retention.**

- **D-20 · MY OWN PROCEDURE WAS WRONG; the fix makes `OBSOLETE` crisper.**
  I specified *"build and record what actually warns"* — but a sanctioned path
  enforces `-Werror`, so removing a live suppression **fails the build** rather
  than warning, and a lane told to record a warning finds none where it was
  told to look. Restated: **build fails -> the failing diagnostic is the cause;
  build succeeds -> `OBSOLETE`.** `OBSOLETE` is an **exit code, not a text
  search** — absence-of-a-string is the evidence class this project keeps being
  burned by.

- **D-21 · PROPOSED, not ruled: a sixth disposition `MIXED`.** `OPTIONS_GHC` is
  **module-scoped**, so one pragma covers every deprecation in the module from
  every source. At `0a7332482c`: `Shelley/Transaction.hs` 70 imports / 4
  `Cardano.Api` / namespaces `Ouroboros`+`Control`+`Data`;
  `Unsigned.hs` 55 / 1 / plus `Cryptography`. **The failure prevented is
  concrete:** the deprecations ticket's definition of done is *"no
  `disposition: M1` remain"*, so a file misclassified `M1` sends it to delete a
  pragma still load-bearing for a foreign deprecation — a broken build under
  `-Werror`, found late, in the ticket meant to retire suppressions safely.
  M1 discharges `MIXED` by **narrowing the `cause` field and reclassifying
  `FOREIGN`**, not by deleting the line.
  **Row 2 target becomes: no `M1`, no `MIXED`, no `UNKNOWN`.**

- **D-22 · STATIC FINDING, no build required:** `lib/wallet/src/Cardano/DB/Sqlite.hs`
  imports `Cardano.Api` **zero times**, so that entry **cannot be
  `disposition: M1`** — the import list settles it. With its bare `ADP-2841`
  pointer it is the strongest `OBSOLETE` candidate, and the ticket says **run it
  first**: a disposition procedure that has never produced `OBSOLETE` is one
  nobody has shown can reach that outcome. Establishing it once on the cheapest
  candidate is the **positive control for the classification step itself**.

- **D-23 · RULED (A-007): `MIXED` stays a distinct value.** Not taxonomy — the
  two carry **different obligations**. A `FOREIGN` entry imposes nothing on M1;
  a `MIXED` entry imposes real M1 work. **Collapse them and M1's remaining
  obligation becomes unobservable** — nothing distinguishes *"M1 has work here"*
  from *"M1 never had work here"*, which is this instrument's own defect class
  reproduced in its metadata. And at classification time a `MIXED` entry **is
  not foreign**: it has a live `cardano-api` cause, so `FOREIGN` is a false
  statement about the present tense made to simplify a table.

- **D-24 · RULED: the classifier proves itself in BOTH directions.** No
  `OBSOLETE` is admissible from a run that has not **also** produced at least
  one build failure on the same tree and path. Costs nothing — every
  `M1`/`FOREIGN`/`PERMANENT`/`MIXED` entry *is* a demonstrated failure.
  **If the procedure succeeds on every entry, the test is broken, not the
  suppressions.** The set carries its own controls both ways: `Sqlite.hs` for
  reachability of `OBSOLETE`, the three Shelley modules for detection of a live
  deprecation. Prove-it-red applied to the classifier rather than the gate.

- **D-25 · the load-bearing assumption is ENFORCED, not recorded** — my one
  change to `A-007`, ruling `%107`'s if they prefer recorded-only.
  **A recorded assumption is a check that cannot fail**: read once at review by
  someone who already knows it, never again by whoever adds
  `-Wwarn=deprecations` later, while every disposition's evidence goes void
  silently and the row keeps reporting green. Row 2 already runs on every PR,
  so it asserts its own premise — RED if a deprecations demotion appears on the
  sanctioned path config, with its own same-class red-proof. One grep.

  **Premise verified at `0a7332482c` with a control on the search:** the only
  `-Wwarn`/`-Wno-error` demotion in the whole tree is
  `cabal.project:258  ghc-options: -Wwarn=unused-packages`; deprecations
  demotions **0**; the pattern *does* find the known hit, so the zero is a real
  absence. Sanctioned mechanisms unchanged (`nix/haskell.nix:277-278`,
  `justfile:51,61`). **`-Wwarn=deprecations` is a one-line addition to the
  stanza directly beneath the existing demotion.**

- **D-26 · RULED (A-008): the assumption is ENFORCED.** `%107` adopted my
  argument over its own: *"a recorded assumption is a check that cannot fail."*
  Row 2 asserts its own premise.

- **D-27 · the deprecations ticket is ADOPTED as M1's own** and re-issued as
  `handoffs/TICKET-DRAFT-M1-deprecations-to-ledger-v2.md`. `%107` authored v1
  before this desk existed, so with author and project reviewer being the same
  desk **the review step was a check that cannot fail** — the defect removed
  from the instrument all afternoon, sitting in the process that governs it.
  Verified at `origin/mastera7332482c`: the round-trip at
  `Cardano/Wallet.hs:2249` **confirmed**; all five converter symbols
  **confirmed to exist**; v1's tuple-bind rendering **corrected** — it is a
  **record pattern on `Cardano.TxBodyContent`**. The *"25 sites"* figure is
  **deliberately not re-derived** (another branch, another instrument, another
  unit); the *"11.0 had 3 pragmas, 11.5 has 5"* claim is **unverified and
  flagged** — it is about upstream releases, not this tree.

- **D-28 · DEFECT in v1 that v1 could not see about itself:** its cited
  evidence, `glm-bodycontent-attempt.patch`, lives at
  `/tmp/projects/cardano-wallet/ms6-dijkstra/e-node-11-1-0/w-txbody-migration/handoffs/`
  — **inside another milestone's runtime root, in `/tmp`.** It makes an M1
  ticket depend on a fenced lane, and **`/tmp` dies with the host**, so the
  pointer 404s exactly when someone is resurrecting the work. v2 does not cite
  it. Preserving it needs **relaying by `%107`** into a durable location — a
  request, not an artifact this desk takes from a sibling root.

- **D-29 · RULED (A-008): P-5 closed — the window keeps `ms1-drop-cardano-api`.**
  It matches `ms6-dijkstra`, names the milestone rather than a ticket, and the
  machine owner's inventory already carries it under that name.

- **D-30 · RULED (A-009): v2 PASSED project review with two gaps, both closed.**

  **Gap 1 — the un-stack trigger missed the orphaning event.** #5399 is
  **rebased before it merges** (M6's own owed unit, triggered by #5402 merging,
  which happens **first**), so this branch's base is rewritten one event before
  the old trigger fires. Only `%107` could see this: it holds both desks' units
  and neither desk could see it alone.

  **Gap 2 — "transaction bytes are preserved" could not fail at these sites.**
  Verified at `0a7332482c` before accepting:
  `buildCoinSelectionForTransaction :: ... -> Write.Tx era -> CoinSelection`,
  body containing no `ByteString`, no CBOR, no encode. Inherited from v1 without
  checking. Replaced by a **differential test** — the values obtained from the
  ledger are identical to those previously obtained through `cardano-api`, same
  inputs, old path against new. Same class, and stronger than a byte check.

- **D-31 · my addition: the un-stack unit is discharged by OBSERVATION, with the
  relay as redundancy.** `%107` is carrying a notification obligation to M6, but
  **a relay nobody sends is a check that cannot fail.** The same event is
  readable from a public remote ref — a read, not contact:
  record the base SHA at branch time, then
  `git merge-base --is-ancestor <base> origin/chore/issue-5397-node-11-1-0`;
  false means the base was rewritten. Verified computable (#5399 head
  `33ccafc2`). The lane records its base SHA in the PR body so the check has an
  input.

- **D-32 · two open items CLOSED BY DELETION, not by work.** The `3 -> 5`
  upstream-pragma claim is **not load-bearing** — it explained the two
  populations rather than evidencing them, and the populations are directly
  observable (bump-widened = does not warn on `master`, does warn on the stack).
  The prior-attempt patch is **not relayed and not preserved**: its one lesson
  is already durable in the out-of-scope fence and the seat rule.
  **Not everything that dies with `/tmp` needs saving — only what a decision
  still depends on.**

- **D-33 · the migration is smaller than it looks.**
  `buildCoinSelectionForTransaction` already takes `Write.Tx era`, so
  `toCardanoApiTx` converts a **ledger** tx to `cardano-api`, destructures it,
  and converts each field back. **The input is already the right type** — a
  converter deletion, not a representation change, which is why the no-new-type
  fence is affordable.

- **D-34 · the orphan check has FOUR outcomes, not three** (`A-010` proposed
  three; running it found the fourth). `ls-remote --exit-code` returns **0**
  present / **2** absent / **128** transport error, verified against the real
  remote. **The fourth is the dangerous one:** a network blip read as
  "merged-or-deleted" triggers an un-stack against an event that never happened
  — distinct causes mapped onto one accepted outcome.
  **Rule recorded, generalisable beyond this check: only a cleanly
  distinguishable result is a verdict; everything else is "no verdict — retry",
  never a default to either answer.** It also absorbs the case where the
  recorded base object is absent locally and `--is-ancestor` errors instead of
  answering. Exhaustiveness applied to a procedure with outcomes rather than
  dispositions — same shape as `OBSOLETE`, and found the same way: by running it.

- **D-35 · RULED PROJECT-WIDE (A-011): only a cleanly distinguishable result is
  a verdict.** Binding on every check, gate, ratchet and probe. `%107` accepted
  that its own third outcome silently manufactured a verdict — *a wrong verdict
  is worse than no verdict*. Recorded as the registry contract
  **`no-verdict-status`**, with **both** independent derivations named: M6's
  census exits `2` on instrument failure; **M1's orphan check exits `75`
  (`EX_TEMPFAIL`)** on transport error. **The two numbers differ and that is
  CORRECT, not drift** — see the encoding rule below. Two desks reaching the
  same shape independently is why it is one contract, not two.

  **The shared thing is the CONCEPT, not the number** — `%107` first ruled `2` as
  the shared status and **withdrew it** after this desk found the collision:
  `ls-remote --exit-code` already uses `2` for *ref absent*, which M1's check
  maps to **exit 1 (trigger fired)**. A shared number would have put *"the ref is
  gone"* and *"I have no verdict"* on one integer, one layer apart.
  M1 uses **`75` = `EX_TEMPFAIL`** (*"temp failure; user is invited to retry"*) —
  the standard code for exactly this status, not a spare number.
  **Cited durably as `sysexits.h`, `EX_TEMPFAIL = 75`**, present in both glibc
  and musl. A nix store path was cited first and withdrawn: **a store path is
  version- and hash-specific, so it is an evidence pointer that expires** — the
  same class as citing a file inside a `/tmp` runtime root.
  **A probe's status is never passed through as a script's status, and never
  choose an encoding that already means something else in the layer you
  consume.**

- **D-36 · the finding underneath the findings, recorded because it is
  actionable and not a compliment:** every rule this project gained on
  2026-08-28 came from **executing** a procedure; every defect came from
  **reasoning about** one — the count-vs-set swap, the `OBSOLETE` exit-code
  correction, and the fourth outcome, against the `A-001` cycle, the literal
  precondition, the unsatisfiable condition 2, the untestable byte criterion,
  and the three-outcome check. `%107` has bound itself to walking a procedure
  before shipping it. **This desk binds the same: no procedure leaves this desk
  in a brief or a criterion until it has been run at least once.**

- **D-37 · RULED (A-012): `c2` re-homes to M6 (#118)** as an **observed**
  contract, not new work. **Retained in this registry until M6 records it**;
  `%107` says when to remove it. Context worth keeping: `%107` retired M2 on
  2026-08-28 preserving its outcome and the #5381 inheritance but **not its
  contracts**, and `c2` survived only because a July desk wrote it down and this
  desk carried it into a different milestone's registry precisely so a
  transition would not lose it. When M6 closes it re-homes again rather than
  lapsing — `%107` owns that step, trigger *"M6 accepted"*.

- **D-38 · RULED: `m1-tx-bytes` gets a TRIGGER, and is not commissioned yet.**
  Neither ticket at the operator gate touches a byte-producing path — v2
  established that `buildCoinSelectionForTransaction` serialises nothing — so an
  instrument commissioned now **would have no red-proof available**. Trigger:
  **the first M1 ticket whose scope touches a path that constructs or serialises
  a transaction**, commissioned in that ticket with a same-class red-proof.
  **Owner: the M1 milestone owner.** #5288 or #5289 on today's map.
  *A candidate with no trigger is the thing this project keeps losing.*

- **D-39 · open item 3 CLOSED: YES.** `Cardano/Wallet.hs` is among #5399's five
  and arrives `disposition: M1`; its cause is the legacy `TxBodyContent`
  build-and-destructure — **the deprecations ticket's exact site**. Verified at
  `origin/mastera7332482c`: all five paths exist and **none carries a
  suppression there**, so the whole of `N = 5` is **bump-widened** and disjoint
  from the seven pre-existing files. `7 + 5 = 12` is two independent
  observations, not a sum — established without reading #5399's branch.

- **D-40 · CONFLICT 1 decided: entry 2 is NOT the deprecations ticket's.** No
  narrow exception. #5290's integration policy prescribes the remedy (*"stop and
  open a separate ticket"*), so an exception would override an inherited rule
  for convenience; it is substantively different work (legacy-Alonzo fixture
  deserialisation, not a four-field round-trip); and *an owed removal inside a
  fence that forbids it is an obligation guaranteed to be discovered as a
  contradiction by whoever tries to discharge it.* Registered as **U-5**.

- **D-41 · CONFLICT 2 decided: row 2 gains `retires-with`, ASSERTED not carried.**
  Format `path | spelling | cause | disposition | retires-with`. **Two levels:**
  the **row's** final state is no `disposition: M1` entries at all; a **ticket's**
  definition of done is no `M1` entries with `retires-with: <that ticket>`. So
  the deprecations ticket is done with entries 1, 3 and 4 and is **not blocked by
  `Gen.hs`**, which retires with #5290 behind fence 2.
  **A `retires-with` that nothing checks is a documented assumption** — the shape
  D-25 rejected. Row 2 asserts it: non-empty for every `M1` entry; names an
  **open** issue or `unfiled:<owed-unit-id>` present in this ledger; **named
  issue closed while the entry remains -> RED.** The issue-state query resolves
  to **`75` (no verdict)** on failure. **The network dependency is acceptable
  only because `no-verdict-status` exists.**

- **D-42 · THE COUNTING INSTRUMENT IS SETTLED, and this desk's own number is
  retired.** *Removing the suppression and reading the compiler is a **different
  instrument**, not a more careful grep.* The deprecated surface is dominated by
  **record fields** of `TxBodyContent` and by `createTransactionBody` /
  `getTxBodyContent` / `getTxBody` — invisible to a grep for type names.
  `Gen.hs` alone flags the type, the constructor and **22 distinct record
  fields**. **`10 -> 25 -> 41` was never three people converging**; it was three
  grep-family instruments measuring a category that excluded most of the
  surface. **This desk's 41 was in that family and is retired with the rest.**
  Counting by build on the stacked branch is now the **only** sanctioned method,
  not the preferred one.

- **D-43 · the `retires-with` check reads `stateReason`; a closed issue has TWO
  causes with OPPOSITE remedies** (`NOTE-004`, non-gating, folded in).
  `COMPLETED` -> the work shipped and the entry should have been removed with
  it. `NOT_PLANNED` -> the work was abandoned and the entry must be
  **re-attributed**. Both RED; one message cannot say both, and collapsing them
  is *distinct causes mapped onto one accepted outcome* — the
  verdict-vocabulary defect, inside the check written to enforce attribution.
  **Enum verified, not assumed:** across 60 recently closed issues in this repo
  exactly two values occur — `COMPLETED` (56, e.g. #5387) and `NOT_PLANNED`
  (4, e.g. #5297).

- **D-44 · `unfiled:<id>` resolves against a branch the PR checkout does not
  have.** The ledger lives on the orphan `milestones` branch, so the check must
  fetch it to resolve that half of the rule; feasible —
  `git ls-remote --exit-code origin refs/heads/milestones` exits 0 today.
  Resolution path: `.milestones/113/ledger.md`, **Owed units**. A failed fetch is
  **`75` — no verdict**. Recorded shape: **this check has three external
  dependencies — the issue API, the `milestones` branch, and the working tree —
  and only the third is local. That is acceptable ONLY because each non-local
  one degrades to no-verdict rather than to an answer.**

- **D-45 · RULE (NOTE-005), binding on every brief this desk or its children
  write: A BRIEF THAT NAMES A REPORTING OBLIGATION NAMES THE EXACT MECHANISM
  THAT DISCHARGES IT** — the script, its path, the acknowledgement form.
  *"Report via `STATUS.md`"* is a destination; `status-event` is a mechanism.
  **A destination without a mechanism is an instruction the child cannot follow
  and the parent cannot observe.** Second clause, and the one that matters:
  **when a parent corrects a brief for a missing mechanism, the correction
  propagates to the briefs that child writes** — otherwise it is fixed at one
  edge and reappears at the next.
  *Evidence, twice in six hours one layer apart:* `%107`'s brief said *"escalate
  to `%107`"* and omitted how — a `BLOCKED` sat 27 minutes; **this desk's brief
  said "report via `STATUS.md`" and omitted the script** — a prose `START` and a
  wait that timed out against a journal containing one. Both symptoms were
  indistinguishable from a child that never started.

- **D-46 · STANDING: every seat attests its own identity FROM THE PROCESS, not
  from its brief.** A `START` asserts what is **running**, not what was
  **specified**. The lane did this unprompted (`ps` on `%127`). It matters most
  exactly where today's `glm-5.3` / `glm-5.3-flash` error was headed — into a
  durable `START` attesting a model that was never launched, looking exactly as
  trustworthy as a true one.

- **D-47 · MY THIRD WAIT DEFECT, and the first FALSE POSITIVE.** I armed
  `'  NOTE  '` for a specific acknowledgement; it matched an unrelated
  `ISSUE-FILED` note in the same second and `send-pointer` returned success while
  the note was **not** acknowledged. **A wait that can match something other
  than what it waits for is a check that passes without observing its subject** —
  and the false-positive direction is worse, because a timeout makes you look
  while a spurious pass makes you stop looking.
  **Correction: every wait names its SUBJECT, not just its tag** —
  `'  NOTE  .*NOTE-002'`, never `'  NOTE  '`. Re-armed on the specific form; it
  matched in seconds.

- **D-48 · a FOURTH wait defect, and it was produced BY the remedy for the first
  three: malformed alternation scope.** Armed `'  (NOTE  .*NOTE-003|COMPLETE)  '`;
  the trailing two-space anchor binds to **both** branches, so the `NOTE` branch
  demanded two spaces after `NOTE-003`, which no message has. On the exact line
  that should have matched: correct pattern **2**, armed pattern **0**.
  `COMPLETE` in the alternation is still right — the third terminal case is real
  — but **a rule that makes patterns more complex makes malformed patterns more
  likely**, and that complexity has to be paid for by a check.

- **D-49 · AND THE PREFLIGHT PASSED, ON A PATTERN NOT ARMED.** Preflighted
  `'  NOTE  .*NOTE-00'` (3 matches, recorded); armed
  `'  (NOTE  .*NOTE-003|COMPLETE)  '` (0 matches, ever). A hand-retyped
  near-version was proven able to match, and something else was armed.
  **The preflight's subject was not the thing under test** — the same defect one
  level up, inside the instrument built to catch it. This shape has now appeared
  in a criterion, a gate, a classifier, a citation and a preflight.

  **REFINEMENT, binding: preflight the EXACT string you arm, byte for byte, from
  one shell variable — never a hand-retyped near-version.** And a trailing
  anchor does not compose safely with an alternation by hand: anchor inside each
  branch, or do not anchor the tail at all, since `status-event`'s format already
  anchors the head.

  **Two-step preflight, adopted:** the subject pattern returns **0** (not yet
  written) *and* the same shape with a known-present subject returns **non-zero**
  (the shape can match). That pair is what distinguishes *"not yet written"* from
  *"cannot ever match"* — the distinction this whole family of defects turns on.
  Demonstrated: `NOTE-004` acknowledged in seconds on a wait built that way.

- **D-50 · STANDING, bound downward: an assertion that LEAVES this project — an
  issue body, a PR body, a comment, a receipt — is verified by the desk that
  publishes it, never inherited.** The lane did this unprompted, re-reading
  `TransactionsNew.hs:7135-7150` before filing #5412 rather than relaying a
  description that had come to it through two desks. **Re-deriving costs one
  command; publishing a wrong claim costs a correction in public.**

- **D-51 · the preflight has TWO halves and only one is decidable now.** You
  cannot preflight a future event's id, so the journal `grep -c` returns `0`,
  and `0` means both *not yet written* and *can never match* — the no-verdict
  failure inside the instrument built to catch it.
  **Form (synthetic line, fatal):** *can this pattern ever match?* — decidable
  before the event exists. **State (journal grep, informational):** *has it
  matched yet?* — **a `0` here is NOT a failure**, and treating it as one is what
  pushes people back to widening patterns, which is how this family started.
  Verified with the control that matters: the synthetic test **rejects the exact
  tail-anchored shape that timed out today** and accepts the anchor-inside-branch
  form.

- **D-52 · a compound pattern needs a synthetic line PER BRANCH.** Testing the
  `NOTE` branch says nothing about the `COMPLETE` branch. **The branch most
  likely to be silently dead is the one you added because it is rare** — and
  `COMPLETE` is exactly that: the terminal case added for safety, covering the
  seat that runs out of context.

- **D-53 · the rule stops propagating as prose.** The lane shipped it as
  `t-deprecations-to-ledger/bin/preflight-wait` — form-per-branch fatal, state
  informational, prints the exact string to arm — and its commit-owner and
  auditor briefs **name that path rather than restate the discipline**. It
  applied `NOTE-002` rule 1 (*name the mechanism, not the destination*) back up
  the chain, unprompted, and proved the tool able to fail with both controls.
  **Independently verified from this desk against its own cases:** today's broken
  pattern -> exit 2; corrected form -> exit 0; a deliberately dead `COMPLETE`
  branch that the `NOTE` branch would have hidden -> exit 2.
  *Five briefs restating a discipline drift; one executable every brief names
  cannot.* Third time today a level below bound a rule tighter than it was
  handed down.

- **D-54 · #5411 WIDENED to retire the `Cardano.TxBody` boundary** as well as
  the four-field round-trip. Owned set gains **exactly** the `fromCardanoApiTx`
  import and two call sites in `lib/api/.../Shelley/Server.hs`; **no general
  licence over `lib/api/**`.** Verified in this desk's own tree first:
  `Wallet.hs:3436/:3459` return `Cardano.TxBody (CardanoApiEra era)`;
  `Server.hs:3177/:3650` unwrap immediately; `Extra.hs`'s Conway branch is an
  identity pair. **Deleting the round-trip alone would not retire the pragma**,
  and #5411's own done-condition names that file.

  **The distinction that makes this consistent with ruling the opposite way for
  `lib/integration` four hours earlier: A FENCE I DREW, I MAY ADJUST; A FENCE I
  INHERITED, I MAY NOT.** #5290 states the integration policy *and names the
  remedy*; the `lib/api` boundary was this desk's own owned-files list, written
  before the `TxBody` boundary was known.

  **The sibling option does not reduce coupling** — the bisect rule forces the
  return-type change and its consumers into one PR either way, so a `U-6` would
  touch most of #5411's files, serialised behind it. **The two-line `lib/api`
  edit happens under every option.** And a sibling closes #5411 without
  satisfying its own criterion, manufacturing the `retires-with`-names-a-
  `COMPLETED`-issue red specified in D-43.

  Five constraints bind: `Extra.hs` untouched (fence 2); orphaning checked both
  ways with stop-and-report (it is #5290's to delete); one PR compiling at every
  commit; both signatures and both sites or none; #5411's published body updated
  **and verified by the desk publishing it** (D-50).

  **Flagged upward, not asked:** this widens a document the operator cleared.
  The fence is this desk's, so the ruling is; `%107` may bounce it to the
  operator, and the fallback costs a `U-6` filing rather than rework.

- **D-55 · the fence rule has THREE rows, not two** (`A-018`). (1) **inherited**
  — may not adjust; (2) **self-drawn, never reviewed above** — may adjust
  narrowly, on the record; (3) **self-drawn but parent-endorsed — bring it
  back**, because once a parent reviews and endorses a fence it is no longer only
  yours, and adjusting it silently makes the endorsement worthless.
  `%107` **checked which row this was** rather than assuming: the `lib/api`
  boundary lives in a brief this desk wrote and `%107` never reviewed; the six
  fences it did endorse in the v2 ticket are M6-contact, `Extra.hs`, the
  bootstrap ground, bisect-safety, seat policy and the census guard. **Row 2,
  cleanly.** Had it been in the endorsed ticket, this would have been a Q.

- **D-56 · GENERAL RULE: an option whose end state is a known invariant violation
  is not an option.** It is a decision to break something later, taken now, with
  the breakage **scheduled rather than risked**. The `retires-with` check (D-43)
  would have caught the sibling option's end state — **being caught by your own
  instrument in the design phase is the instrument working before it exists.**

- **D-57 · the operator cleared an OUTCOME, not a file list.** *"Can we finally
  work on removing deprecations?"* — reading nine words as a specification of
  which files the work may touch is over-reading, and the narrow reading delivers
  a ticket that **closes without removing the deprecation it names.** The
  widening **serves** the clearance. `%107` disclosed it to the operator rather
  than escalating it as a question.

- **D-58 · #5411 CLAIMS NO EFFECT ON ROW 1.** The widening removes a converter
  **application** in `Server.hs`; it does **not** remove `lib/api`'s
  `cardano-api` dependency, which has other uses. Effect is **row 2 only**, on
  precisely one entry (`Wallet.hs`). Row 3 unaffected. **A ticket that quietly
  implies a closure effect it does not have is the scope-of-evidence problem
  running the other way** — evidence narrower than the claim.

- **D-59 · this desk's orphaning count was WRONG and the lane corrected it.**
  I counted call sites of *"the two converters"* as **one population** — the
  class-separation error this milestone has chased all day, in my own arithmetic.
  Per converter, after this ticket's removals: `fromCardanoApiTx` retains **2**
  (`Shelley/Transaction.hs:360`, `TransactionLedgerSpec.hs:1555`);
  `toCardanoApiTx` retains **5**. Neither orphans; `Api/Extra.hs` stays untouched
  and #5290 keeps its deletion. The lane's framing — *"conclusion agrees with the
  ruling; the derivation does not"* — is the right way to report a correction
  that does not change an outcome.

- **D-60 · the orphaning check is a GATE LEG, not a reading** — the lane's
  upgrade of this desk's constraint 2. `TransactionLedgerSpec.hs` and
  `TransactionSpec.hs` are **themselves in the definition of done**, so their
  converter uses may change *during* the slice. Re-run after the candidate, and
  **proven able to fail** like every other leg. *A verification run once is a fact
  about a moment; a gate leg is an invariant.*

- **D-61 · #5419 CLAIMED, kept as written; the FOLD into #5413 REJECTED.** The
  ticket carries this milestone's discipline unprompted and its exclusions match
  standing rulings (`Gen.hs` -> #5290, `TransactionsNew.hs` -> #5412). Kept
  **because I agree with it, not because it exists** — which is what makes first
  refusal mean anything. The fold is rejected on a fact that **post-dates** the
  decision: #5413 went 58/58 green on `master` and became **the only non-draft
  PR open in the repository**, so folding would invalidate a green run, change
  what a reviewer was asked to review, and **enlarge the head of the very queue
  whose scarcity justified folding.** #5419 gets its own PR, stacked one level.

- **D-62 · MY OWN READING WAS WRONG AND A LANE PAID FOR IT.** I told the #5411
  lane its INV-3 differential had passed while behaviour regressed, and read that
  as the differential being too narrow. **It was not.** #5413's four commits are
  unchanged across the rebase and CI is 58/58 — **the 54 failures were the
  base.** The lane spent a regression campaign on a failure it did not cause,
  and my framing sent it looking in the right place for the wrong reason.

- **D-63 · RULE BOUND PROJECT-WIDE: a parked desk's open decisions do not stay
  open.** Park with decisions **closed**, or explicitly **deferred with a named
  owner and trigger**. Drawn from two crossings in one park — `U-1` by the
  operator, the sibling scoping by M6. **Both were competent**, which is the
  point: a rule that only fires against careless desks would have caught
  neither. *An open decision is an attractive nuisance, and competent desks are
  exactly the ones that will pick it up.*

- **D-64 · #5418 ruled into M7 (#119)** by `%107` — it narrows a `MonadThrow`
  constraint, which is technical debt, not `cardano-api` removal. **A milestone
  owns work by what makes it necessary.** Third instance of a dependency with no
  home (#5401, #5416, #5418): **not a filing error, a milestone boundary nobody
  drew.**

- **D-65 · the #5411 lane's seat is gone and nothing was lost.** `%127` and
  `%242` were removed in the `wallet` restructure. **154 journal events, 8
  archived worker roots, PR pushed and green.** The correction owed to that lane
  has **no recipient**; it is written into the lane's own root as a closing
  artifact and **is not recorded as delivered.** The pane died and the record did
  not — the concrete vindication of that lane anchoring detached candidates as
  refs *because a detached HEAD is not a ref*, against exactly the event that
  then happened.

- **D-66 · #5419's lanes claimed, re-briefed, and their runtime roots MOVED OUT
  from under M6's tree.** Fresh root under this desk's parent root via
  `init-worker`; the old root **preserved, not moved** (17 events, M6's record of
  the M6-parented run). Reason: **a lane whose durable root sits under another
  milestone's root is one `archive` away from being deleted by a desk that does
  not own it**, and the path asserts the wrong owner to any resurrector — a
  defect that surfaces only at the next park or cleanup, which is exactly when
  nobody is looking. **`%107` confirmed it was not hypothetical: four hours
  earlier it had archived five runtime roots with a bare `mv`.** Had a live
  lane's root been inside one, it would have moved another desk's working
  directory out from under it while it ran, **and the loss would have looked like
  a mystery rather than an action.**

- **D-67 · the three stale brief lines superseded EXPLICITLY, not by omission**,
  because the lane had already read them: parent `%4` -> `%122`; *"no PR, fold
  into #5413"* -> **own PR, stacked one level**; *"do not push, hand me the
  branch"* -> pushes on **my** acceptance, and *"me"* was M6 and is now nobody.
  M6 correctly refused to edit them — **editing a brief is instructing a lane it
  had been told to stop instructing** — and flagged the text instead. It also
  checked rather than assumed that silence was safe: the brief terminates at
  *"hand me the branch"*, so **there is no autonomous path from it to an outward
  action.** That is the difference between abandoning a lane and releasing one.

- **D-68 · both M6 fences carried unchanged BECAUSE I CHECKED THEM AND AGREE.**
  `INV-2` BLOCKING (helpers stay inside the two spec files) and **`44` is a
  ceiling, not a quota** — honest sub-44 repairs are wins reported as **site
  deltas** so the milestone re-baselines deliberately. *A rename that hides a
  stub from the census is not a repair*, which is the breach actually caught
  pre-submission, so the fence is real rather than hypothetical.

- **D-69 · the operative fact on #5419: census 49/15 against a 44/15 BLOCKING
  fence, a candidate, a repair in flight, and NO ACCEPTANCE BEHIND ANY OF IT.**
  The audit is **owed and was never run** — no auditor was ever seated. Auditor
  resolved mechanically to **`grok`**; three distinct families; no `glm` without
  a ruling. Submission budget **untouched** — no `PROOF-COMPLETE` was declared,
  so the breach and its repair cost zero submissions.

- **D-70 · RULING `2026-09-01-wallet-5420-then-dark`** (sha256 `090dfdbb57f7b7f6…`,
  **recomputed at this desk and matching**): #5420 must be **green AND open**,
  then the lane goes dark. **Reaching the condition IS the terminal event.**
  State read here: head `69480c325c`, draft, base
  `refactor/5411-txbody-fields-from-ledger` — stacked one level on #5413 exactly
  as `D-61` ruled — **56 SUCCESS / 1 FAILURE (`Check HLint`) / 1 IN_PROGRESS
  (`Conway Integration Tests`)**. Merging, commenting, requesting review and
  pinging a reviewer are all **excluded**; marking ready is mechanical and
  **required**.

- **D-71 · this desk's dispositions were closed BEFORE the terminal event, not
  at it.** `D-63`'s rule — *park with decisions closed, or deferred with a named
  owner and trigger* — gets its first test here, and the test is whether it binds
  the desk that wrote it. Every open item now carries an owner and a trigger:
  `U-2`, `U-3`, `U-4` (closed as **unobserved**), `U-7` (the deferred audit),
  the unfiled closure instrument, #5414 slice A, #5412, `P-3`, `P-4`.
  **Nothing of mine is open without both.**

- **D-72 · #5413 is APPROVED and CLEAN.** `paweljakubas` approved it; the human
  review that gated this milestone for a week has happened. **Merging is the
  operator's** — outward and irreversible. This desk does not merge and does not
  comment.

- **D-73 · #5420 finished under the `5420-then-dark` ruling** — open, **58/58
  SUCCESS**, head `3ed3ccc98c`. Its terminal event carried **"green-and-open is
  not acceptance"** verbatim, so the disposition survived the lane going dark and
  no later reader can infer acceptance from CI. `U-7` (the owed, never-run audit)
  stands, deferred to `%122`.

- **D-74 · the lane RETARGETED to #5412 rather than requesting a window.** The
  seats were released and idle with their goal finished, and `%107` will not open
  a window while two PRs sit unmerged. Fresh root, fresh brief, fresh `START`;
  the `t5419` root preserved read-only. **#5412 is `base=master` and independent
  of both PRs**, which is precisely why it is the only unblocked work.

- **D-75 · #5412's likely terminus is named IN ITS BRIEF, in advance.** The site
  deserialises a **hard-coded Alonzo-era body**; unlike #5411 there is **no
  ledger transaction to start from**, and the wallet's accessors are recent-era.
  If no ledger-native path exists for a legacy-era body the lane **escalates**
  rather than designing one. **A Q saying "this cannot be retired without X" is a
  complete result** — stated in the brief so the lane is not incentivised to
  manufacture a migration to avoid filing it.

- **D-76 · `INV-3` WAS MIS-SCOPED — ruled now; the regression question is NOT
  decided.** `INV-3`'s subject is *the four values at the migrated site*; the
  behavioural change #5413's review found is **at the consumers**. Verified from
  the diff: the added tests exercise `ledgerFieldValues`,
  `buildCoinSelectionForTransaction`, `mkUnsignedTx`; **zero added lines touch
  either `Server.hs` consumer.**

  **The self-indictment:** the spec says, in this desk's own words, *"`INV-3`
  replaces a byte-preservation criterion, which could not fail at this site."*
  **A criterion that could not fail was replaced with one that could, scoped to
  the same site — the class problem fixed and the scope problem reproduced.**

  **The root error is the widening, not the invariant.** `D-54` widened the owned
  set to the two `Server.hs` consumers. **The scope of the change was widened and
  the scope of the evidence was not.** That is this desk's, not the project
  owner's, whatever it approved.

  **NOT decided:** whether the change is a regression. Populated witnesses may be
  **correct** and the empty-witness path the original bug — remedy would then be
  coverage plus a stated behavioural change, not a revert. **`INV-3` is not
  re-cut before the measurement lands**; re-scoping against an unmeasured
  hypothesis is how a gate widens in the wrong direction.

- **D-77 · GENERALISED: widening a ticket's owned set widens the CLAIM.** Ask in
  the same ruling what evidence covers the new surface, and if the answer is *"the
  existing invariant"*, check whether that invariant's **subject** moved with the
  fence. **Usually it did not.** Two widenings in this milestone, both this
  desk's: `lib/integration` **refused** — cost a sibling ticket; `lib/api`
  **allowed** — cost an invariant. Not an argument against widening; an argument
  for **widening the evidence in the same ruling.**

- **D-78 · THE CONTEXT LEASE ON `%310` WAS NOT AVAILABLE AND THIS DESK RENEWED
  IT.** The sequential lease permits reusing a conversation across adjacent
  slices **only after the predecessor is accepted.** #5420's campaign 2 is
  audited by **nobody** — `U-7`, registered by this desk and deliberately kept
  alive past the lane going dark. **The precondition failed on a fact this desk
  wrote.**

  **Proximate cause is the wording:** the brief said *"reset if your CLI supports
  it"*. `orchestrator-contract` says reset **or respawn when reset support is
  unknown** — not conditional. **A requirement was turned into an option and the
  lane took the option.** Its `context_reset=NO` was honest *and* a declined
  instruction, declined because declining was made available.

  **Ruling: abandon the lease and respawn — NOT run `U-7` to unblock it.** That
  would **use an audit as a means to a scheduling end**; the audit decides
  acceptance of #5420 and does not license a context reuse on a different ticket.

  **Executed with `tmux respawn-pane -k`, preserving the pane ID**, so every
  binding in briefs, `session.md` and armed waits still resolves and no layout
  drift is introduced. **Same runtime root kept deliberately** — the goal did not
  change, the worktree and six journal events are intact, and the defect was the
  **conversation**, not the run. **No `freeze-context-horizon` manifest is needed
  precisely because nothing is retained.**

  The `t5419` root **stays preserved, not archived**, for a positive reason:
  archival is what a parent does **on acceptance**, and #5420 is explicitly not
  accepted. Archiving would assert the thing `U-7` exists to keep open.

  **CORRECTED 2026-09-02: `%326` had NO defect.** It was **designated** in the
  dispatch event and **never given a #5412 mandate** — verified: **zero
  commit-owner roots under `t5412-integration-suppression`.** Its unreset
  conversation and its `-C` into the `t5419` worktree are **unswept residue of a
  completed run that had not been re-dispatched**, not a fault in an active seat.
  **Nothing was defective because nothing was in flight.** The action is
  identical — respawn before dispatch, by `%310` on its own authority, not by
  this desk — but *"a defect was confirmed"* and *"a previous run had not been
  swept"* are different sentences on a record that gets audited later, and only
  the second is true.

- **D-79 · what a respawn costs, recorded because it is easy to over- or
  under-claim.** Lost: the conversation. Kept: the runtime root, the worktree,
  every journal event. **And the lane's prior findings change epistemic status
  rather than vanishing** — it had labelled them *"RUNG-1 … LEADS … they do not
  establish that the fixture bytes decode under the ledger"*, so **a fresh reader
  is the correct consumer for them.** The line most at risk and therefore named
  explicitly in the respawn note: *"THE PREDICTED ESCALATION MAY NOT HOLD, and I
  am reporting that against the brief's own expectation rather than taking the
  easy exit it offered."* **The brief offered an exit; the lane declined it.**

- **D-80 · PROJECT RULE (from this desk's refusal, adopted by `%107`): an audit
  run to unblock is not the artifact of an audit run to decide** — even when the
  finding would be identical. `%107` records that **it offered the shortcut**,
  and that a parent proposing a route through an audit **is exactly the pressure
  the rule protects against.**

- **D-81 · THE FINDING THAT MATTERS MORE THAN THE LEASE VIOLATION: the lane
  offered the remedy an hour before the defect was found, and BOTH parents read
  past it.** Its second event, `07:22:19Z`, called its own carried conversation
  **"CONTAMINATION TO BE MANAGED, not an asset"**, listed concretely what must
  not carry — `gate.sh d15bb209`, the **44/15 census fence** (with the correct
  reason that it is a *Dijkstra-stub* measure, not a *deprecation* measure), and
  #5419's mandate hashes — and closed:

  > *"If `%122` wants a hard guarantee rather than my discipline, respawn `%310`
  > and I will re-absorb from this brief alone."*

  **The child had already distinguished discipline from a guarantee and named the
  act that converts one into the other.** The failure was that neither parent
  took the offer.

  **Why this desk read past it, stated specifically because "be more careful" is
  not actionable: a field was requested in `START`, the answer came back honest,
  and the question was treated as closed. The qualification was in the NEXT
  event, after this desk had stopped reading for that question.**
  **Rule: when a child answers a question you asked, read the events AFTER the
  answer — the qualification travels separately from the field.**

- **D-82 · measuring what an irreversible act destroys, BEFORE the act, is the
  habit.** Before respawning `%326`, `%310` read `/proc/4180266/fd/47` and
  recorded that the codex rollout file (734,611 bytes) persists on disk, naming
  it as `U-7`'s recovery path **without claiming codex can resume from it** —
  the claim it could have made and did not.

- **D-83 · THE MECHANISM SENTENCE IS UNDER TEST AND CONTRADICTED; THE SCOPE
  RULING IS NOT.** `%107` wrote *"`Cardano.Tx body []` … asserts one is absent"*
  as if verified and has withdrawn it: it read `Tx` as a plain data constructor
  when against pinned `cardano-api` 11.5 it is a **bidirectional pattern
  synonym** whose constructor is `makeSignedTransaction`, and `getTxBody` carries
  `Map.elems (tx ^. witsTxL . scriptTxWitsL)` forward as its `[Script]` field.
  **Prediction — not evidence — that the deleted round trip was identity on
  `scriptTxWitsL`.** Measurement running. **Not relayed as evidence, not acted on
  as settled.**
  This desk's own carriage, accounted precisely: **not propagated as a finding**
  (confined to *"What I have NOT decided"*), but **repeated as an unmarked
  presupposition** inside that section. Corrected.

- **D-84 · SECOND, INDEPENDENT SCOPE DEFECT IN `INV-3`, VERIFIED HERE ON BOTH
  HALVES** — `%107` verified the symbol on `master` and explicitly did not verify
  the PR-branch call site, so this desk did.
  `noScriptWitnesses` (`Unsigned.hs`, `master`) is **all four fields empty**, its
  own haddock calling it *"the no scripts default"*. On the PR branch,
  `TransactionLedgerSpec.hs:1805`, `genDifferentialCase` builds
  `tx0 = buildNewParityTx … noScriptWitnesses ctx`, and **both `INV-3`
  properties draw from it** (`forAllBlind genDifferentialCase`, `:1855`,
  `:1927`). **Every transaction in `INV-3`'s population has an empty
  `scriptTxWitsL` by construction.**

  **Sharper than "the generator was narrow": the same module already had the
  capability** — `:1421` `swStakingScript`, `:1445`/`:1460` `swMintingSources`,
  `:1466` `swReferenceScript`. **A generator choice, not a missing capability.**

  **The second defect subsumes the first:** even had `INV-3` run `balanceTx` and
  covered both consumers, **it still could not have observed a script-witness
  difference. Reach was never the binding constraint — POPULATION was.**

- **D-85 · GENERALISED: a differential's POPULATION is part of its SCOPE.**
  Widening an invariant's reach does not widen the population it draws from, and
  **a generator that hard-codes a feature's absence makes the invariant blind to
  that feature at any reach.** Operationally: **an invariant over a feature must
  be shown to FAIL when that feature differs** — which requires the population to
  contain it. **Prove-it-red applied to the generator rather than to the
  assertion.**

- **D-86 · CLOSURE RATCHET COMMISSIONED and drafted.** Two counters in one gate,
  built from the Dijkstra census gate's proven shape. **`MAX` measured here on
  `803e376bae`: closure 9, suppressions 9.**
  **The project owner quoted 12 for suppressions from a pre-merge tree**, and
  requirement 4 — *"a ratchet that lands with slack is not done"* — **caught the
  person who wrote it.** Inheriting 12 would have shipped a gate unable to go red
  on three retirements, on day one, by construction. The standing rule
  *"re-measure, inherit nothing, including from this brief"* is the only reason it
  will not be born broken.

- **D-87 · anchored-by-form beats restricted-by-population.** The project owner
  required `^{-# OPTIONS_GHC` over `*.hs` only; the accepted refinement adds an
  anchored `ghc-options:` match over `*.cabal` and `cabal.project*`. Both scopes
  return **9** today — evidence, not preference — and the difference is future
  blindness, since a `.hs`-only gate cannot see a `.cabal`-level suppression that
  a 2026-08-28 positive control proved catchable.

  > **Anchoring excludes prose by form. Restricting to `.hs` excludes it by
  > discarding a population.**

  Third time the population rule has bitten this week, this time on the
  requirement rather than the instrument. **Naive count is 16, seven prose, five
  of them our own spec files** — the trap grew as a consequence of the operator
  ruling that specs live in the tree, and the anchor makes that price zero.

- **D-88 · base `master`, not stacked on #5407.** *Independence beats reuse when
  the thing you would reuse sits behind someone else's blocked PR.* Cost: two
  short shell scripts duplicated, consolidated later.
  **Not inconsistent with M6 being told to stack ITS child on #5407** — #5407 is
  M6's own PR, so M6 sequencing its own work creates no cross-milestone
  dependency, while M1 stacking on it would import another milestone's blockage.
  **Same PR, opposite answer; the variable is whose blockage you inherit.**

- **D-89 · the census gate refused to certify itself, and that is the general
  answer.** `RATCHET SLACK: 39 < MAX=44` / `negative-control: FAIL —
  gate-exit-0-not-1`. With slack a seeded violation reached 40 and passed.
  **Every instrument failure in this milestone was that class** — a single-line
  regex blind to multi-line stubs, a liveness poll matching a persistent footer,
  a criterion scoped so it could not observe, a differential whose generator
  excluded the feature. **A gate that checks whether it can still fail is the
  general remedy**, and it is non-optional and CI-run in this ticket, because a
  control that only ever ran on someone's laptop is a claim.

- **D-90 · acting under a pause is not symmetric with complying with one.** No
  written release exists; the project owner cannot author one — pause and release
  are the machine owner's instruments — and it is escalated. Internal work only,
  measurement and a draft, and **the ledger push is held** though the ledger
  itself is updated: the sweep is internal, the push is not.


- **D-91 · the closure counter I drafted was transitive-blind, and it was already
  wrong — not merely fragile.** `MAX` moves **9 → 12**, declared as an
  **instrument correction, not a raise**: the world did not change, the
  population did. The old row counted **direct** `build-depends` edges while the
  contract's subject is the **closure**, so its 9 was a direct-edge count wearing
  the closure's name. The project owner's review called it latent — *"today it is
  harmless, both instruments read 9"*. **Measurement says otherwise: four `lib/*`
  packages are transitive-only at `803e376bae`**, none mentioning `cardano-api`
  anywhere in its `.cabal`: `address-derivation-discovery` (→ primitive:57 →
  cardano-api:51), `cardano-wallet-application` (:73, :102),
  `cardano-wallet-ui` (:118), `cardano-wallet-blackbox-benchmarks` (→
  benchmarks:64 → cardano-api:188). **A direct-edge counter reaches zero while
  the closure is non-empty** — a false zero on this milestone's own acceptance
  criterion, delivered by the instrument built to protect it.

- **D-92 · library-only closure adopted (12) over any-stanza closure (13).**
  Transitivity propagates only through **library** components. All four
  definitions were measured rather than one chosen silently: any-stanza direct 9,
  any-stanza closure 13, library-only direct 8, library-only closure **12**. The
  readings disagree on exactly two packages and both disagreements are real —
  `cardano-wallet-benchmarks` is direct under one and transitive-only under the
  other (its `cardano-api` edges are in `benchmark db`/`benchmark api`, not its
  library), and `cardano-wallet-blackbox-benchmarks` leaves the closure entirely.
  **Adopted because the outcome sentence says "no production LIBRARY under
  `lib/`", and a benchmark stanza is not a production library** — scope of
  evidence matched to scope of claim, applied to my own row. **Offered to the
  project owner as a ruling rather than settled here**, because the two
  definitions disagree about what *done* means. Cost measured so no
  disproportionality ruling is needed: **0.315s / 0.304s / 0.307s**, 28 packages,
  1423 edges, pure `awk`+`grep`, no `cabal`, no network.

- **D-93 · the registry held the right invariant and the instrument drifted from
  it.** `m1-closure` has said since the milestone opened: *"no production library
  under `lib/` depends on `cardano-api`, in the Cabal/Nix dependency **closure**"*
  — correct on day one. The check drafted to enforce it silently narrowed the
  population, and Requirement 3 **on the same page of the same draft** forbade
  exactly that. **A contract registry only works if someone re-reads the invariant
  when building the check**; otherwise it records the right thing while
  enforcement drifts, which is a slower `enforced: NONE`. Fourth population-narrower-
  than-claim defect this week, second one inside an instrument built to catch it.

- **D-94 · seat identity comes from runtime roots and journals, never from window
  names.** The release's seat table listed `%379` and `%350` as live seats; neither
  pane exists, and `t5413-review-findings` holds `%387` running bare `bash`. It
  also attributed `%310`/`%326` to `t5419-specs-deprecations` four days after
  their retarget, **because this desk never renamed the window** — my omission
  propagated into the machine owner's record and then into an instruction to M6
  to *"resume %379"* that was wrong twice over. Renamed. **Window names drift and
  panes die; runtime roots and journals do neither.**

- **D-95 · a current date over unre-read sources is worse than a stale date.**
  `state.md` carried `Updated: 2026-09-04` while its rows still read *"#5413
  APPROVED, merge is the operator's"* and *"#5420 open, awaiting a human"* — both
  merged. Rewritten against re-read sources. Separately: `description.md:58` has
  pointed at `wiki/M113-State` since publication and **that page had never
  existed** — the milestone's only link to its currency was a 404 for its whole
  life. Published, wiki `8d30140`. `publish-state`'s render check returned RED on
  *could-not-evaluate* and was resolved by an **independent control** (`curl`:
  `http=200`, `data-type="mermaid"` and `js-render-enrichment-target` present,
  `highlight-source-mermaid` absent), **not by rerunning until green**.


- **D-96 · THREE ROWS. A zero can be true of the milestone and false of the
  repository.** Project ruling on the 12-vs-13 question this desk refused to
  settle alone. **`closure-lib` = 12 is M1's completion criterion; `closure-any`
  = 13 is a second ratcheted row that is explicitly not one.** The reason is what
  neither definition alone can express: **if `closure-lib` reaches 0 while
  `cardano-wallet-benchmarks` still names `cardano-api` at `benchmark db`:188 and
  `benchmark api`:229, the pin cannot leave `cabal.project`.** A single row forces
  a choice between a criterion that **overstates** (any-stanza — blocks M1
  acceptance on work that is not M1's) and one that **understates** (library-only
  — reports done while the dependency is still built). `closure-any` is owned by
  whoever removes the pin; **M1 acceptance does not wait on it.** Each row prints
  what it licenses, because a number without its licence is read as the criterion
  next to it.

- **D-97 · one case proves the two closure rows are different computations.**
  `cardano-wallet-blackbox-benchmarks` is **in** `closure-any` and **out** of
  `closure-lib` — its only path runs through a `benchmark memory` stanza. Without
  it the two rows would be the same number printed twice and nobody could tell.
  Required as `closure-lib`'s exclusion demonstration for exactly that reason.
  `cardano-wallet-read` excludes correctly under **both** definitions (verified
  absent from the any-stanza 13 as well) while **nine in-closure packages depend
  on it** — proximity includes it wrongly, only a directed-edge computation
  excludes it correctly.

- **D-98 · PROJECT RULE — when you build a check for a registered contract,
  re-read the contract's own words and quote them in the check.** Adopted by
  `%107` from this desk's finding, in its operative form. **A registry that is
  right while its gates are narrower is a registry that lies slowly — and it lies
  with the authority of something that was audited once.**

- **D-99 · the sample-one-route error, third instance this week.** `%107`
  confirmed D-91 and named its own error exactly: it checked the packages
  reachable through `cardano-api-extra`, found both carried direct edges, and
  concluded about **all** paths — while the four real cases run through
  `cardano-wallet-primitive` and `cardano-wallet-benchmarks`. **It sampled one
  route and claimed the population, inside the note diagnosing that exact error
  in this desk's draft.** Recorded because the shape now has instances in three
  different desks' instruments in one week, which makes it structural rather than
  personal.


- **D-100 · #5423 FILED AND THE LANE IS LIVE.** Operator cleared it — *"make it a
  PR"* — collapsing the three-step gate at step three. Issue
  https://github.com/cardano-foundation/cardano-wallet/issues/5423, parent #5237,
  base `master` `6b42c36b58`, lane `%441` `claude` T.O. / `%443` `codex` commit
  owner / `%442` `claude` auditor, `draft=NONE`. **The draft was not pasted as the
  body**: a ticket draft is written for this hierarchy and a public issue must not
  carry it. Rewritten impersonal, internal terms stripped, and **the sweep
  controlled in both directions** — forbidden terms 0, `closure`/`ratchet` 22/4,
  so the grep is proven able to find what is there.

- **D-101 · the gate enforces one direction and that is deliberate.** The merged
  sibling instrument exits **1** on an addition and **0** on a retirement that
  leaves `MAX` alone, printing the remedy. **Keep the asymmetry:** a hard failure
  on slack turns every cross-effort retirement into a red build for a branch that
  did nothing wrong — which #5420 already did to another count while both efforts
  were paused. **An instrument that goes red on the outcome it wants is badly
  aimed.** But **state it**: `GATE GREEN` does not mean the ratchet is current, in
  the issue *and* in the gate's own output, and keeping it tight is the landing
  desk's obligation at review. The acceptance criterion moved from *"no slack"* —
  which was enforced by a message pretending to be an exit code — to *"no slack at
  land time"*.

- **D-102 · seats are decided by what the work TOUCHES, not by what it COMPUTES,
  and the helper cannot know the difference.** `alternate-authoritative-cli`
  enforces alternation and the role fence; it has no view of the work, so it
  **cannot apply the secrets bar**. Taking its output as the answer nearly seated
  a barred family. Both this desk and `%107` made the mirrored error — `%107`
  validated on what the gate *computes* (*"a shell gate counting cabal edges
  touches no credential"*), this desk asserted `ci.yml` from the repository's
  general shape. **Both wrong on the fact:** the precedent wires into a standalone
  `.github/workflows/dijkstra-census.yml`, 47 lines, zero secrets references, whose
  header says *"Deliberately NOT folded into `ci.yml`"*. **One grep in the artifact
  already cited as the thing to copy would have answered it.**

- **D-103 · staffing must not drive architecture, in either direction.** A
  standalone workflow needs no secrets and would re-open `glm`/`grok`. This desk
  refused to specify it for that reason and paid two cheaper families to hold the
  line. `%107` then kept the conservative seats on the stronger form of the same
  argument: **seating families permissible under only one outcome pre-commits the
  design** — the identical inversion arriving from the parent's side. Ordering
  fixed in the lane's brief: **decide the wiring on the sibling's merits, then
  revisit seats** — and if the lane finds itself weighing a wiring decision by
  which families it frees, it stops and escalates.

- **D-104 · a probe that never ran reports the same zero as a probe that found
  nothing.** Preflighting the `START` pattern, this desk ran `status-event`
  against a **non-existent** file; it refuses one and exits 1, so nothing was
  written, the grep found nothing, and the preflight printed `FAIL` — for a
  pattern that matched the lane's real `START` seconds later. **"The probe never
  ran" was read as "the pattern is wrong."** Acting on it would have "fixed" a
  correct pattern. **Standing method:** create the file, **assert the probe wrote
  ≥1 line**, and only then treat a non-match as real. This is the
  before-believing-a-zero rule, failed inside the instrument built to apply it.

- **D-105 · pane text is not a control record, and raw `send-keys` is not
  delivery.** An operator instruction sat unsubmitted in `%310`'s composer for
  **147 minutes**; this desk reported the lane as *running* on the strength of a
  2.5-hour-old acknowledgement, then cleared the instruction while trying to
  submit it with raw `tmux send-keys`. Recovered by dispatching the same step as a
  durable note **in this desk's own voice** — it was within the lane's standing
  mandate — rather than retyping the operator's words, which would have been the
  wrong repair for a transport failure. **`send-pointer` settles the paste,
  retries submission, and waits for a post-cursor acknowledgement; it exists for
  exactly this.** Liveness is judged on **journal age first**, always.

## Parked decisions (P) — each with what unblocks it

- **P-1 · RESOLVED** by `A-001` (see D-5, D-6). Superseded by **FINDING-002**:
  the ruled landing order cannot execute as written, because #5399 cannot raise
  a `MAX` that row 2's own PR creates, and the precondition forbids landing into
  the red that would cause. One clause fixes it; `%107` is carrying it to M6.
  **Not blocking** — row 1 is unaffected.
- **P-2 · RESOLVED, and the reason is now stronger.** No unification with
  #5407 (D-7): not merely that it is DRAFT under an operator hold, but that
  **its single commit is a plan, not a gate** — no census check appears anywhere
  in its rollup. **There is nothing to unify with.** Causality also corrected by
  `%107`: #5407 was created 2026-08-27T08:51:54Z and the lane parked
  2026-08-28T10:23Z, so the PR predates the hold by more than a day. It is M6's
  child under an operator product hold: **do not touch, do not re-run its CI,
  do not comment.** Its `Conway Integration Tests` red is an M3-backlog flake
  (#5094 / #5108 signature), noted, not actioned, and no desk owns it.

## Owed units (registered at creation, per standing rule)

- **U-1 · un-stack the deprecations branch — DISCHARGED 2026-09-01, verified.**
  Executed by **the operator** at `15:08:29+01:00`, the minute #5399 merged.
  Verified here by **patch-id across the replay** — `efabcb19ccde3cb7`,
  `ff96a9cc56232a53`, `164e2fd09d0bf642`, `160a5f0ac88f4603`, identical before
  and after — with the pre-rebase head recovered from `origin` rather than
  trusted. Nothing dropped, nothing altered. **An owed unit executed by the
  authority above its owner is still that owner's to verify.**
  *(original text)* U-1 · un-stack the deprecations branch. Owner: **M1 milestone owner** (not
  the lane — a lane closes when its PR merges and this happens after).
  Trigger: **any rewrite of #5399's history, OR #5399 merging — whichever comes
  first.** The rebase precedes the merge (M6 rebases when #5402 lands) and it is
  the rebase that orphans this branch; the old "#5399 merged" trigger fired one
  event too late. Discharged by **observation** — record the base SHA at branch
  time, then `git merge-base --is-ancestor <base> origin/chore/issue-5397-node-11-1-0`;
  The probe is `git ls-remote --exit-code` (**not** `git fetch`, whose status is
  easily lost through a pipe) and it has **four** outcomes:
  **0 + is-ancestor 0** intact; **0 + is-ancestor 1** trigger fired, rebased;
  **2** ref absent -> trigger fired, merged-or-deleted; **128** transport error
  -> **NO VERDICT, retry, do not act.** A network blip must never read as
  merged-or-deleted, or the un-stack runs against an event that never happened.
  **Only a cleanly distinguishable result is a verdict.**
  `%107`'s relay from M6 is redundancy, not the primary signal: a relay nobody
  sends is a check that cannot fail.
  Steps: rebase onto the new base, retarget PR base,
  and **verify the replayed commit set by comparing before and after** — a clean
  rebase that silently drops a commit looks identical to a correct one.
- **U-5 · retire the `lib/integration/**` deprecation suppression — FILED as
  #5412** on 2026-08-28. `.../Shelley/TransactionsNew.hs`, entry 2 of #5399's
  five. The row-2 allowlist entry moves from `unfiled:U-5` to **`#5412`**. **Owner: M1
  milestone owner** (registration only); the work is a **standalone M1 ticket**,
  not a slice of the deprecations ticket, because #5290's integration policy
  prescribes exactly that remedy and the fence is inherited, not this desk's to
  trim. **Trigger: the deprecations ticket clearing the operator gate**, filed as
  its sibling — deliberately not earlier, since nothing may be filed before
  clearance and an earlier trigger would be unexecutable. Until filed, the row-2
  entry reads `retires-with: unfiled:U-5`.

- **U-6 · DISCHARGED 2026-09-01T16:20:46Z by its owner at its trigger**, no
  fallback needed. #5419 now reads *"covered by #5418 (M7)"* — **verified
  independently at this desk against the live issue body**, not accepted from the
  lane's report. 33 seconds from `START`. *(original)* U-6 · amend #5419's exclusion to "covered by #5418 (M7)"** so the
  cross-milestone dependency is visible on the face of the ticket.
  **Owner: `%310`**, as task 1 of its re-brief. **Trigger: `%310`'s fresh
  `START`.** **Fallback: if that `START` has not landed by
  `2026-09-01T23:59Z`, the milestone owner does it as a declared one-off** —
  *the altitude line is worth more than an hour, not more than a week*.
  **Observed how:** this desk owns no scheduler, so the trigger cannot fire on
  its own; it is checked at **every turn this desk takes** — `%310`'s STATUS for
  a post-cursor `START`, and the clock — and acts on whichever comes first.

- **U-7 · the #5420 audit — OWED, NEVER RUN, DEFERRED.** No auditor was ever
  seated. Seating one is new work the `5420-then-dark` ruling forbids.
  **Owner: `%122`. Trigger: this lane's release, or any move to accept #5420,
  whichever comes first.**
  **Load-bearing: GREEN-AND-OPEN IS NOT ACCEPTANCE.** A green PR is not an
  audited one, and that fact must survive the lane going dark — otherwise the
  next reader infers acceptance from CI.

- **U-2 · DISCHARGED 2026-09-02.** Criterion re-verified against `master`
  @ `4a2227a`: **row 2 = 12** suppressions, **row 3 = 9** `lib` packages, negative
  control passing. **And the uncomfortable reading: master is identical to before
  any M1 work** — it went `7 -> 12` through #5399's raise and has come back down
  by nothing, because neither #5413 nor #5420 has merged. **A week of M1 output
  sits entirely in two unmerged PRs.**
- **U-4 · CLOSED AS UNOBSERVED, not as satisfied — 2026-09-01.** #5399 merged at
  `14:08Z` carrying its suppressions to `master`, and **the row-2 instrument that
  was supposed to watch that window was never built** — the closure-instrument
  ticket is still unfiled at the operator's gate. The whole apparatus around it —
  `MAX`, the same-PR raise rule, both carve-outs, the ceiling protocol — governed
  an event **no instrument saw.** Owner of the consequence: `%122`. Trigger: the
  closure instrument landing, at which point row 2's `MAX` is measured against a
  `master` that has **already absorbed the raise**.
  *(original)* U-4 · the expected-red window on #5399. Opens when row 2 lands on
  `master`; closes when #5399 carries its MAX edit. Owner **M6 desk**, relayed
  by `%107`; this desk never contacts them. Named so nobody sees a red gate and
  reverts the instrument. One PR, active lane, one-line fix — *an instrument
  reporting an undeclared rise is an instrument working.*

- **U-8 · DISCHARGED BY EVENTS 2026-09-04** — #5420 merged, so there is nothing
  left to un-stack. The rebase it required was executed and verified on 09-02.
  *(original)* U-8 · un-stack #5420. **Owner: `%122`** — not the lane, by `U-1`'s
  reasoning: a lane closes and this happens after. **Trigger: #5413 merged, OR
  any rewrite of `refactor/5411-txbody-fields-from-ledger`, whichever first.**
  Steps: rebase onto the new `master`, retarget the PR base, **verify the
  replayed commit set by comparing patch-ids** — not by a clean rebase exit.
  The procedure is **proven, not hoped**: executed correctly once on `U-1`.
  **Do not pre-empt the merge.**

- **U-9 · re-cut `INV-3`.** Owner **`%122`**. Trigger: the evidence lane's
  measurement arriving via `%107`. **Requirement, fixed in advance so it is not
  negotiated after the answer is known: the re-cut invariant must cover THE
  CONSUMER, not only the site.** If a ticket's owned set includes a consumer, the
  evidence must exercise that consumer — **a differential over the migrated site
  can never observe a change in what the site hands on.**


- **U-10 · audit the #5420 campaign.** **Owner: `%122`.** **Trigger: `%441`'s
  submission reaching its auditor** — at that moment a fresh-audit pattern is
  established, an auditor seat is free, and nothing competes for it. **Observable
  without a scheduler**, which is the only kind of trigger this desk accepts.
  No auditor was ever seated on that campaign; it has appeared as *"still owed"*
  in five consecutive reports, which is drift, not patience.
  **Say at seating time that this is an audit of SHIPPED code** — the campaign-2
  commits are on `master` — because that changes what a finding means: not a gate
  on a merge, but a defect report against production.

- **U-11 · restore the script-witness parity suite.** **Owner: `%122`.**
  **Trigger: #5423 merged.**
  **Reframed 2026-09-04 and this is the substantive change:** it is **not** a
  floating debt of this desk's. It is **#5288's third acceptance criterion**,
  verbatim: *"New unit or golden tests compare ledger-builder output against the
  existing cardano-api builder for representative script-witness cases."*
  **Verified in the code, not taken from the note:** `buildLegacyParityTx`
  (`TransactionLedgerSpec.hs:1462`) still ends `in body`, returning
  `mkUnsignedTx`'s result — and since #5413 `mkUnsignedTx` returns
  `Write.Tx era`. **The legacy arm no longer traverses `cardano-api` at all**, so
  the comparison no longer spans the boundary it was written to span and stays
  green either way.
  **Therefore #5288 is legitimately OPEN** — which settles the earlier
  false-open question in the opposite direction from the one this desk suspected:
  its implementation commit `088de979f0` is on `master`, but criterion 3 is unmet.
  **Closing this gap closes #5288.**

- **U-3 · retire both ratchet rows together** at `MAX = 0`.

## Fences (verbatim, carried into every child brief)

1. **No direct contact with the M6 desk, its epics, or its lanes.** Anything
   that crosses goes to `%107`.
2. **`Cardano/Api/Extra.hs` is not deleted — and no PR that deletes it lands —
   without a ruling from `%107` naming the M6 state at that moment.** The one
   hard interlock in this milestone.
3. **`/code/cardano-wallet` is read-only bootstrap ground.** It carries a
   protected untracked file, `.llm/issue-5309-unit-memory-analysis.md`, which
   the operator ordered preserved. Never mutate that checkout; every lane works
   in its own worktree.
4. **Bisect-safety (#5237's own constraint):** each child PR is either additive
   or removes a fully migrated slice, with local verification recorded in the
   PR. No lane bundles a migration with a deletion.
5. **Seat policy:** ticket-owner seats on a metered family (`claude`/`codex`);
   commit-owner family ≠ ticket-owner family; fresh auditor family ≠ commit
   owner family. `grok`/`glm` may not occupy a seat touching production secrets.
6. **Census denominator guard (inherited, binding):** the M6 census criterion
   must **not** be narrowed to exclude `Extra.hs`. The count falls when stubs
   die — deletion or implementation — **never by agreement between two desks**.

## Two objects both called "Api Extra" — do not conflate

| object | path | consumed by | in scope? |
|---|---|---|---|
| module `Cardano.Api.Extra` (the 5 stubs, **fence 2**) | `lib/wallet/src/Cardano/Api/Extra.hs` | inside `lib/wallet` | yes, #5290 AC names it |
| package `cardano-api-extra` | `lib/cardano-api-extra/` | **`lib/unit` only** | **named in no AC** — desk calls it test-only, out of row 1, as a *demonstrated exclusion* |

## Evidence discipline binding every instrument this desk lands

**The scope of the evidence must match the scope of the claim — in class, and in
granularity.**

- **Class.** A closure gate is proven red by a **closure violation**, not by an
  import line. Constructible here and therefore not substitutable: add
  `cardano-api` to the `build-depends` of a package the wallet library depends
  on transitively, with no source importing it — grep stays green, closure gate
  must go red. Both runs pasted in the PR.
- **Granularity.** A build of the tip cannot test a claim about every commit.
- **Sanctioned build path, named in the receipt.** Exactly two exist, enforcing
  `-Werror` by different mechanisms: CI/nix via the per-package `release` flag
  (`nix/haskell.nix:277-278`), and `just build` via command-line
  `--ghc-options="-Werror "` (`justfile:51,61`). A bare `cabal build` is not a
  gate — #5399's gate used a third path that exists nowhere in this repo and
  fed GHC an unrecognised flag 180 times, unread.
- **Class separation.** Row 1 counts closure membership; row 2 counts
  **deprecation** suppressions — not "all `OPTIONS_GHC` pragmas". Each row
  states what it counts and demonstrates a case it correctly excludes.
- **#5400 documents this repo's whole `-Werror` topology.** Read it; do not
  re-derive it. Not M1's to fix.

## Suppression surface — MEASURED at `origin/master@0a7332482c`, 2026-08-28

**7 files, 7 occurrences, both spellings.** Measured in a throwaway clone
fetched from GitHub — **not** the fenced bootstrap checkout, which was stale at
`346786a112`. Cross-checked: `346786a112` also gives 7, and all seven
suppression-bearing files are **byte-identical** across the two SHAs, so the
agreement is a fact about the tree rather than one instrument counted twice.

Instrument, stated:

```sh
git grep -lE -- '-(Wno-deprecations|fno-warn-deprecations)' \
  -- '*.hs' '*.cabal' 'cabal.project*' | wc -l
```

```
lib/faucet/lib/Cardano/Faucet/Addresses.hs                       -Wno-deprecations
lib/unit/test/unit/Cardano/Wallet/ApiSpec.hs                     -fno-warn-deprecations
lib/wai-middleware-logging/src/Network/Wai/Middleware/Logging.hs -fno-warn-deprecations
lib/wallet/src/Cardano/DB/Sqlite.hs                              -fno-warn-deprecations
lib/wallet/src/Cardano/Wallet/Shelley/Transaction.hs             -Wno-deprecations
lib/wallet/src/Cardano/Wallet/Shelley/Transaction/Ledger.hs      -Wno-deprecations
lib/wallet/src/Cardano/Wallet/Shelley/Transaction/Unsigned.hs    -Wno-deprecations
```

Controls run and recorded (`REPORT-003`): **POS** legacy `-fno-warn-deprecations`
→ 8 RED, revert → 7 GREEN; **POS** `.cabal` `ghc-options` suppression → 8 RED,
revert → GREEN; **NEG** unrelated `-Wno-unused-top-binds` → unchanged;
**NEG** documentation prose excluded.

**Consequence:** #5399's declared raise `MAX 7 → 12`, `N = 5`, **stands**. Its PR
body needs no change.

**This number is provenance, not a MAX to hardcode.** The lane re-measures at
land time and inherits nothing — including this ledger. An instrument knowing
one spelling is born blind to three of these seven sites.

### The exclusion that defeats the obvious fix

A naive repo-wide grep returns **9**: `ERA-CHANGES.md:372` and `TODO.md:55`
quote the pragma in prose. Tightening the regex to match the pragma's *shape*
**still returns 9** — the docs quote it verbatim, backticks and all. *A regex
cannot distinguish a pragma from a faithful quotation of one.* The exclusion
must be by **file class**, not pattern shape. This is row 2's most instructive
demonstrated exclusion: a guard strictly more precise than the naive one, and
still wrong.

## Cross-milestone state (read-only observation, no contact)

| PR | head | state | why M1 cares |
|---|---|---|---|
| #5402 | `2f2e5ed` | OPEN, `CHANGES_REQUESTED`, BLOCKED | M6's; blocks #5399; the review was never answered though the head moved |
| #5399 | `33ccafc` | OPEN, BLOCKED, CI green 58/58 | the deprecations branch stacks on its head; creates most of that surface |
| #5407 | `53f95d5` | DRAFT, base `master` | M6's census ratchet — P-2 |
| #5364 | — | DRAFT | unrelated |

**M1 depends on M6.** Accepted at project level as a deliberate choice, not
absorbed as a side effect. Cost visible: **two PRs must land before the
deprecations ticket can.**

`cw-e5209-t0-census-ratchet` (`%106`, `%109`, `%110`) is **PARKED** on operator
instruction. Not ours, not to be touched, not to be inferred as released.

**Routing:** pause/release/capacity reaches this desk **only through `%107`**.
An order arriving from any other desk is unrouted — check with `%107` before
acting on it. (On 2026-08-28 a machine-level release reached the M6 owner
directly while its project owner was still paused.)

## Host caveat

Host-wide signing fault 2026-08-27 ~13:08Z–13:45Z: `gpg.format` invalid, every
signed commit on this host failed. Fixed. Work committed in that window has its
signatures **verified, not assumed**.

## Row 3 baseline — MEASURED at `origin/master@0a7332482c`, 2026-08-28

**9 packages** under `lib/` list `cardano-api` in `build-depends`. Measured in a
throwaway clone fetched from GitHub, ref named, not the bootstrap checkout.

```
lib/api                lib/network-layer      lib/primitive     lib/wallet      <- row 1 covers these 4
lib/benchmarks         lib/cardano-api-extra  lib/integration   lib/unit
lib/local-cluster                                                               <- row 1 excludes these 5
```

Controls: **POS** add `cardano-api` to `lib/numeric` → 9 → 10 RED, revert → GREEN.
**NEG** the strict dependency pattern matches `, cardano-api` and **not**
`, cardano-api-extra` — `lib/unit` depends on both, so a pattern that cannot
separate them gets the population wrong in both directions.
**SWAP** remove from `lib/benchmarks` + add to `lib/numeric` → count unchanged at
9, set-based row RED. That swap is row 3's entire justification.

**Provenance, not a value to hardcode.** The lane re-measures at land time.

## Row 2's seven entries — READ, not counted, at `origin/master@0a7332482c`

| file | stated cause | `cardano-api`? | disposition |
|---|---|---|---|
| `lib/faucet/lib/Cardano/Faucet/Addresses.hs` | `cardano-addresses` deprecates Byron *to discourage new use*; the wallet must support Byron addresses indefinitely — *"for this purpose it isn't deprecated"* | no | `PERMANENT` |
| `lib/wai-middleware-logging/src/.../Logging.hs` | Wai `requestBody` getter returns chunks; the middleware wants the chunks | no | `FOREIGN` |
| `lib/unit/test/unit/Cardano/Wallet/ApiSpec.hs` | `Network.Wai.Internal` `requestBody` used as a **setter**; the getter is deprecated | no | `FOREIGN` |
| `lib/wallet/src/Cardano/DB/Sqlite.hs` | bare `-- TODO: …/browse/ADP-2841` — a **pointer, not a cause**; **imports `Cardano.Api` ZERO times** | **no, statically settled** | strongest `OBSOLETE` candidate — **run first** |
| `lib/wallet/src/Cardano/Wallet/Shelley/Transaction.hs` | **none — bare pragma** | unestablished | `UNKNOWN` |
| `lib/wallet/src/Cardano/Wallet/Shelley/Transaction/Ledger.hs` | **none — bare pragma** | unestablished | `UNKNOWN` |
| `lib/wallet/src/Cardano/Wallet/Shelley/Transaction/Unsigned.hs` | **none — bare pragma** | unestablished | `UNKNOWN` |

The three `UNKNOWN` rows are *presumably* the `cardano-api` ones. **"Presumably"
is not a disposition.** The lane establishes each by removing the pragma and
reading what warns under the sanctioned build path.

**Provisional dispositions, not a value to hardcode.** The lane re-reads at land
time and inherits nothing, including this table.
