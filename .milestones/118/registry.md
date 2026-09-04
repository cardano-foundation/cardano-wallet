# M6 — Dijkstra HF readiness (#118) — contract registry

Every entry names the contract, its parties, the invariant, and the check that
enforces it — or `enforced: NONE` explicitly. An unstated NONE is a scheduled
incident.

Two entries carry project-owner rulings as of 2026-08-25 (`A-001`, `A-002`).
**A ruling is not enforcement.** Where a ruling is the only thing holding an
invariant, the entry says so and names the check that must replace it.

---

```
contract:   node runtime pin ↔ Haskell dependency pin-set
parties:    flake.nix `cardano-node-runtime` input (publishes the node binary
            the integration suite runs against)
            cabal.project `constraints:` block (pins cardano-api/cardano-cli/
            ledger/base versions the wallet compiles against)
invariant:  the node version the wallet is COMPILED against equals the node
            version it is TESTED against; a bump moves both or neither
observed:   consistent today — flake.nix:125 ref=11.0.1, cabal.project comment
            "Cardano Node 11.0.1 dependencies" with cardano-api ==11.0.0.0
enforced:   NONE — the linkage is a comment in cabal.project, not a check.
            Nothing fails if flake.nix advances and the constraints block does
            not; the suite would simply run a newer node against older client
            libraries and pass until a wire-format difference bites.
action:     commissioned as the epic's first check. Highest-value NONE in this
            milestone, because moving exactly these two things IS the epic.
```

```
contract:   pinned cardano-node ref must resolve to a STABLE release
parties:    IntersectMBO/cardano-node release process (publishes; marks
            prereleases)
            flake.nix `cardano-node-runtime` ref + cabal.project constraints
invariant:  the pinned ref is a non-prerelease, unless an explicit recorded
            waiver is present
ruling:     A-002, 2026-08-25, project owner. DECOUPLE: migrate against 11.1.0
            now; hold the pinned ref on a stable release until a stable 11.1.x
            exists; the pin flip is its own final gated child, never a side
            effect of a migration child.
enforced:   NONE — and this is the defect the ruling exposed rather than fixed.
            Today the answer is "hold", and the only thing holding it is a
            sentence in a ruling that a future lane will not read.
action:     COMMISSIONED. A check that fails when the pinned ref resolves to a
            prerelease absent a recorded waiver. Per `invariants` it must be
            proven able to fail: demonstrated RED against a deliberately
            prerelease-pinned tree before anyone trusts it. This is what turns
            the ruling into an invariant.
note:       the "this repo never pins prereleases" precedent is WEAK evidence
            (a `-S` history search over flake.nix surfaced one pin-change
            commit only, because the pin string's form changed) and the project
            owner explicitly declined to have it strengthened — the ruling does
            not rest on precedent. Do not launder that weak signal into a fact.
escalation: if the Dijkstra HF lands before a stable 11.1.x, that is a
            user-facing release-policy question for the OPERATOR, routed
            through the project owner. Carry the date collision EARLY.
slug:       pinned-ref-must-be-stable
```

```
contract:   CHaP flake input ↔ cabal.project CHaP index-state
parties:    flake.nix CHaP input (Nix-side package set)
            cabal.project `index-state:` for cardano-haskell-packages
invariant:  both advance in lockstep; a Nix build and a cabal build resolve the
            same package versions
enforced:   NONE known at this altitude — to be confirmed by the epic owner.
            The known failure mode is not a red build but Windows
            cross-compilation dying in iserv/wine because a CHaP mismatch
            resolved a different th-orphans/network, i.e. a seam that fails far
            from where it was broken.
action:     epic owner reports in D1 whether CI actually catches divergence; if
            not, this becomes a commissioned check.
cross-ref:  #5381 (ms #119) states this same lockstep requirement in its own
            acceptance criteria — the shape recurs across milestones, which is
            precisely why it lives at this altitude.
```

```
contract:   minimal-advance discipline on the pin-set
parties:    this milestone (wants Dijkstra readiness)
            the wider ecosystem bump (Hackage index-state, cardano-crypto-class,
            cardano-addresses, and everything downstream)
invariant:  the pin-set advances by the minimum needed for the outcome; an
            ecosystem bump is not dragged in wholesale as a side effect
observed:   under pressure. The epic owner reports 11.1.0 moves the **Hackage**
            index-state 2026-03-26 → 2026-08-14 — a ~4.5-month jump on the
            GENERAL index, not just CHaP — and that 11.1.0 removes the legacy
            tracing system and LMDB. That is exactly the "de-facto ecosystem
            bump" shape #5381's criterion 5 exists to refuse, and the same
            discipline binds this milestone.
enforced:   NONE — a review convention, not a mechanism
action:     D1 must state the exact package closure 11.1.0 forces to move, and
            must name whether cardano-addresses and cardano-crypto-class are
            in it. `A-001` and this entry both turn on that answer.
```

```
contract:   #5209's acceptance criterion ↔ the actual Dijkstra stub surface
parties:    issue #5209 (declares the criterion)
            lib/ (carries the stubs)
invariant:  the criterion goes red while any Dijkstra stub remains
observed:   FALSE today. The criterion greps one message shape
            ("DijkstraEra not yet supported", 6 sites). At least five shapes
            exist; the real surface is 44 across 15 files (38 error stubs,
            multi-line-aware, + 6 `pendingWith`). The criterion goes green with
            38 of the 44 still live — a check that would appear in a burn-down as proof of an
            outcome it never tested.
enforced:   NONE — worse, a criterion that reads as enforcement and is not
ruling:     project owner, 2026-08-25, made the re-cut BINDING: span every
            message shape plus the 6 `pendingWith` era tests, install in CI,
            and prove it RED on today's tree before trusting it. A green from
            an unproven criterion does not close #5209.
slug:       dijkstra-stub-census
```

```
contract:   cardano-addresses version ownership across milestones
parties:    #5381 (milestone #119 "M7", OPEN, target 4.0.8)
            this milestone, via the 11.1.0 pin-set advance
            (#5293, ms #114, target 4.0.6 — RULED SUPERSEDED, not a claimant)
invariant:  exactly one owner moves cardano-addresses; the other consumes it
ruling:     A-001 then CORRECTED by A-004, 2026-08-25, project owner.
            No three-way collision: #5293 is a strictly weaker, older statement
            of #5381 and its closeout is routed to the M119 desk. THIS DESK
            DOES NOTHING ABOUT #5293. Ordering stands: M6 advances the pin-set
            as its own declared scope; #5381 does not race it.
            `cardano-addresses` is NOT in 11.1.0's forced closure — confirmed
            twice — so Arc 2 costs this nothing.
CORRECTION: **"Arc 2's index-state advance unblocks #5381" is FALSE.** That was
            my inference, not the epic owner's finding, and the M119 desk
            refuted its earlier form. #5381 has TWO independent blockers and
            Arc 2 clears only one:
              1. CHaP index-state visibility — 4.0.8 entered CHaP 2026-08-16,
                 wallet index-state is 2026-07-20. Arc 2 does clear this.
              2. an EMPTY crypton intersection, which Arc 2 does NOT clear:
                   cardano-addresses 4.0.8      crypton >= 1.1 && < 1.2
                   cardano-crypto-class 2.3.3.0 crypton ^>=1.0  (i.e. < 1.1)
                   cardano-crypto-class 2.5.0.0 crypton ^>=1.0  (i.e. < 1.1)
                   cardano-crypto-class 2.5.1.0 crypton ^>=1.0  (i.e. < 1.1)
                 2.5.0.0 is what node 11.1.0 requires, so the intersection is
                 empty AT THE TARGET VERSION too.
evidence:   cardano-addresses.cabal:137 at tag 4.0.8 (local read-only clone);
            each cardano-crypto-class .cabal fetched at its cardano-base tag,
            HTTP 200, with a positive control (build-depends present, so it is
            a real cabal file) and a negative control (a bogus tag returns
            404). Re-derived at this desk, not relayed.
limit:      cardano-crypto-class 2.6.0.0 could NOT be checked — no such tag in
            cardano-base under that naming, and 404 is also what the negative
            control returns, so "absent" and "named differently" are
            indistinguishable by this method. NOT a claim that 2.6.0.0 fails to
            relax the ceiling.
withdrawn:  the planned `NOTE RELEASE:` from the epic on index-state landing.
            Publishing it would have told M119 a blocker had cleared when it
            had not — a release signal that is actively wrong is worse than no
            signal. Withdrawn by NOTE-003.
enforced:   NONE — the ordering is a ruling, not a mechanism. Nothing fails if
            #5381 races the advance. Accepted only because the lanes are fenced
            apart and both parents hold the sequence.
SUPERSEDED: 2026-08-25 — the upstream work is CUT. cardano-crypto-class
            2.6.0.0 declares `crypton ^>=1.1` (>=1.1 && <1.2), which
            intersects cardano-addresses 4.0.8 exactly. It entered CHaP
            2026-07-27, a month before four separate desks asserted no such
            version existed. Upstream already shipped the relaxation; nothing
            is to be cloned, widened, forked or upstreamed.
            Verified at this desk from the authoritative CHaP endpoint
            chap.intersectmbo.org/package/<pkg>-<ver>/<pkg>.cabal, with a
            positive control (9 build-depends blocks = a real cabal file) and
            a negative control (version 9.9.9.9 -> HTTP 404, so absence is
            detectable). Two of my three fetch methods for this fact were
            BROKEN and only positive controls revealed it.
            The blocker did not vanish — it MOVED. See
            `crypto-class-2.6.0.0-vs-node-aligned-pin-set` below.
```

```
contract:   milestone artifact ↔ outcome audit
parties:    the `nightly` pre-release line (in-flight artifact)
            the `Release vYYYY-MM-DD` line (graduation target)
invariant:  the outcome is audited against an artifact a stranger can obtain,
            not against a source tree
enforced:   NONE yet — the audit is not written. Recorded now so that
            MILESTONE-COMPLETE cannot be reached by counting closed epics.
action:     write the outcome-audit procedure before the first epic completes.
```

```
contract:   cardano-crypto-class 2.6.0.0 vs the node-aligned pin set
parties:    cabal.project:191 — pins `cardano-crypto-class ==2.3.3.0` INSIDE
            the `-- Cardano Node 11.0.1 dependencies:` block
            cardano-node 11.1.0 — carries 2.5.0.0, still `crypton ^>=1.0`
            #5381 in milestone #119 — needs a crypto-class whose crypton range
            intersects cardano-addresses 4.0.8's `>=1.1 && <1.2`
invariant:  the wallet's crypto-class version stays coherent with the node it
            is compiled and tested against
observed:   **Neither the current pin nor the 11.1.0 advance delivers 2.6.0.0.**
            Only 2.6.0.0 intersects 4.0.8. So satisfying #5381 means
            deliberately diverging from the node-aligned pin set.
            This is exactly the hazard `node-pin-lockstep` exists to catch, and
            it caught it BEFORE being implemented as a check — which argues for
            building the check, not for trusting the reasoning that stood in
            for it.
question:   can the wallet carry 2.6.0.0 while remaining coherent with its
            node-aligned pin set — at the current pin, at 11.1.0, or neither?
method:     settle with RESOLVER output, not declared ranges. Declared ranges
            got four desks wrong on 2026-08-25.
enforced:   NONE — no check asserts crypto-class coherence with the node pin.
            Subsumed by `node-pin-lockstep` once that is built.
owner:      this desk, held on behalf of milestone #119. NOT an Arc 2 trigger:
            Arc 2 is a contingency of Dijkstra readiness, and cardano-addresses
            is not in 11.1.0's forced closure. May well be answerable at the
            current pin, which is the best outcome available.
routing:    answer reports to the project owner, who forwards to M119. No
            lateral contact in either direction.
```

---

## Enforcement scheduling — A1.6

**Every `enforced: NONE` above is commissioned into Arc 1, not Arc 2.**

Recorded because the first version got this wrong in a way that would have been
invisible: #5395 commissioned four checks in a section of their own, with no
owning child and no arc. Three of the four are pin-related, so a reader files
them under Arc 2 — which the operator has made **conditional**. If Arc 1 closes
the Dijkstra surface, Arc 2 never runs and those three are never built.

The inversion is the whole point: under the operator's ruling the pin is meant
to **stay put**, which is precisely when `pinned-ref-must-be-stable` and
`node-pin-lockstep` matter most. Scheduling them inside a contingency that only
fires if the pin *moves* is backwards, and would have produced three registry
entries that read as commissioned and were never going to exist.

| check | owning child |
|---|---|
| `dijkstra-stub-surface` | **A1.0** |
| `pinned-ref-must-be-stable` | **A1.6** |
| `chap-lockstep` | **A1.6** |
| `node-pin-lockstep` | **A1.6** |
| `crypto-class-2.6.0.0-vs-node-aligned-pin-set` | subsumed by `node-pin-lockstep` once built |
| `cardano-addresses` ownership | ordering ruling only; no mechanism, by acceptance |
| `milestone-artifact-vs-outcome-audit` | outcome-audit procedure, owed before the first epic completes |

**A1.6 is a consolidation ticket, not three checks.** The epic owner identified
the shape: the last three are one `reconcile(declared, observed)` over a registry
of pin pairs. Per `invariants`, matching the shape is bookkeeping the epic owner
may do; designing the unification is code and goes to a ticket owner seated
strong — a cheap seat on a consolidation produces a fourth framework rather than
one mechanism.

Deliverable: one mechanism with a comparator **proven able to fail**; three
registered pairs; and **the registry of pairs itself checkable by the same
mechanism**, because a pair silently dropped from the registry is the next
version of this bug.

Named negative control: point `pinned-ref-must-be-stable` at `11.1.0`, which
**is** a prerelease, and require red. If it passes there, it is inert.

A1.6 depends on nothing, is parallel-safe with A1.1–A1.4, and touches no era
code — so it survives every reading of how Arc 1 turns out.

---

## Operator staffing exceptions — recorded as exceptions, 2026-08-25

The operator directed by name that the upstream-bump **sanity** work be staffed
`grok` ticket owner, `qwen` commit owner, `claude` Sonnet auditor. Two standing
`orchestrator-contract` rules are set aside for those seats:

1. **Ticket-owner seats must sit on a metered family** (`claude`/`codex`).
   `grok` is unmetered → **grok-as-TO is an operator exception.**
2. **`qwen` is draft-only and barred from every authoritative seat.** The
   mechanical helper `alternate-authoritative-cli` refuses it outright
   (`DRAFT-ONLY-AUTHORITATIVE-SEAT`, exit 65) → **qwen-as-commit-owner is an
   operator exception.**

The operator may set aside its own contract; a desk may not quietly redefine the
contract so an instruction looks compliant. These stay recorded **as exceptions
for as long as those seats exist**, and are **not precedent for any other
ticket**.

**What makes them tolerable is scope, not seat quality.** The operator's own
fence — *"no code mutation madness, only sanity checks"* — gives the ticket a
near-zero blast radius. The exceptions were granted against that radius and **do
not travel with a larger one**: if the scope widens to landing a bump, the
staffing returns to this desk before anything happens.

**Not set aside by the seat choice** (each carried into the ticket brief):

| fence | status |
|---|---|
| secrets bar — `qwen`/`grok` never near production secrets, credentials, or secret-bearing config | **binds.** Resolves cleanly here: a read-only ticket has nothing to push, so no barred seat approaches a credential. If that changes → stop and escalate, never hand a barred seat the secret |
| unmetered cap — one `grok` seat per ticket, family reported in STATUS | binds |
| `agy` REVOKED for every role | binds; the directive names grok/qwen/sonnet and does not reopen it |
| operator scope fence, carried **verbatim as forbidden scope** | binds |
| review, provenance, signing, acceptance, protected-file, live-boundary gates | bind — a staffing exception is not a gate exception |
| `/code/cardano-wallet` read-only, protected file intact | binds |

**PR-fence reading, stated before acting:** the directive fences this ticket to
sanity checks, which is *not* end-to-end bump authority, so **no PR is in scope
anywhere**. The project owner's "PR to `cardano-base` comes to me first" fence is
therefore **moot for this scope, not overtaken**, and revives if the scope widens.

### A1.6's ticket-owner seat — ruled 2026-08-25, and the exception does NOT reach it

**A1.6's ticket owner is a strong metered seat** —
`claude --model 'claude-opus-5[1m]' --effort high` or `codex-raw` at
`model_reasoning_effort=high`. Not Sonnet, not `grok`, not `qwen`. Alternation
applies downstream; `agy` stays revoked.

Three independent reasons, none of which depends on the others:

1. **The operator scoped its directive** to the upstream-bump *sanity ticket*,
   then narrowed even that to two repos. A scoped exception does not spread to a
   sibling ticket by adjacency.
2. **The standing rule already puts ticket-owner seats on a metered family.**
   That is the default; the sanity ticket is the departure. **A1.6 needs no
   exception because nothing excepted it.**
3. **`invariants` names this seat.** A consolidation is where a weak driver
   yields a fourth framework instead of one mechanism — and A1.6's entire value
   is being *one* mechanism.

**The distinction, which is the reusable part:** on a read-only sanity sweep a
weak seat that produces mush is cheap to **detect** and cheap to **discard**. On
a consolidation a weak seat produces something that **compiles, passes, and is
wrong in shape**, and the cost lands on everyone who later builds against it.
So *"it can be reviewed afterwards"* is not an answer here — **review catches a
wrong result; it rarely catches a wrong decomposition**, because by then the
decomposition is what the review is written against.

Binds A1.6 only. Not a general objection to cheap seats in this milestone and
it does not reach the sanity ticket, which keeps the operator's named seats. The
operator may override; that would be recorded as a **third named exception**,
never as a reinterpretation of the rule.

---

## The milestone's work, restated: the `cardano-deps` bump to node 11.1.0

Recorded because the registry was built around *Dijkstra stub elimination with
a conditional pin advance*. That framing is corrected: **the work is the
`cardano-deps` dependency-bump workflow with target `cardano-node` 11.1.0.**
Dijkstra era support is the outcome the bump delivers.

```
contract:   upstream source-repository-package pin sets ↔ the target node version
parties:    cardano-ledger-read, cardano-balance-transaction (each owns its own
            pin set)
            cabal.project's five source-repository-package entries (consume them)
invariant:  every SRP dep is pinned at a commit whose own pin set matches the
            wallet's target node version
observed:   VIOLATED today, in two different ways.
            - cardano-ledger-read: the 11.1.0 pin set EXISTS upstream at
              49b325ca (PR #22, branch chore/issue-21-node-11-1-0, OPEN). The
              wallet pins 242c5c85, which predates it. Consume 49b325ca.
            - cardano-balance-transaction: the wallet pins c3a340d1, which IS
              the merge of PR #42 chore/issue-41-node-11-0-1 — the 11.0.1 pin
              set. No 11.1.0 branch exists. It must be AUTHORED.
enforced:   NONE. Nothing checks that an SRP pin's own pin set agrees with the
            wallet's node target. This is the same shape as node-pin-lockstep
            one level out, and it is a natural fourth registered pair for A1.6's
            reconcile(declared, observed) mechanism.
method:     when reporting an upstream absence, STATE THE SEARCH SPACE AND
            JUSTIFY IT. `main` is not "upstream" — branches and open PRs are
            upstream too. Two false negative results were published today from
            exactly this error, both with correct positive controls on the
            instrument and none on the denominator.
```

**Intelligence carried by PR #22, relevant to the wallet's own 11.1.0 work:**
`Header` relocates out of `Ouroboros.Consensus.Protocol.Praos.Header` into
`Cardano.Protocol.Praos.BlockHeader`; `Cardano.Protocol.TPraos.BHeader` is
renamed to `.BlockHeader`; KES gains `fixedSize`; `PackageImports` becomes
necessary. The wallet will meet the same relocations.

---

contract:   an owed step has a named owner and a named trigger at the moment it
            is created, or it does not exist
observed:   VIOLATED TWICE in this project, both times as a side effect of a
            decision that was itself correct.
            - The #5399 base retarget (correct, operator-instructed, produced a
              readable diff) manufactured an un-stacking obligation that no desk
              owned and that lived only in one agent's head.
            - The #5293 closeout was routed to "the M119 desk". That desk no
              longer exists. The closeout has sat unowned since.
            In both cases the WORK was fine and the EDGE was untracked.
enforced:   NONE today. Issued as a project-wide rule by the project owner in
            `A-005` (2026-08-27) and carried into every brief this desk writes.
            Nothing mechanically checks it, so it is a rule and not yet a gate.
            The natural instrument is the ledger's unit table: an owed unit with
            an empty owner or an empty trigger column is a detectable defect,
            which makes this a candidate for A1.6's reconcile mechanism rather
            than a permanent honour system.
note:       the failure class is "an obligation created as a side effect of a
            correct local decision". It is not caused by bad decisions, which is
            why review does not catch it — every individual step passes.

---

contract:   a dependency must appear in a milestone map, not only in a base-ref
observed:   VIOLATED. M6's 27-of-28 dependency advance was gated by PR #5402,
            whose issue #5401 carried NO MILESTONE AT ALL, as did #5404. The
            dependency existed only as a `baseRefName` on a draft PR and was
            visible only to the two lanes that happened to share it — invisible
            to every desk including the project owner's, which is why it arrived
            there as a discovery rather than as a plan.
enforced:   NONE. Remedied for this instance by `A-005`: #5401 stamped into
            milestone #118 (verified; #5404 deliberately left unmilestoned and
            confirmed untouched). The general check does not exist.
note:       the ruling's reasoning is worth keeping as precedent — a milestone
            should own work by WHAT MAKES IT NECESSARY, not by what it
            RESEMBLES. "Drop iohk-monitoring" resembles M7 technical debt; it
            belongs to M6 because the `contra-tracer` conflict that makes it
            urgent exists only because of M6's outcome. Filing by resemblance
            would have made M6 blocked across a milestone boundary by a desk
            that does not currently exist.

---

contract:   fixing what a review asked for does not ANSWER the review
observed:   VIOLATED. RE-ANCHORED 2026-08-27 on GitHub API state, deliberately
            NOT on a local journal — see the two-writer contract below.
            LOAD-BEARING, and untouchable by any writer on this host:
              reviewDecision   = CHANGES_REQUESTED
              mergeStateStatus = BLOCKED   (mergeable = MERGEABLE)
              review           = disassembler CHANGES_REQUESTED 2026-08-26T16:47:24Z
              pending request  = paweljakubas — NOT disassembler
            Control: PR #5399 returns reviewDecision=REVIEW_REQUIRED on the
            same query, so the field discriminates rather than returning a
            constant.
            CORROBORATING ONLY: the tracing lane's 138-line journal mentions no
            human review and records itself "ready-for-review" — it answered
            Copilot and never saw the human. That observation rests on a runtime
            root with TWO WRITERS and is the weaker half; it is retained as
            corroboration and must never be the sole support for the claim.
enforced:   NONE. reviewDecision and mergeStateStatus are both machine-readable
            on the PR, so this one is genuinely cheap to check and nothing
            checks it.
note:       the consequence was structural, not cosmetic: because #5399 is
            stacked on #5402, one unanswered review was the blocker on the whole
            milestone's dependency advance while both lanes believed they were
            waiting on CI. A lane that reads its own CI but not its own reviews
            will report GREEN and be BLOCKED.

---

contract:   a claim must not rest on a control record that has two writers
observed:   VIOLATED, and caught while depending on it rather than afterwards.
            This desk asserted "the tracing lane never saw the review" on the
            strength of that lane's STATUS.md — a runtime root held
            simultaneously by the epic owner and by an unowned codex-raw pane,
            later confirmed to be the operator's own console.
enforced:   NONE mechanically. REMEDIED for this instance: the claim is
            re-anchored on GitHub API state, which no local writer can touch,
            and the journal observation is demoted to corroboration.
note:       the general rule, and it is cheap: when a claim depends on a local
            record, ask WHO ELSE CAN WRITE HERE before citing it. If the answer
            is not "exactly one agent", find an external anchor for the
            load-bearing half. Ownership of the second writer is irrelevant —
            that pane's work turned out to be correct AND helpful, and the
            record was still weakened by its presence.

---

contract:   an instrument must be shown to see EVERY spelling of what it counts
observed:   VIOLATED, twice in one exchange, in opposite directions.
            - The bump epic counted deprecation suppressions with a grep that
              knew `-Wno-deprecations` and reported 4. The true count is 7; it
              was blind to `-fno-warn-deprecations`, a legacy alias used in 3
              files ON THE DAY IT RAN.
            - This desk reported 7 and was credited for it, but its pattern
              caught the 3 alias files through an incidental second clause
              (`OPTIONS_GHC.*deprecat`), not because the alias was known. Right
              number, wrong reason.
enforced:   NONE. Specified for the suppression ratchet: MAX=9, counting the
            union of both spellings, with a control proving all 7 pre-existing
            sites are found. A single-spelling ratchet would be born blind to 3
            of 7 and could never rise when someone used the alias — a criterion
            that cannot fail, which is the defect this milestone has now paid
            for three times.
note:       THE SHARP EDGE, and it corrects something this desk believed: a
            NEGATIVE CONTROL proves an instrument CAN RETURN ZERO. It does NOT
            prove the instrument's REACH. Those are different controls and only
            the second was load-bearing here. Both count discrepancies today
            were reach, and neither was a false claim — so when two counts
            disagree, COMPARE THE INSTRUMENTS BEFORE CREDITING EITHER NUMBER.

---

contract:   "another milestone OWNS this" and "another milestone has DECIDED
            this" are different claims; only the second can be built against
observed:   VIOLATED. The escalation correctly diagnosed a cheap seat inventing
            a parallel abstraction (`BodyContent`), then proposed pointing it at
            `EraValue Read.Tx` "per #5241" as M1's decided model. #5241 is OPEN,
            last touched 2026-07-29, one comment, and its own body says "If we
            decide not to take this refactor, close as not planned" — a
            "Consider…" issue nobody has ruled on.
enforced:   NONE. Remedied by ruling the M1 front GRANTED IN PRINCIPLE AND
            UNSTAFFED: staffing it against an undecided model would reproduce
            the identical failure one layer up — a bespoke boundary model with a
            better name and a milestone's authority behind it.
note:       the diagnosis was right and the remedy was unavailable, which is a
            distinct failure from being wrong. The falsifiable fence that needs
            no design decision: IF RETIRING A DEPRECATION SITE REQUIRES A NEW
            TYPE TO MODEL THE TRANSACTION-BODY BOUNDARY, IT IS M1 ARCHITECTURE —
            STOP AND ESCALATE; if it can be done with existing ledger converters
            and no new boundary type, it is mechanical and may proceed.
            `BodyContent` fails that test on sight; a direct fromCardanoTxIn ->
            fromShelleyTxIn swap passes it.

---

contract:   a census over BUILD OUTPUT measures what was rebuilt, not what exists
observed:   VIOLATED. The bump epic enumerated cardano-api deprecation sites from
            the output of an INCREMENTAL `cabal build all`. It compiled 152
            modules; `Cardano.Api.Gen` was not among them — cabal skipped it as
            up to date, GHC never re-examined it, and it emitted nothing. The
            log contained zero occurrences of `Gen.hs`. The reported "complete
            enumeration of all 12 sites" was in fact "all sites in modules that
            happened to need recompiling". CI found the 13th.
enforced:   NONE. Remedied for this instance by a CLEAN full-tree build
            (`--flags=release`, -O2 -Werror) before the gate is believed, and by
            counting suppressions from the TREE (git grep, both spellings)
            rather than from build output.
note:       this is a third distinct failure mode alongside reach and false
            values. The instrument worked; the compiler was correct; the LOG WAS
            COMPLETE FOR WHAT RAN. The universe was smaller than the claim, and
            nothing in the output said so. Any enumeration derived from
            incremental build output inherits this defect silently.

---

contract:   an "each / all / every" claim carries the enumeration that produced
            it, and that enumeration carries its own control
observed:   VIOLATED, and notably INSIDE a disclosure of the same class of
            error. The epic reported "Each comment reads … each cites #5237"
            while disclosing its reach and scope failures. Verified at branch
            `9733926`: `Wallet.hs` and `TransactionsNew.hs` cite #5237;
            `lib/cardano-api-extra/lib/Cardano/Api/Gen.hs` contains NO comment
            and NO issue number at all. Two sites were checked and generalised
            to "each".
enforced:   NONE. The aggravating detail: that pragma sits in a block of five
            pre-existing suppressions, so an uncommented one there is CAMOUFLAGED
            — indistinguishable from prior debt and invisible to a reader not
            already hunting for it. The other two sit alone and explain
            themselves.
note:       three desks made a universe error today in three different
            instruments — one grep blind to a spelling, one census blind to
            unrebuilt modules, one self-report generalising from a known subset.
            None returned a FALSE value; all three returned TRUE values about a
            SMALLER UNIVERSE than claimed. That is the transferable artefact.

---

contract:   unread output is unmeasured output
observed:   VIOLATED. The epic passed `-Wno-error` to its build; GHC rejected it
            as unrecognised **180 times**, in the very output being collected as
            evidence, and those lines were never read.
enforced:   NONE. This is worse than reach or scope: the instrument SAID it was
            not doing what was believed, in output already in hand.
note:       a flag that silently does nothing turns every downstream "clean" into
            a claim about a build never configured the way the claimant thought.
            Collecting output is not reading it, and evidence not read is not
            evidence.

---

contract:   a gate is trusted only if it runs one of the project's SANCTIONED
            build paths, and NAMES WHICH ONE in its receipt
observed:   VIOLATED. This project has exactly two sanctioned build paths and
            both enforce -Werror, by different mechanisms:
              - CI / nix: the per-package `release` flag (nix/haskell.nix:277-278,
                `flags.release = true`), where each .cabal declares
                `if flag(release) ghc-options: -O2 -Werror`
              - `just build`: a COMMAND-LINE `--ghc-options="-Werror "`
                (justfile:51 and :61), applied on top of -O0
            EITHER would have caught the L0 deprecation. The acceptance gate used
            NEITHER — a bare `cabal build` plus a `-Wno-error` GHC rejected as
            unrecognised 180 times, unread. It was not a weaker version of a
            sanctioned gate; it was A THIRD BUILD PATH THAT EXISTS NOWHERE IN
            THIS REPOSITORY'S TOOLING, so nothing else in the project produces a
            comparable PASS and nobody could know what its PASS meant.
enforced:   NONE. "I ran cabal build with some flags" is not a gate identity.
note:       the aggravating and EXCULPATING detail, both at once: the release
            flag is `default: False, manual: True`. THE GATE'S BLINDNESS WAS THE
            DEFAULT STATE — nobody switched it off. Anyone who builds without
            explicitly opting in gets a build that cannot fail on any warning
            class. Not an exotic misconfiguration; it is what you get by not
            thinking about it. Topology already documented in issue #5400
            (M7): https://github.com/cardano-foundation/cardano-wallet/issues/5400

---

contract:   the RED-PROOF must be in the SAME CLASS as the criterion
observed:   VIOLATED in the specification of every ratchet briefed today. The
            standing red-proof was "introduce a throwaway X, show RED, remove
            it, show GREEN". #5399's gate COULD fail — on compile errors — and
            COULD NOT fail on deprecations. A red-proof on an easy trigger
            licenses NOTHING about the class the criterion actually claims.
enforced:   NONE. Now binding on all three instruments: the Dijkstra census
            ratchet, the suppression ratchet, and M1's closure instrument.
            - a CLOSURE gate is proven red by a CLOSURE violation — a transitive
              dependency, not an import line;
            - a DEPRECATION ratchet is proven red by a DEPRECATION, under the
              flags that make deprecations fail;
            - the Dijkstra census ratchet already satisfies this: its red-proof
              introduces a Dijkstra stub, which is the criterion's own class.
note:       this is the sharpest generalisation of the whole day. "Prove it can
            fail" is necessary and NOT sufficient — proving it can fail AT
            SOMETHING is not proving it can fail AT THE THING CLAIMED.

---

contract:   a ratchet MEASURES its own MAX at land time; it does not inherit a
            number from a report
observed:   PRE-EMPTED. The suppression MAX moved 9 -> 10 while nobody was
            building the instrument, purely because a third site was discovered.
            A MAX hardcoded from any report is a number that WAS TRUE ONCE.
enforced:   NONE. Specified: the instrument measures the count itself at land
            time, across BOTH spellings (-Wno-deprecations and
            -fno-warn-deprecations), and the PR shows the measurement rather
            than asserting a figure.

---

contract:   "one commit per sublibrary, never revisited" is verified PER PATCH,
            at each patch's own commit — never by a build of the tip
observed:   VIOLATED for seven ladder passes' worth of defects, and the earlier
            "26 of 28 clean" was itself an over-claim produced by this error.
            A TIP BUILD REPORTS GREEN WHILE A PATCH LOW IN THE STACK IS BROKEN,
            BECAUSE THE TIP CARRIES THE LATER FIXES. Only popping the stack and
            building each patch's own package at its own commit tests the claim
            the structure actually makes.
enforced:   Verified for #5399 by `stg pop -a` then pushing one patch at a time
            and building that patch's package with `--flags=release`: 9 packages
            OK, 0 failing. Positive control: a deliberately bad import injected
            into `faucet` fails the same harness (CTL=1), then reverted — so the
            green can fail.
note:       the ladder found SIX root causes a tip build could not: the void L0
            acceptance; the IsValid -> IsPhase2Valid TYPE change (not a rename —
            two nullary constructors); the coin-selection SRP; four redundant
            foldl' imports introduced by the lane's OWN L4 patch; MonadThrow
            gaining annotateIO; and two further deprecation files. Two of the
            five suppressions were visible to NO earlier instrument.
            The general form: A CLAIM ABOUT EVERY COMMIT CANNOT BE TESTED AT ONE
            COMMIT.

---

contract:   THE SCOPE OF THE EVIDENCE MUST MATCH THE SCOPE OF THE CLAIM — in
            CLASS, and in GRANULARITY
            (bound project-wide 2026-08-27. This is the UNION of two entries
            above, not a third rule: `the red-proof must be in the same class as
            the criterion` and `a claim about every commit cannot be tested at
            one commit` are the SAME rule on two different axes.)
observed:   VIOLATED on both axes in one afternoon, by two different desks.
            - CLASS: #5399's acceptance gate could fail on compile errors and
              could NOT fail on deprecations, so its PASS licensed nothing about
              the class the criterion claimed. L0's acceptance was void.
            - GRANULARITY: a build of the TIP reported "26 of 28 clean" while a
              patch low in the stack was broken, because the tip carries the
              later fixes. This desk reported that number upward and withdrew it.
              "One commit per sublibrary, never revisited" is a claim about
              EVERY commit, so only `stg pop -a` plus a per-patch build could
              test it — and doing so surfaced SIX root causes no tip build could
              see, including four redundant imports the lane's OWN L4 patch
              introduced.
enforced:   NONE mechanically. Binds the Dijkstra census ratchet, the suppression
            ratchet, M1's closure instrument, and every acceptance in this
            project.
note:       THE SHAPE TO HUNT: both failures produce a PASS WHOSE SCOPE IS
            NARROWER THAN THE SENTENCE IT IS USED TO JUSTIFY, and both are
            invisible PRECISELY BECAUSE THE PASS IS REAL. Nothing is broken, no
            value is false, no check is skipped — the evidence simply does not
            reach as far as the claim. Class separation in an instrument is the
            same rule a third time: a suppression ratchet that counted an
            unrelated pragma class would be measuring something other than what
            it claims.

---

contract:   `reviewDecision` is a LATCH, not a signal — re-check reviews BY
            TIMESTAMP
observed:   VIOLATED by this desk, repeatedly, in reports going upward. #5402 was
            reported for two days as "CHANGES_REQUESTED, the lane is working the
            findings". Verified by timestamp:
              2026-08-26T15:06:58Z  copilot        COMMENTED
              2026-08-26T16:47:24Z  disassembler   CHANGES_REQUESTED
              2026-08-27T09:05:43Z  re-request to disassembler  <- POSTDATES both
              since the re-request: 0 issue comments, 0 review comments
            So the flag is STICKY FROM 08-26 and says nothing about whether the
            reviewer responded. Every item was addressed and pushed on 08-27.
            THE LANE OWED NOTHING; IT WAS WAITING. Caught by the tracing lane
            correcting its parent, not by the parent.
enforced:   NONE. `reviewDecision` and `mergeStateStatus` are both latched fields
            and both are machine-readable, so the timestamp comparison is cheap
            and nothing does it.
note:       this is the WEEK'S OWN RULE in a new disguise: a true value whose
            SCOPE is narrower than the sentence it is used to justify.
            CHANGES_REQUESTED is true; "the reviewer wants changes" is true of
            08-26; "the reviewer has seen the fix" is NOT entailed and was
            implied by every report this desk wrote. A latched field answers
            "what was the last verdict", never "has anything happened since".

---

contract:   a SCOPED measurement answers the SCOPED question — reusing its output
            for the unscoped question is a different claim with no evidence
observed:   VIOLATED BY THIS DESK, 2026-08-30, while correcting a subordinate's
            evidence label. Checking whether a stale build rev invalidated the
            wire-format campaign, this desk ran
              git diff --stat 346786a112..origin/master -- cabal.project flake.nix flake.lock
            SCOPED TO THREE FILES. Only flake.nix matched, and the desk then
            described THE WHOLE 8-COMMIT DELTA as "one flake.nix change adding a
            delegation-agda check". The true delta is 25 FILES, 2055 INSERTIONS,
            43 DELETIONS. Caught by the glm investigation seat, which corrected
            its parent in the same breath as CONFIRMING the parent's conclusion.
enforced:   NONE. Remedied for this instance: the correct evidence is the seat's
            — no delta file touches network-layer, cabal.project, genesis or
            codec surface, verified by diff over all 25 paths.
note:       THIS IS A DISTINCT VARIANT AND HARDER TO CATCH THAN ORDINARY REACH
            FAILURE. The scoping was CORRECT for the question actually being
            asked — does the dependency surface change? It does not, and that
            answer is sound. The instrument was right; only the SENTENCE was
            wrong, because the narrowing was chosen deliberately and then
            forgotten when the result was written up. Nothing in the output
            announces that it is scoped.
            The conclusion SURVIVED and the evidence DID NOT: this desk asserted
            a SMALL delta; the real argument is a LARGE DELTA THAT IS IRRELEVANT.
            Those are different arguments for the same conclusion, and only one
            of them was true. RIGHT ANSWER, INSUFFICIENT EVIDENCE.

---

contract:   an observation on a state page is not an owed step — only a row with
            an OWNER and a TRIGGER is
observed:   VIOLATED BY THIS DESK, and found by the operator rather than by the
            desk. On 2026-08-27 the state page recorded that #5397/#5399's title
            described "two SRP pin bumps" while the branch carried a 28-package
            node 11.1.0 advance, and stated that re-cutting it was BINDING and
            must happen "before the PR leaves draft". THE PR LEFT DRAFT WITH THE
            TITLE UNCHANGED and stayed that way for four days. Neither named pin
            appears in the branch's 19 commits.
enforced:   NONE. Remedied 2026-08-31: both #5399 and #5397 re-cut to "Advance
            the dependency pin set to cardano-node 11.1.0", closing link verified
            intact after the edit.
note:       this is THIS MILESTONE'S OWN RULE APPLIED TO EVERYONE EXCEPT ITSELF.
            The owed-step rule — a named owner and a named trigger at the moment
            the obligation is created, later extended to require a named OBJECT —
            was bound here after losing the #5399 un-stacking and the #5293
            closeout. It was then applied to the base retarget, the suppression
            removal, the MAX declaration and the M1 front. THREE OWED UNITS WERE
            REGISTERED AT THIS DESK AND THE TITLE RE-CUT WAS NOT ONE OF THEM,
            because it was written as a SENTENCE in a state page rather than as a
            ROW in a unit table. A sentence describing an obligation reads exactly
            like a record of one; only the table distinguishes them.
