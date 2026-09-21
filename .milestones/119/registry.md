# Contract registry — cardano-wallet technical-debt cleanup (#119)

Snapshot 2026-08-24. Entries for lanes that have merged and closed are removed,
not archived — the past has other owners.

contract:   scope quarantine from Drop cardano-api
parties:    technical-debt desk, GitHub milestone #113 desk/workstream
invariant:  this desk never dispatches, edits, closes, or merges work whose
            purpose is cardano-api removal
enforced:   NONE beyond the desk ledger and a forbidden-scope line in every
            worker brief; no automated check

contract:   milestone identity
parties:    shared milestone registry, GitHub milestone #119, singleton desk
invariant:  M7 Technical Debt Cleanup resolves to session `wallet`, window
            `cardano-wallet-ms119-technical-debt`, and never aliases #113
enforced:   shared `/code/llm-settings/shared/milestones.md` entry plus the
            GitHub milestone object #119; no automated cross-check.
            2026-08-24: the GitHub milestone counter, recorded stale at 0/0 on
            08-02, has self-healed to 8 closed / 2 open.

contract:   milestone boundary against #5350
parties:    technical-debt desk, cardano-wallet project owner, the #5350 lane
invariant:  issue #5350 / PR #5363 is a project-direct standalone ticket owned
            by the project owner through its own ticket owner; this desk does
            not take it over, dispatch into it, instruct it, inspect it for
            control, or count it in the milestone map, burn-down, or state page;
            a believed contract interaction is filed as a Q-file, never acted on
enforced:   project-owner ruling 2026-08-24 (NOTE-001), this ledger, the #5326
            worker brief's forbidden-scope section, and the deliberate omission
            of wallet:3 from session.md. No automated check — and on 2026-08-24
            an untargeted tmux query pointed a child at that lane's pane, which
            is exactly the failure this boundary must survive.

contract:   merge authority and ordering
parties:    ticket lanes, milestone desk, human operator
invariant:  lanes may push and open draft PRs; the desk verifies and proposes;
            only the operator merges, one green proposal at a time
enforced:   brief/STATUS protocol plus GitHub branch protection; no
            repository-side ordering check

contract:   master acceptance
parties:    PR branch and protected master
invariant:  a proposal is not green until every required check succeeds and
            required review is satisfied, unless the operator explicitly
            authorizes proceeding and the desk proves the sole residual red is
            infrastructure-only with no pending checks
enforced:   GitHub branch protection; the desk independently counts the
            exact-head rollup and records any operator-authorized exception

contract:   Pawel semantic review
parties:    milestone desk, ticket owners, Pawel, operator
invariant:  a prepared candidate states up front whether Pawel must review it;
            after accepted delivery a required Pawel review is recorded as
            AWAITING-EXTERNAL-REVIEW rather than BLOCKED, and reaching that
            state counts as delivery success, not as a blocker
enforced:   ticket body plus this ledger. Currently live for **#5326**, which
            changes production shutdown and wallet-worker lifecycle semantics
            and must stay open for Pawel's review of the exact accepted head.
            No automated check that a candidate declares its review need.

contract:   authoritative seat families and alternation
parties:    milestone desk, every ticket owner, commit owners, auditors
invariant:  standing authoritative set is {claude, codex, grok}; the
            commit-owner family differs from the ticket-owner family and the
            auditor family differs from the commit-owner family; ticket-owner
            seats stay on a metered family (claude or codex); grok is pinned to
            grok-4.6, capped at one seat per ticket, and barred from any seat
            touching production secrets; agy is REVOKED 2026-08-14; qwen is
            draft-only and never occupies an authoritative seat
enforced:   PARTIAL — `alternate-authoritative-cli` derives the edges
            mechanically and the desk records the derivation at dispatch, and
            since 2026-08-24 the desk reads /proc/<pane_pid>/cmdline at the
            dispatch barrier to confirm the launched process matches. That
            check is manual and per-dispatch, not automated.
            Supersedes the old "provider budget boundary" entry, which still
            claimed only Codex and Claude may own or execute tickets.

contract:   worker identity binding
parties:    every orchestrator that splits a pane, every worker that reports one
invariant:  a seat derives its own pane id from $TMUX_PANE (or
            `tmux display-message -p -t "$TMUX_PANE"`), never from an untargeted
            `tmux display-message -p`, and every tmux command carries an explicit
            -t target; a parent rejects any START whose reported pane or CLI
            family does not match the launch it performed
enforced:   NONE — found by hand on 2026-08-24 when the #5326 owner reported
            pane=%38 (the unrelated #5350 lane) because the operator's client
            happened to be focused there. Damage was nil and the hazard was
            prospective: a split against %38 would have placed a grok commit
            owner inside another milestone's window. The rule is written in
            tmux-orchestrator and in worker briefs; nothing mechanically stops a
            worker from asking the wrong question. Candidate for a commissioned
            check — a dispatch-time assertion comparing the worker's claimed
            pane against the pane id the parent recorded at split time.

contract:   protected untracked state in the shared master checkout
parties:    every lane whose process cwd is /code/cardano-wallet, the operator
invariant:  /code/cardano-wallet/.llm/issue-5309-unit-memory-analysis.md is
            preserved by explicit operator order; it is untracked and has never
            been committed, so it is unrecoverable from git; no lane runs
            git clean / stash / checkout -- / reset --hard / restore, or a
            cabal|nix clean, with that directory as cwd
enforced:   NONE — brief and inbox fences only, plus the desk's recorded
            sha256 71e6dee8a639fece22712f69cd794c0340d64de17ac8cc11e37ebe049cdfc1d5
            as a tripwire to detect loss after the fact. Detection, not
            prevention. The durable fix is for lanes never to have the shared
            checkout as cwd; today the launch line still points there for
            bootstrap.

contract:   #5350 formal guarantees must survive the Lean migration
parties:    the #5350 lane (project-direct, PR #5363), the M119 Agda→Lean
            migration epic, Pawel/operator as reviewers of both
invariant:  the four laws AGDA-5350-EMPTY / -LAST / -HISTORY / -SAME, the
            explicit DRep-equality assumption, and the six registered
            QuickCheck properties (prop_effectiveDelegationStatusEmpty,
            prop_effectiveDelegationStatusLast, prop_voteDecisionIgnoresHistory,
            prop_joinDRepVotingActionEffective, prop_joinDRepParityWithVoteRequest,
            prop_drepEqualityMatchesStructure) survive the migration to Lean
            present, registered, non-vacuous, and unweakened; a LEAN-5350-*
            alias is permitted but the migration ledger retains the original IDs
enforced:   NONE — and worse than unenforced, the referent does not yet exist.
            Verified 2026-08-24 on master: all six properties return ZERO files;
            they live only in PR #5363, which is OPEN and unmerged (head
            b7e82488e9, +345/-1 DelegationSpec.hs, +245/-4 Delegation.agda).
            So this epic's binding contract points at artifacts not in the
            codebase, and the party that owns them (#5350) is a project-direct
            lane this desk is explicitly fenced from. Consequence: epic Child 2
            has an unstated hard dependency on PR #5363 merging, and if #5363
            merges in altered form the contract's content changes underneath the
            epic. Escalated to the project owner rather than resolved here,
            because only it can speak for the #5350 lane.

contract:   the repository's Lean gate must actually gate
parties:    M119 Agda→Lean migration epic (Child 1), repository CI
invariant:  the pinned Lean toolchain check runs on master, covers every tracked
            Lean specification, and is proven able to fail via a retained
            negative control
enforced:   NONE — and the existing workflow is a textbook check that cannot
            fail. Verified 2026-08-24: .github/workflows/lean.yml triggers on
            push to master but is gated by paths ['specifications/**.lean'], and
            no master push has ever touched such a file. `gh run list
            --workflow lean.yml` returns exactly ONE run in the workflow's entire
            history — 2026-02-13, manual workflow_dispatch, on branch
            fix/cleanup-buildkite, never master. Issue #5340 diagnosed precisely
            this and was closed COMPLETED 2026-07-28 by removing the stale badge
            rather than by making the gate run. The epic draft's sentence "the
            repository already has a Lean project and CI workflow" is literally
            true and materially misleading: there is today no working Lean gate
            at all. Child 1 is therefore load-bearing in a way the draft
            understates, and its negative-control clause is the acceptance
            criterion that matters most.

---

## 2026-08-25 re-derivation — two `enforced: NONE` entries upgraded

Both entries above were filed honestly as `NONE` on 2026-08-24 when the
referents were not on master. **PR #5363 merged** (`ab51934b7b`, parents
`346786a112` + audited head `b7e82488e9`, no rewrite; #5350 closed). Re-derived
from master directly rather than from a relayed paragraph:

contract:   #5350 formal guarantees must survive the Lean migration  [SUPERSEDES the NONE entry]
parties:    master (post-#5363), the Agda→Lean migration epic #5389, child #5391
invariant:  the four laws, the DRep-equality assumption, and the six registered
            QuickCheck properties survive present, registered, non-vacuous, unweakened
enforced:   **STRONGLY, and better than expected.**
            (a) CI job `delegation-agda` (`.github/workflows/ci.yml`,
                `flake.nix`, `nix/delegation-agda.nix`) typechecks the model and
                then applies ONE DELIBERATE MUTATION PER NAMED LAW, requiring
                the typecheck to FAIL each time — EMPTY (`= a`→`= Inactive`),
                LAST (`lastStatus t ts`→`s`), HISTORY (`effectiveDelegationStatus`
                →`activeStatus`), SAME (`SameVote`→`DifferentVote`). It asserts
                the mutation actually changed the file and errors with
                `$law mutation still typechecks` if the check stops being able
                to fail. **Proven able to fail on every CI run.**
            (b) All six properties are REGISTERED, not merely defined, in
                `lib/unit/test/unit/Cardano/Wallet/DelegationSpec.hs`
                (e.g. `it "AGDA-5350-EMPTY: …" $ property prop_effectiveDelegationStatusEmpty`).
            (c) Merged-form deltas reach this desk through the project owner,
                which holds the #5350 lane; this desk has no channel to it.

contract:   the repository's Lean gate must actually gate  [UNCHANGED — still NONE]
enforced:   NONE, and the 2026-08-24 finding stands: `lean.yml` has exactly one
            run in its entire history (2026-02-13, manual dispatch, non-master),
            path-filtered to `specifications/**.lean` which no master push has
            ever touched. #5340 closed it by removing the badge.

contract:   enforcement parity across the Agda→Lean migration   [NEW]
parties:    epic #5389 and its children #5390–#5394
invariant:  the migration must not reduce enforcement. The Lean gate must run on
            master and carry a per-law negative control at least as strong as
            today's `delegation-agda` mutation harness; the Agda harness is
            removed (#5394) only after the Lean equivalent is green AND shown
            failing on mutation
enforced:   PARTIAL — written into the epic body and into the acceptance criteria
            of #5390, #5391 and #5394 as explicit "shown able to fail" clauses.
            No machine check yet; the guard that would close this is the Lean
            negative control #5390 must build. Until #5390 lands, this is the
            single most important unenforced contract in M119: the migration's
            natural failure mode is landing a Lean check that typechecks, cannot
            fail, and looks like progress while being a strict regression.

contract:   evidence method controls — both directions   [NEW, project-wide 2026-08-25]
parties:    every M119 lane, every brief this desk writes, epic #5389 and its children
invariant:  a check must be shown able to FAIL before it is trusted to pass, and
            a search must be shown able to FIND before its zero is trusted as an
            absence. Before reporting an absence, run the exact method against
            something known to contain matches and show the hits; then show the
            zero; record both.
enforced:   PARTIAL, and honestly so. Written into the acceptance criteria of
            filed tickets #5389 and #5394 (whose two load-bearing criteria are
            absence claims) and into this desk's standing brief content. No
            machine check — nothing stops a future lane from publishing a zero
            it never controlled. The guard that would close this does not exist
            and probably cannot be generic.
            Origin: on 2026-08-25 a dependency question had three independent
            methods available; `cardano-base` git tags 404 for a real version
            identically to a bogus one, and a raw.githubusercontent CHaP
            `_sources` path failed its own positive control outright. TWO OF
            THREE METHODS WOULD HAVE CONFIRMED A FALSE ANSWER. Only a positive
            control on a known-present item separated the finding from a
            fabrication. This desk's own CHaP visibility work had carried a
            4.0.2 positive control, which is the only reason its 4.0.8 zero was
            trustworthy.
            Dual of the negative control: negative proves the check CAN FAIL and
            catches a guard that always passes; positive proves the search CAN
            FIND and catches a zero that means "broken method", not "absent".
