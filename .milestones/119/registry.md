# Contract registry — cardano-wallet technical-debt cleanup

contract:   scope quarantine from Drop cardano-api
parties:    technical-debt desk, GitHub milestone #113 desk/workstream
invariant:  this desk never dispatches, edits, closes, or merges work whose
            purpose is cardano-api removal
enforced:   desk ledger + every worker brief; no automated check

contract:   milestone identity
parties:    shared milestone registry, GitHub milestone #119, singleton desk
invariant:  M7 Technical Debt Cleanup resolves to session `wallet`, window
            `cardano-wallet-ms119-technical-debt`, runtime
            `/tmp/ms-cw-tech-debt`, and never aliases milestone #113
enforced:   shared `/code/llm-settings/shared/milestones.md` entry plus GitHub
            milestone object #119; no automated cross-check

contract:   provider budget boundary
parties:    technical-debt desk and every child lane
invariant:  only Codex or Claude may own or execute tickets; Claude is
            represented by Opus and Codex by Sol for released #5358: Sol desk,
            Opus ticket owner, Sol PAIR driver, Opus PAIR navigator; Grok and
            Qwen are review/advisory only and never authoritative
enforced:   release-v2 and worker briefs freeze the four-seat topology; desk
            verifies pane commands/models/effort before START and records any
            corrective reseat durably

contract:   merge authority and ordering
parties:    ticket lanes, milestone desk, human operator
invariant:  lanes may push and open draft PRs; the desk verifies and proposes;
            only the operator merges, one green proposal at a time
enforced:   brief/STATUS protocol; no repository-side automated check

contract:   master acceptance
parties:    PR branch and protected master
invariant:  a proposal is not green until every required check succeeds and
            required review is satisfied, unless the operator has explicitly
            authorized proceeding and the desk proves the sole residual red is
            infrastructure-only with no pending checks
enforced:   GitHub branch protection; desk independently counts the exact-head
            rollup and records any operator-authorized exception

contract:   milestone ownership boundary
parties:    technical-debt desk, Randomness Hardening milestone #116 / epic #5304
invariant:  findings on PR #5343 are routed through the #5304 epic owner; this
            technical-debt desk never edits, tests, rebases, or prompts the
            epic's ticket owner or pair directly
enforced:   durable desk ledger and reloaded role contract; live #5304 epic
            owner pane `%5199`, runtime `/tmp/epic-5304`, supervised only via
            its STATUS and resurrection fragment

contract:   standalone ownership for #5325 / PR #5345
parties:    technical-debt milestone desk, standalone ticket owner, operator
invariant:  because #5325 has no parent epic, the milestone desk supervises
            only its Codex ticket owner; that owner alone controls execution,
            branch corrections, rebase, tests and push; Grok and Qwen may
            review only; #5343
            remains the sole merge proposal until operator disposition
enforced:   milestone ledger and worker-protocol brief; no repository-side
            ordering check

contract:   Pawel semantic review
parties:    milestone desk, ticket owners, Pawel, operator
invariant:  prepared candidates identify whether Pawel must review them; after
            accepted delivery, required Pawel review is recorded as
            AWAITING-EXTERNAL-REVIEW rather than BLOCKED, and large candidates
            are split into reviewable semantic core and mechanical remainder
enforced:   ticket body plus milestone ledger; #5325 currently awaits Pawel at
            exact head 14842f1fbc14; #5303/#5343 also awaits Pawel; #5326
            requires Pawel after delivery; #5358 and #5359 explicitly do not
            require Pawel because they are mechanical test/CI guards
