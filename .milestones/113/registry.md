# Contract registry — M1 (#113) Drop cardano-api

Inherited c1/c2/c3 from `.milestones/1/registry.md` (the 2026-07-29 project-altitude
desk). Only **c1** is M1's; c2 and c3 are recorded here as *observed* and belong
to the project owner or another milestone. Every `enforced: NONE` is either
commissioned or explicitly waived — never silent.

---

contract:   **m1-closure** — no production library under `lib/` depends on
            `cardano-api`, in the Cabal/Nix dependency **closure**
parties:    every `lib/*` production library (`wallet`, `api`, `primitive`,
            `network-layer`, …) and the CI build
invariant:  the closure for wallet library components contains no
            `cardano-api`; the count may fall, may not rise, final state 0
enforced:   **NONE — this is the milestone's first deliverable.** Commissioned
            as the closure ratchet, drafted and project-reviewed 2026-09-04,
            **awaiting operator review**; no lane dispatched.
            **Row re-specified 2026-09-04 (D-91, D-92):** the drafted check
            measured **direct** `build-depends` edges while this contract's
            subject — stated correctly above since day one — is the **closure**.
            `MAX` 9 → **12**, an instrument correction and not a raise. Four
            `lib/*` packages were already transitive-only at `803e376bae`, so a
            direct-edge counter **reaches zero while this invariant is false**.
            Definition adopted: **library-only transitive closure**, matching
            this contract's own words *"production library"*.
            Until it lands, the milestone's own acceptance criterion has no
            enforcing check, which is precisely the state this registry exists
            to make impossible to hold unknowingly.

contract:   **m1-pin-deletable** — the `cardano-api` pin can leave
            `cabal.project` only when **no** `lib/*` stanza of any kind reaches
            `cardano-api`
parties:    M1 (drives the count down as a side effect), and **whoever removes
            the pin** (owner — NOT M1)
invariant:  the any-stanza `cardano-api` closure over `lib/*` may fall, may not
            rise; **final state 0**. Baseline **13** at `803e376bae`
enforced:   **NONE — commissioned 2026-09-04 by project ruling (D-96)** as
            `closure-any`, the ratchet's third row. **Deliberately not an M1
            acceptance criterion**, and M1 acceptance must not wait on it.
            It exists because `m1-closure` = 0 can be **true of the milestone and
            false of the repository**: `cardano-wallet-benchmarks` names
            `cardano-api` at `benchmark db`:188 and `benchmark api`:229, so the
            pin stays even when every production library is clean. Ratcheted so
            the number cannot silently rise and the pin's remaining blockers stay
            visible to whoever eventually owns them.

contract:   **m1-suppressions** — `cardano-api` deprecation suppressions are
            temporary and retire with the dependency
parties:    M6 (#5399 adds them to unblock its bump), M1 (owed their removal)
invariant:  the deprecation-suppression count may fall, may not rise; final
            state 0, retiring together with m1-closure
enforced:   **NONE — commissioned; unblocked 2026-08-28 by A-001.** Ruled: the PR
            that changes the count changes MAX in the same PR. Row 2 lands on
            `master` with row 1; #5399 raises to 7+N in its own diff; the
            deprecations ticket lowers it in its own diff. Landing order
            amended by FINDING-002. **Not waived.** Historical rate for
            temporary suppressions in this repository: **seven for seven
            permanent.** Both spellings in scope; an instrument knowing one is
            born blind to three sites.

contract:   **m1-m6-extra-stubs** — the five Dijkstra stubs in
            `lib/wallet/src/Cardano/Api/Extra.hs`
parties:    M6 (implements them; holds a census gate counting them), M1
            (deletes the module, which lowers M6's census by 5)
invariant:  the census count falls only by **deletion or implementation**,
            never by two desks agreeing the stubs no longer count; the census
            denominator is **not** narrowed to exclude `Extra.hs`
enforced:   **partial** — M6's census gate counts them (#5407, DRAFT). The
            *denominator guard* itself is a standing ruling, not a check: no
            mechanism prevents a future narrowing. Fence 2 is the M1-side
            enforcement and it is a human interlock at `%107`, not code.

contract:   **m1-tx-bytes** — transaction bytes and DB compatibility preserved
            across every migration in this milestone
parties:    wallet tx layer, deployed wallets, previous-release DB snapshots
invariant:  migrated paths produce byte-identical transactions; previous-release
            database snapshot loading still passes (#5290 AC)
enforced:   **NONE at milestone level — candidate named AND now triggered.**
            No cross-ticket mechanism observes it. **Candidate check:** a
            golden-bytes fixture over the affected paths that a migration must
            reproduce, wired into the sanctioned build path.
            **NOT commissioned yet, deliberately:** neither ticket at the
            operator gate touches a byte-producing path —
            `buildCoinSelectionForTransaction` serialises nothing — and an
            instrument commissioned against a surface no current ticket
            exercises **has no red-proof available**.
            **Trigger (ruled by `%107` 2026-08-28, because a candidate with no
            trigger is the thing this project keeps losing):** the first M1
            ticket whose scope touches a path that **constructs or serialises a
            transaction**. It is commissioned in that ticket, with a same-class
            red-proof — mutate a byte-producing path, show the fixture RED.
            **Owner: the M1 milestone owner.** On today's map that is #5288 or
            #5289; it is neither ticket now in flight.

contract:   **c1 (inherited)** — tx-layer seam between cardano-api removal (M1)
            and ledger/node version bumps (M2/M6)
parties:    M1 arc (rewrites `Transaction.Ledger`, `SealedTx`, signing),
            M6 arc (bumps the pins the same modules build on)
invariant:  one arc rebases on the other by explicit decision; both lanes never
            mutate the tx layer concurrently
enforced:   **DECIDED 2026-08-28, still unchecked.** The parked arbitration is
            resolved: operator ruling *"we stack on that"* — **M1 rebases on
            M6.** The deprecations branch stacks on #5399's head; #5290 waits
            behind fence 2. No mechanism enforces the non-concurrency; it is
            fence 1 plus this desk's dispatch discipline.

contract:   **c2 — RE-HOMING TO M6 (#118), ruled by `%107` 2026-08-28.**
            Wallet tracks the mainnet protocol version. `enforced: NONE`,
            breach found by hand 2026-07-29 (mainnet on v11 since 2026-07-20
            while the wallet pinned node 10.x).
            Tracking the mainnet protocol version is **hard-fork readiness** —
            M6's outcome, not tech-debt or CI. It transfers as an **observed
            contract, not new work**: M6 inherits knowing it exists and that it
            is unenforced with a hand-found breach.
            **Retained here until M6 records it; `%107` will say when to remove
            it.** `%107` retired M2 on 2026-08-28 preserving its outcome and the
            #5381 inheritance but **not its contracts**, and `c2` survived only
            because a July desk wrote it down and this desk carried it into a
            different milestone's registry precisely so a transition would not
            lose it. It nearly was lost anyway. When M6 closes it re-homes again
            rather than lapsing — `%107` owns that step, trigger *"M6 accepted"*.

contract:   **c3 (observed, not M1's)** — `source-repository-package` pins target
            commits reachable from upstream `main`. `enforced: partial` — desk
            law with no CI check. **M6 owns the pin surface right now**; a slice
            of M1 needing a pin moved files a Q and parks.

contract:   **m1-ratchet-bidirectionality** — a ratchet MAX moves only in the
            diff that causes the count to move
parties:    every PR touching either row; M1 (owns the rows), M6 (#5399 raises
            row 2), every later child (lowers both)
invariant:  a count that rises without a declared raise in the same diff is RED;
            lowering MAX is part of every child's definition of done
enforced:   **NONE until the instrument lands.** This is the instrument's own
            central behaviour, and its acceptance criteria require it be
            *demonstrated*, not asserted. Ruled by `A-001` (project owner),
            mechanising M6 `NOTE-011` `RULING 1` condition 4. The known
            execution defect in its landing order is `FINDING-002`.

contract:   **no-verdict-status** — every check, gate, ratchet and probe in this
            project distinguishes a THIRD status from pass and fail
parties:    M1's un-stack orphan check; M6's Dijkstra census script; every
            future instrument in either milestone
invariant:  **only a cleanly distinguishable result is a verdict; everything
            else is "no verdict — retry", never a default to either answer.**
            **What is shared is the CONCEPT — a third status, never collapsed
            into pass or fail. There is NO shared number.** The encoding is
            per-instrument and must not collide with any status that instrument
            itself consumes. **The two instruments using different values is
            CORRECT, not drift.**
enforced:   **partial.** Two instruments implement it:
            • **M1's un-stack orphan check exits `75` (`EX_TEMPFAIL`)** on
              transport error, so a network blip cannot be read as
              merged-or-deleted.
            • **M6's census exits `2`** on instrument failure, so a broken
              instrument cannot be read as a clean count — **reported by `%107`,
              NOT verified by this desk**: #5407 carries only a docs commit, so
              the script is not on the remote.
            **Nothing prevents a future check from omitting the third status** —
            that is the unenforced half.
            Ruled project-wide by `%107` 2026-08-28, reached independently in two
            desks, which is why it is ONE contract with both instances named.
note:       **Why there is no shared number, recorded because the alternative was
            tried.** `%107` first ruled `2` as the shared status and **withdrew
            it**: `git ls-remote --exit-code` already uses `2` for **ref
            absent**, which M1's check maps to **exit 1 (trigger fired)**. A
            shared number would have put *"the ref is gone"* and *"I have no
            verdict"* on the same integer one layer apart, and the first
            simplification of the wrapper would have merged them.
            M1 uses **`75` = `EX_TEMPFAIL`** — *"temp failure; user is invited to
            retry"* — the standard code for exactly this status, not a spare number.
            **Citation: `sysexits.h`, `EX_TEMPFAIL = 75`** — standard, and
            present in **both glibc and musl** on every libc checked. (A nix
            store path was cited here first; a store path is version- and
            hash-specific, so it is **an evidence pointer that expires** — the
            same class as citing a file inside a `/tmp` runtime root, milder
            only because it expires to a reader rather than to a sweep.)
            **Never pass a probe's status through as a script's status, and never
            choose an encoding that already means something else in the layer you
            consume.**

contract:   **scope-by-structure-not-by-pattern** — a pattern-based instrument
            run over a document that discusses its own subject will match the
            discussion
parties:    M1's row-2 suppression instrument; M6's per-file verification
            script; every future instrument in either milestone that reads a
            document rather than a program
invariant:  **an instrument over a structured document parses the structure; it
            does not grep the body.**
enforced:   **partial — two instances, both found by accident, neither by a
            check.**
            • M1: a naive repo-wide grep returned **9** suppressions instead of
              7, because `ERA-CHANGES.md` and `TODO.md` quote the pragma in
              prose. Tightening the regex to match the pragma *shape* still
              returned 9 — **a regex cannot distinguish a pragma from a faithful
              quotation of one.** Fixed by scoping to file class (`*.hs`,
              `*.cabal`, `cabal.project*`).
            • M6: `grep -F <basename> | head -1` matched a prose mention earlier
              in a PR body instead of the table row, producing a **false
              MISSING**. Caught only because the script printed the raw table
              beside its summary; trusting its own summary would have sent an
              epic back to fix a defect that does not exist.
              (Reported by `%107`, **not verified by this desk**.)
            **Nothing prevents the next instrument from grepping a body.** That
            is the unenforced half, and both instances were caught by a human
            reading output rather than by any check.
note:       Two independent instances, two desks, two days — the same reason
            `no-verdict-status` is one contract and not two. The mitigation is
            the same in both: **scope by structure.** M1 scoped by file class;
            M6 must parse table rows.
