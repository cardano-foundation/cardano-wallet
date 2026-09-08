Outcome: no production library under `lib/` depends on `cardano-api`, verified
in the **dependency closure**, with transaction bytes and database
compatibility preserved.

**The outcome is a verified closure, not an issue count.** Closing the issues in
this milestone is not the outcome, and a grep over imports is not the check: a
package can leave the closure while imports linger in comments, and can re-enter
it through a transitive path with no source file importing it at all.

## Observable test

A CI ratchet with **three rows**, and the acceptance criterion is their
**conjunction**, not any one of them:

- **row 1** — `cardano-api` in the Cabal/Nix dependency closure of the
  **production** library components. Final state **0**.
- **row 2** — the **named set** of deprecation suppressions, both
  `-Wno-deprecations` and `-fno-warn-deprecations`, each entry carrying its
  spelling, an **established** cause, and a disposition. Each disposition is backed by a build under a sanctioned path: the build fails
  and names the cause, or **succeeds — meaning the pragma suppressed nothing,
  and it is deleted rather than documented.** Final state is a residual of
  permanent and foreign entries, each with a build-backed cause — **not zero**.
- **row 3** — the **named set** of `lib/*.cabal` packages listing `cardano-api`
  in `build-depends`, compared against a checked-in allowlist and red on a
  difference in either direction. Final state is the **documented test-only
  residual**, each entry naming its follow-up — not necessarily zero.

Rows 2 and 3 ratchet sets rather than counts because their targets are nonzero,
and at a nonzero target a count admits substitution silently. Only row 1 targets
zero, where a count and a set coincide.

Every row may fall and may not rise: a PR that raises one declares the raise in
the diff that causes it.

A second row ratchets the temporary `cardano-api` deprecation suppressions —
both `-Wno-deprecations` and `-fno-warn-deprecations`, because an instrument
that knows one spelling is born blind to the other. The two rows retire
together.

## Scope boundaries

- Production libraries under `lib/`. Test, benchmark, integration and
  local-cluster consumers are a separate population, excluded explicitly rather
  than silently.
- No file under `lib/integration/**` is modified.
- Each PR is either additive or removes a fully migrated slice — bisect-safe.
- Migrating to `Cardano.Api.Experimental` is **out of scope and counter to the
  outcome**: it keeps `cardano-api` in the closure under another module name.
  The deprecation pragmas recommend it; this milestone does not follow them.

## Artifact

M1's outcome is a subtraction from the dependency closure, so it forks no
executable of its own. Whether the published CI ratchet result serves as the
milestone's obtainable artifact is an open project-level decision, recorded
rather than omitted.

Live state: https://github.com/cardano-foundation/cardano-wallet/wiki/M113-State
