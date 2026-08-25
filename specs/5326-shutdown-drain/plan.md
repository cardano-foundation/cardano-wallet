# Plan: Drain wallet database workers on shutdown

Artifact ceiling: 150 lines / 10 KiB

## Topology

One `OWNER` slice, `shutdown-drain`. Concurrency, asynchronous exceptions,
resource lifetime, and a live process boundary require semantic ownership and
a fresh independent audit; LIGHT is ineligible.

## Technical strategy

1. Extend the stable Registry owner with one drain operation and the minimum
   completion observability needed to make its return a resource-release
   barrier. Preserve selected-wallet `unregister` behavior.
2. Give each of the four wallet API layers an explicit acquisition/release
   scope in `serveWallet`. The release observes the layer's existing worker
   registry and invokes the Registry-owned drain; unrelated service lifetimes
   remain unchanged.
3. Ship two proof layers:
   - focused Registry concurrency/finalizer proofs, including the required
     deliberately incomplete negative control first;
   - a bounded process smoke that crosses the shipped SIGTERM handler and
     observes at least two release callbacks before exit.

## Live-boundary decision

The smoke ships inside the ticket gate, not as an operator follow-up. The
repository CI already builds the application and provides a local process/test
boundary; this proof needs no production credentials or live external data.
The test must use the shipped signal handler rather than directly throwing at
one worker. It must fail loudly on timeout, skip, an empty match, or fewer than
two completed releases.

## TDD and invariant order

1. Commit the complete RED proof bundle.
2. Before production GREEN, freeze evidence showing
   `INV-5326-FINALIZERS` fails against an implementation that does not await
   cleanup. This is the load-bearing negative control.
3. Implement the minimum production change, run focused proofs, then refactor.
4. Run the immutable slice gate and full ticket gate through durable receipts.
5. Park the owner at a clean candidate for fresh independent audit.

## Verification contract

- Registry unit proof: exact CI `unit-cardano-wallet-unit` artifact, with
  empty-match failure enabled.
- Shutdown smoke/integration proof: the repository integration artifact with a
  focused shutdown-drain match, finite timeout, and explicit release count.
- Quality: `git diff --check`, CI format script, HLint, and Cabal/Nix
  configuration check.
- Final local build: exact relevant Linux CI Nix outputs plus the focused
  proofs. The repository has no `just ci` recipe; no claim will describe it as
  mirroring CI.
- Final remote proof: all required PR checks green; never use
  `gh pr checks --watch`.

## Slice boundary

The slice is bisect-safe only as a whole: the Registry barrier, application
lifetime wiring, and shipped signal proof land in one final behavior commit.
No production or proof edit is allowed after the accepted audited candidate;
only the ticket-owner task stamp may be added during final squash.

## Resource and scope fences

- Production: Registry and wallet application/API lifecycle modules only.
- Proof: Registry unit tests plus the nearest existing integration/process
  test owner and required Cabal module listing.
- Runtime gate and evidence: ticket runtime root and ignored `./gate.sh`.
- `/code/cardano-wallet` is read-only; the protected untracked `.llm` file is
  never touched. All commands and child panes use the issue worktree.
- No draft tool. One Grok commit-owner seat, at most two audited submissions.
- Build budget: four audit/acceptance evidence builds total; focused readiness
  compiles are uncharged.
