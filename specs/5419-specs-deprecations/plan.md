# plan.md — #5419

Ceiling: 110 lines.

## Strategy

Retire the deprecated `cardano-api` transaction-body surface by reading the
body from the **ledger** transaction, the direction #5413 established. The
deprecated surface is dominated by **record fields of `TxBodyContent`** and by
`createTransactionBody` / `getTxBodyContent` / `getTxBody`. None of these is
visible to a type-name grep, so **causes come from GHC's own diagnostics**:
remove the pragma, build under release flags, read what actually warns. A
previous count taken by grepping was wrong in kind, not merely in number.

`Cardano/Api/Extra.hs`'s two converters are an identity pair in Conway, so
conversion round-trips can be dropped rather than replaced.

## Constraints

- **Base** `f1dd799c4f704294ae4631eaa7a392c41d9bb8eb` — #5413's head, which was
  force-updated from `9c90a55b` mid-slice when #5413 retargeted to `master`
  after #5399 merged. Numbers taken before that move were re-measured, not
  carried forward.
- **Deliverable was planned as two commits, one per file, folded into #5413
  with no PR of its own.** Human review is this project's binding constraint —
  #5402 waited five days — and these files are ones #5413 already edited, so a
  separate PR looked like a review cycle spent for nothing. **That plan was
  overtaken: #5413 merged first, as `1fdc2d8b41`, so this work went out as its
  own PR #5420 and carries a third commit adding these planning documents.**
- **The commit owner does not push.** The branch was handed to `%4` on
  acceptance and pushed from there.
- `--flags=release` or nothing.
- `/code/cardano-wallet` is read-only; every seat takes its own worktree.

## Live boundary

None. This ticket is test-only and touches no production module, no persistence,
no network, and no chain state. The signing and serialisation *assertions* are
the thing being preserved; the signing *implementation* is untouched. This is
why INV-3 NON-VACUITY carries the weight — with no production change, the only
way this ticket can do damage is by leaving assertions that no longer discriminate.

## Slices — bisect-safe, one commit each

| ID | Slice | File |
|---|---|---|
| S1 | Retire `TransactionLedgerSpec` deprecations and its pragma | `lib/unit/test/unit/Cardano/Wallet/Shelley/TransactionLedgerSpec.hs` |
| S2 | Retire `TransactionSpec` deprecations and its pragma | `lib/unit/test/unit/Cardano/Wallet/Shelley/TransactionSpec.hs` |

Each slice is independently buildable and independently revertible.

## Topology

`OWNER`. The complete accepted behaviour is not mechanically entailed by a
gate: whether a rewritten assertion still discriminates is a semantic judgement,
so `LIGHT` is unavailable. One strong commit owner, one fresh independent
auditor per submission.

- ticket owner `%310` claude / `claude-opus-5[1m]` / high
- commit owner `%304` codex / `gpt-5.6-sol` / high — inherited mid-work
- auditor grok / `grok-4.6` — derived, `../../handoffs/seat-derivation.txt`
- draft = NONE

## Event log — what actually happened, kept because it changed the plan

1. The commit owner was dispatched by `%4` before this ticket was constituted.
   It is **inherited, not restarted**; its work is not discarded or redone.
2. Base moved `9c90a55b` -> `f1dd799c` mid-slice. Both commits rebased; counts
   re-measured from the new tree.
3. **Census breach found pre-submission.** Candidate `73e43b0b` measured
   **49/15** against a base of 44/15 — five pre-existing stub sites deleted, ten
   added, one per `prop_signTransaction_*` per file. Found independently by the
   ticket owner and by the commit owner, and confirmed by two instruments.
4. **A rename repair was attempted and refused.** Five stubs were reworded from
   `"prop_X: Dijkstra"` to `"prop_X: unsupported recent era"`, which does not
   retire a stub — it renames one so the counter cannot see it. Refused,
   reverted, and written into `spec.md` as R-4. It would have made M6's own
   outcome measure lie for every future ticket, and it is the same shape as the
   suppression habit this ticket exists to end.
5. **Q-001 asked and answered:** the per-property era dispatch is **incidental,
   not forced** (`enforced: NONE`, symbol-level evidence). So fence and ticket
   are not in conflict and nothing escalates to `%4`. The ordinary repair is
   the commit owner's to design.
6. No `PROOF-COMPLETE` was ever declared, so all of the above cost **zero
   submissions**.

## Ruling carried into the gate

**44 is a ceiling, not a quota**, on the milestone's own precedent that a MAX is
a measurement and not an allowance. The census must not **exceed** 44/15. An
honest repair landing below is a win and is reported as a site delta so the
milestone re-baselines deliberately rather than discovering it.
