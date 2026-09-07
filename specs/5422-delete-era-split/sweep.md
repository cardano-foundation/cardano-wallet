# Sweep — six suspected era splits, and whether deleting them is safe

Ticket: delete the spurious era split in `mkLedgerTx` (cardano-wallet #5422).
This file records, for each suspected site, the two evidence legs that decide
whether deleting the era match is safe, and the verdict that follows.

**The method.** A site is *same shape* — its era match can simply be deleted —
only when both legs pass:

- **Leg A — type.** Delete the era match at the site, compile the library with
  the compiler set to treat every warning as fatal, record the compiler's
  answer verbatim, then revert. The experiment leaves the tree untouched.
- **Leg B — semantics.** State in one sentence what the surviving Conway arm
  decides, then cite the ledger fact that settles whether the Dijkstra era
  decides it the same way.

Anything else is *not same shape*, and the verdict names which leg failed. A
verdict is a recorded decision, not a change: no swept site is implemented
here; a site found deletable belongs to a follow-up ticket.

**Resolved dependency versions** used for every citation below, read from this
tree's own build plan (`dist-newstyle/cache/plan.json`):

- `cardano-ledger-conway-1.23.0.0`
- `cardano-ledger-dijkstra-0.3.0.0`
- `cardano-ledger-core-1.21.0.0`
- `cardano-balance-transaction` at `d0360834df9d4d4730d5a0b96623d50aadf010d5`
  (pinned in `cabal.project`)

**The standing hazard.** In `cardano-ledger-conway-1.23.0.0`, registration
without a deposit exists: `mkRegTxCert` /
`mkUnRegTxCert` build `ConwayRegCert c SNothing`
(`src/Cardano/Ledger/Conway/TxCert.hs:150-157`). In
`cardano-ledger-dijkstra-0.3.0.0` it does not: every form of
`DijkstraDelegCert` carries a `Coin` deposit
(`src/Cardano/Ledger/Dijkstra/TxCert.hs:80-85`), the `DijkstraTxCert` data
type has no deposit-free registration constructor
(`src/Cardano/Ledger/Dijkstra/TxCert.hs:172-175`), and
`upgradeTxCert` rejects the old forms outright —
`RegTxCert {} -> Left RegTxCertExpunged` and
`UnRegTxCert {} -> Left UnRegTxCertExpunged`
(`src/Cardano/Ledger/Dijkstra/TxCert.hs:258-259`). A change that looks
identical on both sides of that difference compiles, passes every
currently-green test, and is wrong.

--------------------------------------------------------------------------------

## Site 22 — `joinStakePoolDelegationAction` (`lib/wallet/src/Cardano/Wallet/Delegation.hs`)

What the Conway arm decides: whether a DRep voting action accompanies the
stake-pool join — an unregistered stake key joins with an abstain vote
attached, a registered one votes abstain unless the request explicitly
declines.

**Leg A — type.** Deleting the era match and letting the Conway body serve
both eras compiles: the arm produces era-free values
(`Maybe Tx.VotingAction`). Recorded compile of the library with warnings
fatal: exit 0.

**Leg B — semantics.** The decision couples a join to DRep-vote
certificates whose registration half is exactly the Conway-only facility:
Conway tolerates deposit-free registration (`Conway/TxCert.hs:150-157`),
while Dijkstra expunges it (`Dijkstra/TxCert.hs:258-259`) and offers only
deposit-carrying registration, including a combined
register-and-delegate certificate (`mkRegDepositDelegTxCert`,
`Dijkstra/TxCert.hs:336`) whose decomposition differs from what the Conway
path emits. The Conway decision does not describe Dijkstra behavior, so it
cannot simply serve both eras.

**Verdict:** not same shape (leg A compiles; leg B fails on the registration
asymmetry).

## Site 23 — `guardJoin` (`lib/wallet/src/Cardano/Wallet/Delegation.hs`)

What the Conway arm decides: the duplicate-vote policy for re-joining while
already delegating — which vote requests are accepted and which are refused
as already-delegating/already-voting.

**Leg A — type.** Deleting the era match compiles: the arm produces
era-free `Either ErrCannotJoin ()` values. Recorded compile with warnings
fatal: exit 0.

**Leg B — semantics.** The policy's consequences are vote-coupled
delegation certificates, and the certificate space it was decided against
moved: Dijkstra keeps DRep vote delegation itself (the `Delegatee` type is
shared — `Dijkstra/TxCert.hs:26`, defined at `Conway/TxCert.hs:379-382`),
but reshapes every registration around a mandatory deposit
(`Dijkstra/TxCert.hs:80-85`, expunging at `:258-259`). The refusals the
Conway arm hands out for Conway requests are not thereby settled for
Dijkstra requests.

**Verdict:** not same shape (leg A compiles; leg B fails).

## Site 24 — `guardEraIsConway` (`lib/wallet/src/Cardano/Wallet/Delegation.hs`, local binding inside `joinDRepVotingAction`)

What the Conway arm decides: nothing about content — the function exists in
order to reject every non-Conway era loudly.

**Leg A — type.** Deleting the era match (an era-blind `Right ()`)
compiles: exit 0. This is the trap the two-leg method exists for: the
compile succeeds precisely because the deletion removes a rejection, not
because the eras agree.

**Leg B — semantics.** Generalizing the guard would convert a loud refusal
into unconditional acceptance for Dijkstra, whose registration-certificate
space the guarded vote construction has not been checked against
(expunging at `Dijkstra/TxCert.hs:258-259`); a failure made quiet is not a
behavior shared by the two eras, it is a behavior lost.

**Verdict:** not same shape (leg A compiles; leg B fails — the deletion is
suppression wearing a deletion's clothes).

## Site 27 — `installScriptWitnesses` (`lib/wallet/src/Cardano/Wallet/Shelley/Transaction/Unsigned.hs`)

What the Conway arm decides: that a freshly built transaction receives
reference inputs, reference scripts on outputs, and native-script witnesses
before it is returned.

**Leg A — type.** Deleting the era match does not compile. Verbatim:

```
src/Cardano/Wallet/Shelley/Transaction/Unsigned.hs:368:38: error: [GHC-25897]
    • Couldn't match type ‘era’ with ‘Write.Conway’
      Expected: Write.Tx era
        Actual: Write.Tx Write.Conway
```

The surviving helper is Conway-typed
(`installScriptWitnessesConway :: ScriptWitnesses -> Write.Tx Write.Conway ->
Write.Tx Write.Conway`, `Unsigned.hs:374-377`), so the body cannot serve a
rigid era.

**Leg B — semantics.** The things the arm installs are era-indexed values:
script witnesses have type `AlonzoScript era` (`Core.Script era ~
AlonzoScript era`, `Cardano.Balance.Tx.Eras.hs:184` in
`cardano-balance-transaction`), so what Conway decides to install is not a
value a Dijkstra transaction carries; the ledger does not decide the same
attachment for both eras.

**Verdict:** not same shape (leg A fails).

## Site 28 — `certificateFromDelegationActionLedger` (`lib/wallet/src/Cardano/Wallet/Shelley/Transaction/Unsigned.hs`)

What the Conway arm decides: how a delegation action becomes certificates —
a plain join becomes one delegation certificate; join-with-registration and
quit become deposit-carrying certificates; a missing deposit is a refused,
loud degraded case.

**Leg A — type.** Deleting the Dijkstra equation leaves the function
non-exhaustive and does not compile. Verbatim:

```
src/Cardano/Wallet/Shelley/Transaction/Unsigned.hs:546:1: error: [GHC-62161]
[-Wincomplete-patterns, Werror=incomplete-patterns]
    Pattern match(es) are non-exhaustive
    In an equation for ‘certificateFromDelegationActionLedger’:
        Patterns of type ‘RecentEra era’, ‘Either XPub (Script KeyHash)’,
                         ‘Maybe Coin’, ‘DelegationAction’ not matched:
            RecentEraDijkstra _ _ _
```

**Leg B — semantics.** Dijkstra decides registration differently at the
ledger level: deposit-free registration is expunged
(`Dijkstra/TxCert.hs:258-259`) and every registration carries its deposit
(`:80-85`), so the Conway-era meaning of the missing-deposit case — a
tolerated degraded path refused by the wallet — has no Dijkstra counterpart
to reuse; the same body would be deciding something Dijkstra's certificate
language cannot express.

**Verdict:** not same shape (leg A fails; leg B fails independently).

## Site 29 — `certificateFromVotingActionLedger` (`lib/wallet/src/Cardano/Wallet/Shelley/Transaction/Unsigned.hs`)

What the Conway arm decides: how a voting action becomes certificates — a
plain vote becomes one DRep delegation certificate; vote-with-registration
becomes a deposit certificate plus a delegation certificate; a missing
deposit is refused.

**Leg A — type.** Deleting the Dijkstra equation leaves the function
non-exhaustive and does not compile. Verbatim:

```
src/Cardano/Wallet/Shelley/Transaction/Unsigned.hs:603:1: error: [GHC-62161]
[-Wincomplete-patterns, Werror=incomplete-patterns]
    Pattern match(es) are non-exhaustive
    In an equation for ‘certificateFromVotingActionLedger’:
        Patterns of type ‘RecentEra era’, ‘Either XPub (Script KeyHash)’,
                         ‘Maybe Coin’, ‘VotingAction’ not matched:
            RecentEraDijkstra _ _ _
```

**Leg B — semantics.** Dijkstra retains the vote half (shared `Delegatee`,
`mkDelegTxCert` at `Dijkstra/TxCert.hs:331`) but not the registration half
the coupled case depends on (expunging at `:258-259`), so the Conway arm's
decision — in particular its treatment of a missing deposit — does not
describe Dijkstra behavior.

**Verdict:** not same shape (leg A fails; leg B fails independently).

--------------------------------------------------------------------------------

## Outcome

Six suspected sites, six verdicts, all *not same shape*: sites 22, 23 and 24
compile after deletion and are refuted by leg B (the compiler alone returns
the wrong answer at exactly these three); sites 27, 28 and 29 fail leg A
directly, with leg B concurring at 28 and 29 and the ledger citations
recording why. None of the six is implemented here. Each carries a one-line
pointer comment at its site naming the operative reason and this file.
