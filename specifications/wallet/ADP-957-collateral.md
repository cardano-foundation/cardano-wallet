# Basic definitions

*   **Collateral inputs**, in a transaction, are used to cover transaction fees
    in the case that a non-native script fails. The term **collateral** refers
    to the total ada contained in the UTxOs referenced by collateral inputs.
    Collateral inputs can **only** contain ada: they may **not** contain
    non-ada assets.

*   The protocol parameter `collateralPercent` is used to specify the
    percentage of the total transaction fee its collateral must (at a minimum)
    cover. The collateral inputs must not themselves be locked by a script.

*   The protocol parameter `maxCollateralInputs` is used to limit,
    additionally, the total number of collateral inputs, and thus the total
    number of additional signatures that must be checked during validation.

# Specification

1.  The wallet application, when requested to create a transaction requiring
    collateral, should attempt to select one or more **pure-ada** entries from
    the wallet's UTxO set to serve as collateral inputs. The wallet application
    **must not** select UTxO entries that include assets other than ada to
    serve as collateral.

2.  The wallet application, when requested to create a transaction requiring
    collateral, should attempt to select no fewer than one UTxO entry, and no
    more than `maxCollateralInputs` UTxO entries to serve as collateral.

    If it is not possible to satisfy these bounds, the wallet application must
    return an error to the caller.

3.  The wallet application, when requested to create a transaction requiring
    collateral, should attempt to select an amount of ada that is greater than
    or equal to a minimum bound that is determined by the `collateralPercent`
    protocol parameter and the transaction fee:

        collateralAmount ≥ txFee × collateralPercent

    If it is not possible to satisfy the minimum bound, the wallet application
    must return an error to the caller.

4.  The wallet application, when requested to create a transaction requiring
    collateral, should select an amount of ada that is less than or equal to a
    maximum bound that is determined by a user-specified `maxCollateralFactor`
    parameter:

        collateralAmount ≤ txFee × collateralPercent × maxCollateralFactor

    where:

        maxCollateralFactor ≥ 1

    If it is not possible to satisfy the maximum bound, the wallet application
    must return an error to the caller.

5.  The wallet application may provide a default value for the
    `maxCollateralFactor` parameter.

6.  The wallet application, when requested to create a transaction requiring
    collateral, should make a reasonable attempt to select the smallest amount
    of ada that satisfies the minimum and maximum bounds, but is permitted to
    select a higher, non-optimal amount (still within bounds) in cases where
    searching for the optimal solution would entail an unreasonably high
    performance cost.

7.  The wallet application must ensure that UTxO entries used for collateral
    inputs in pending transactions are not available for reuse as collateral
    inputs in future transactions.

8.  The wallet application should consider a UTxO entry used for collateral in
    a pending transaction to be part of the UTxO set that is available for
    ordinary input selection in future transactions, unless that UTxO entry is
    also used as an ordinary input in a pending transaction.

9.  On processing a transaction in a block, if the transaction is marked as
    having passed validation, the wallet application should remove all ordinary
    inputs within that transaction from the wallet's UTxO set:

        UTxO       := UTxO ⋪ inputs tx
        pendingTxs := pendingTxs − tx

10. On processing a transaction in a block, if the transaction is marked as NOT
    having passed validation, the wallet application should remove all
    collateral inputs within that transaction from the wallet's UTxO set:

        UTxO       := UTxO ⋪ collateralInputs tx
        pendingTxs := pendingTxs − tx

# Potential issues that might affect users

## Lack of collateral

In general, a wallet's UTxO set is **not** guaranteed to have entries that are
suitable for use as collateral inputs. For example, it's quite possible that
**all** of a wallet's ada is bundled together with other non-ada assets.

There are several ways this might occur naturally:

1.  If the user frequently makes payments with non-ada assets, then the coin
    selection algorithm will frequently create change outputs that bundle ada
    together with non-ada assets. Over time, the UTxO distribution of their
    wallet may evolve to a state where all ada is bundled together with non-ada
    assets.

2.  If the user migrates a wallet with non-ada assets, these assets will be
    bundled together with ada in the resulting wallet. Indeed, the migration
    algorithm is designed to coalesce bundles together as much as possible, in
    order to:

    a. minimize the number of transactions required to perform a migration;
    b. minimize the overall fee required; and
    c. maximize the proportion of a UTxO set that can be successfully migrated.

## Excessive collateral

The distribution of a wallet's UTxO set may be such that all the pure-ada
entries are far in excess of what would be required for collateral.

In this case, the user might be reluctant to use any of their existing entries
as collateral.

## Potential solutions for these issues

1.  Do nothing in addition to satisfying the requirements above.

    If the user encounters a situation where their wallet's UTxO set does not
    have enough collateral, or where the UTxO set only has pure-ada entries
    that are too large, then they themselves will need to manually create a
    rebalancing transaction that creates the appropriate collateral entries.

2.  Adjust the wallet so that it tries to maintain, over time, a UTxO set that
    has some proportion of entries suitable for inclusion as collateral inputs
    in future transactions. This might involve the following adjustments:

    a.  Adjust the migration algorithm so that it attempts to create a target
        UTxO distribution with at least some entries suitable for use as
        collateral inputs. This would probably require a relatively large
        amount of development and testing effort to achieve, as the migration
        algorithm is currently designed to do the opposite of this.

    b.  Adjust the way we call the standard coin selection algorithm, so that
        if the requested transaction would evolve the UTxO set to a state where
        there are no entries suitable for use as collateral inputs, we
        automatically append one or more additional pure-ada outputs (similar
        to change outputs) to the transaction, to rectify this. This would
        almost certainly add extra complexity to the coin selection algorithm.

3.  On accepting a user request to make a selection that requires collateral:

    a.  If the wallet detects that the UTxO set has **sufficient** collateral,
        then return just one selection, with the outputs the user asked for.

    b.  If the wallet detects that the UTxO set has **insufficient** collateral,
        then return an ordered list with two selections:

        1. Selection 1: a transaction that creates appropriate collateral.
        2. Selection 2: a transaction corresponding to the user request.

    c.  If the wallet detects that the UTxO set has **sufficient** collateral,
        but that it's not possible to satisfy the `collateralPercentMaximum`
        parameter, then return an ordered list with two selections:

        1. Selection 1: a transaction that creates appropriate collateral.
        2. Selection 2: a transaction corresponding to the user request.

    In each of the above cases, the user can inspect the list of selections
    returned, and decide whether or not they want to go ahead (by signing the
    selections, and/or submitting them).

4.  Provide an explicit endpoint that creates a collateral rearrangement
    transaction. This endpoint might have to accept a specific amount.  On
    accepting a user request to make a selection that requires collateral, if
    we detect that the wallet has insufficient collateral, then fail with an
    error that refers them to this endpoint.

# Discussion points

1.  What is the true risk, for an honest user, of losing their collateral? Is
    it possible for bugs in one or more components of the system (e.g., the
    wallet) to result in the loss of the user's collateral, even when the user
    is honest?

2.  Even if we adjust the migration and coin selection algorithms so that they
    maintain, over time, a UTxO set that has some entries suitable for use as
    collateral, it's likely that there still other ways in which a wallet's
    UTxO set can arrive at a state where there are no entries suitable for use
    as collateral. For example, even if we did adjust the migration algorithm
    in the way described above, then a wallet migrated with the old algorithm
    might still be without entries suitable for use as collateral.

# Additional definitions

*   A **rebalancing transaction**: a transaction whose output addresses are all
    owned by the wallet, whose sole purpose is to rebalance the UTxO
    distribution in some way.

*   A **collateral rebalancing transaction**: a transaction that rebalances the
    UTxO distribution so that it has outputs suitable for inclusion as
    collateral inputs in a later transaction. Such a transaction would ensure
    the presence of one or more pure-ada UTxO entries, and could be tailored to
    producing collateral entries of a certain size.
