-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
--
-- This module provides a wallet-specific interface for coin selection.
--
-- Coin selection handles the following responsibilities:
--
--  - selecting inputs from the UTxO set to pay for user-specified outputs;
--  - selecting inputs from the UTxO set to pay for collateral;
--  - producing change outputs to return excess value to the wallet;
--  - balancing a selection to pay for the transaction fee.
--
-- Use the 'performSelection' function to perform a coin selection.
--
module Cardano.Wallet.CoinSelection
    (
    -- * Performing selections
      performSelection
    , Selection
    , SelectionCollateralRequirement (..)
    , SelectionConstraints (..)
    , SelectionError (..)
    , SelectionLimit
    , SelectionLimitOf (..)
    , SelectionOf (..)
    , SelectionParams (..)

    -- * Selection skeletons
    , SelectionSkeleton (..)
    , emptySkeleton

    -- * Selection errors
    , BalanceInsufficientError (..)
    , SelectionBalanceError (..)
    , SelectionCollateralError
    , SelectionOutputError (..)
    , SelectionOutputSizeExceedsLimitError (..)
    , SelectionOutputTokenQuantityExceedsLimitError (..)
    , UnableToConstructChangeError (..)

    -- * Selection reports
    , makeSelectionReportDetailed
    , makeSelectionReportSummarized
    , SelectionReportDetailed
    , SelectionReportSummarized

    -- * Selection deltas
    , balanceMissing
    , selectionDelta
    )
    where

import Cardano.Wallet.CoinSelection.Internal
    ( Selection
    , SelectionCollateralRequirement (..)
    , SelectionConstraints (..)
    , SelectionError (..)
    , SelectionOf (..)
    , SelectionOutputError (..)
    , SelectionOutputSizeExceedsLimitError (..)
    , SelectionOutputTokenQuantityExceedsLimitError (..)
    , SelectionParams (..)
    , SelectionReportDetailed
    , SelectionReportSummarized
    , makeSelectionReportDetailed
    , makeSelectionReportSummarized
    , performSelection
    , selectionDelta
    )
import Cardano.Wallet.CoinSelection.Internal.Balance
    ( BalanceInsufficientError (..)
    , SelectionBalanceError (..)
    , SelectionLimit
    , SelectionLimitOf (..)
    , SelectionSkeleton (..)
    , UnableToConstructChangeError (..)
    , balanceMissing
    , emptySkeleton
    )
import Cardano.Wallet.CoinSelection.Internal.Collateral
    ( SelectionCollateralError )
