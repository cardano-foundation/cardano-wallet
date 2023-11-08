-- |
-- Copyright: © 2023 Cardano Foundation
-- License: Apache-2.0
--
module Cardano.Write.Tx
    (
    -- * Balancing transactions
      balanceTransaction
    , ErrAssignRedeemers (..)
    , ErrBalanceTx (..)
    , ErrBalanceTxAssetsInsufficientError (..)
    , ErrBalanceTxInsufficientCollateralError (..)
    , ErrBalanceTxInternalError (..)
    , ErrBalanceTxOutputAdaQuantityInsufficientError (..)
    , ErrBalanceTxOutputError (..)
    , ErrBalanceTxOutputErrorInfo (..)
    , ErrBalanceTxOutputSizeExceedsLimitError (..)
    , ErrBalanceTxOutputTokenQuantityExceedsLimitError (..)
    , ErrBalanceTxUnableToCreateChangeError (..)
    , ErrUpdateSealedTx (..)

    -- * UTxO-related types and functions
    , UTxO
    , UTxOAssumptions
    , UTxOIndex
    , constructUTxOIndex
    ) where

import Internal.Cardano.Write.Tx
    ( UTxO
    )
import Internal.Cardano.Write.Tx.Balance
    ( ErrAssignRedeemers (..)
    , ErrBalanceTx (..)
    , ErrBalanceTxAssetsInsufficientError (..)
    , ErrBalanceTxInsufficientCollateralError (..)
    , ErrBalanceTxInternalError (..)
    , ErrBalanceTxOutputAdaQuantityInsufficientError (..)
    , ErrBalanceTxOutputError (..)
    , ErrBalanceTxOutputErrorInfo (..)
    , ErrBalanceTxOutputSizeExceedsLimitError (..)
    , ErrBalanceTxOutputTokenQuantityExceedsLimitError (..)
    , ErrBalanceTxUnableToCreateChangeError (..)
    , ErrUpdateSealedTx (..)
    , UTxOAssumptions
    , UTxOIndex
    , balanceTransaction
    , constructUTxOIndex
    )
