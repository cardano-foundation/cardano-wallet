{-# LANGUAGE DuplicateRecordFields #-}
-- |
-- Copyright: © 2023 Cardano Foundation
-- License: Apache-2.0
--
module Cardano.Write.Tx
    (
    -- * Balancing transactions
      balanceTx
    , ErrAssignRedeemers (..)
    , ErrBalanceTx (..)
    , ErrBalanceTxAssetsInsufficientError (..)
    , ErrBalanceTxInsufficientCollateralError (..)
    , ErrBalanceTxInternalError (..)
    , ErrBalanceTxOutputError (..)
    , ErrBalanceTxOutputErrorInfo (..)
    , ErrBalanceTxUnableToCreateChangeError (..)

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
    , ErrBalanceTxOutputError (..)
    , ErrBalanceTxOutputErrorInfo (..)
    , ErrBalanceTxUnableToCreateChangeError (..)
    , UTxOAssumptions
    , UTxOIndex
    , balanceTx
    , constructUTxOIndex
    )
