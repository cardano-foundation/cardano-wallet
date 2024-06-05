{-# LANGUAGE DuplicateRecordFields #-}
-- |
-- Copyright: © 2023-2024 Cardano Foundation
-- License: Apache-2.0
--
--
module Cardano.Write.Tx
    (
    -- * UTxO
      UTxO (..)

    -- * Tx
    , Tx
    , TxBody

    -- ** PartialTx
    , PartialTx (..)
    , Redeemer (..)
    , StakeKeyDepositLookup (..)
    , TimelockKeyWitnessCounts (..)

    -- ** Balancing
    , balanceTx
    , TimeTranslation
    , UTxOAssumptions (..)
    , UTxOIndex
    , constructUTxOIndex
    , ChangeAddressGen (..)

    -- ** Balancing Errors
    , ErrAssignRedeemers (..)
    , ErrBalanceTx (..)
    , ErrBalanceTxAssetsInsufficientError (..)
    , ErrBalanceTxInsufficientCollateralError (..)
    , ErrBalanceTxInternalError (..)
    , ErrBalanceTxOutputError (..)
    , ErrBalanceTxOutputErrorInfo (..)
    , ErrBalanceTxUnableToCreateChangeError (..)
    ) where

import Cardano.Ledger.Api
    ( Tx
    , TxBody
    )
import Cardano.Ledger.Api.UTxO
    ( UTxO (..)
    )
import Internal.Cardano.Write.Tx.Balance
    ( ChangeAddressGen (..)
    , ErrAssignRedeemers (..)
    , ErrBalanceTx (..)
    , ErrBalanceTxAssetsInsufficientError (..)
    , ErrBalanceTxInsufficientCollateralError (..)
    , ErrBalanceTxInternalError (..)
    , ErrBalanceTxOutputError (..)
    , ErrBalanceTxOutputErrorInfo (..)
    , ErrBalanceTxUnableToCreateChangeError (..)
    , PartialTx (..)
    , Redeemer (..)
    , StakeKeyDepositLookup (..)
    , UTxOAssumptions (..)
    , UTxOIndex
    , balanceTx
    , constructUTxOIndex
    )
import Internal.Cardano.Write.Tx.Sign
    ( TimelockKeyWitnessCounts (..)
    )
import Internal.Cardano.Write.Tx.TimeTranslation
    ( TimeTranslation
    )
