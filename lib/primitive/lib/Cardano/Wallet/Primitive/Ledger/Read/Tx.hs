{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
module Cardano.Wallet.Primitive.Ledger.Read.Tx
    ( primitiveTx
    )
where

import Prelude

import Cardano.Read.Ledger.Tx.CollateralInputs
    ( getEraCollateralInputs
    )
import Cardano.Read.Ledger.Tx.CollateralOutputs
    ( getEraCollateralOutputs
    )
import Cardano.Read.Ledger.Tx.Fee
    ( getEraFee
    )
import Cardano.Read.Ledger.Tx.Hash
    ( getEraTxHash
    )
import Cardano.Read.Ledger.Tx.Inputs
    ( getEraInputs
    )
import Cardano.Read.Ledger.Tx.Metadata
    ( getEraMetadata
    )
import Cardano.Read.Ledger.Tx.Outputs
    ( getEraOutputs
    )
import Cardano.Read.Ledger.Tx.ScriptValidity
    ( getEraScriptValidity
    )
import Cardano.Read.Ledger.Tx.Withdrawals
    ( getEraWithdrawals
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.CollateralInputs
    ( getCollateralInputs
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.CollateralOutputs
    ( getCollateralOutputs
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Fee
    ( getFee
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Inputs
    ( getInputs
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Metadata
    ( getMetadata
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Outputs
    ( getOutputs
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.ScriptValidity
    ( getScriptValidity
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Withdrawals
    ( getWithdrawals
    )
import Cardano.Wallet.Read
    ( EraValue (..)
    , IsEra (..)
    , Tx (..)
    )
import Cardano.Wallet.Read.Tx.CBOR
    ( renderTxToCBOR
    )
import Data.Foldable
    ( fold
    )

import qualified Cardano.Wallet.Primitive.Types.Hash as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W

primitiveTx
    :: IsEra era
    => Tx era
    -> W.Tx
primitiveTx = do
    txId <- W.Hash . getEraTxHash
    txCBOR <- Just . renderTxToCBOR . EraValue
    fee <- getFee . getEraFee
    resolvedInputs <- fmap (,Nothing) . getInputs . getEraInputs
    resolvedCollateralInputs <-
        fmap (,Nothing)
            . getCollateralInputs
            . getEraCollateralInputs
    outputs <- getOutputs . getEraOutputs
    collateralOutput <- getCollateralOutputs . getEraCollateralOutputs
    withdrawals <- fold . getWithdrawals . getEraWithdrawals
    metadata <- getMetadata . getEraMetadata
    scriptValidity <- getScriptValidity . getEraScriptValidity
    pure $ W.Tx{..}
