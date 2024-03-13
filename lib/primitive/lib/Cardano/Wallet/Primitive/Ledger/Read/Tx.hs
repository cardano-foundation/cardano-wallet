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
    ( IsEra (..)
    , Tx (..)
    )
import Cardano.Wallet.Read.Eras
    ( eraValue
    )
import Cardano.Wallet.Read.Tx.CBOR
    ( renderTxToCBOR
    )
import Cardano.Wallet.Read.Tx.CollateralInputs
    ( getEraCollateralInputs
    )
import Cardano.Wallet.Read.Tx.CollateralOutputs
    ( getEraCollateralOutputs
    )
import Cardano.Wallet.Read.Tx.Fee
    ( getEraFee
    )
import Cardano.Wallet.Read.Tx.Hash
    ( getEraTxHash
    )
import Cardano.Wallet.Read.Tx.Inputs
    ( getEraInputs
    )
import Cardano.Wallet.Read.Tx.Metadata
    ( getEraMetadata
    )
import Cardano.Wallet.Read.Tx.Outputs
    ( getEraOutputs
    )
import Cardano.Wallet.Read.Tx.ScriptValidity
    ( getEraScriptValidity
    )
import Cardano.Wallet.Read.Tx.Withdrawals
    ( getEraWithdrawals
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
    txCBOR <- Just . renderTxToCBOR . eraValue
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
