{-# LANGUAGE PatternSynonyms #-}
{- |
Copyright: © 2022 IOHK, 2024 Cardano Foundation
License: Apache-2.0

The 'Tx' type represents transactions as they are read from the mainnet ledger.
It is compatible with the era-specific index types from @cardano-ledger@.
-}
module Cardano.Wallet.Read.Tx
    (
    -- * Tx
      TxT
    , Tx (..)
    , getInputs
    , getCollateralInputs
    , IsValid (IsValidC)
    , getScriptValidity
    , ValidityInterval (..)
    , invalidBefore
    , invalidHereafter
    , getValidityInterval

    -- * TxId
    , TxId
    , getTxId
    , EraIndependentTxBody
    , txIdFromHash
    , hashFromTxId

    -- * TxIn
    , TxIx
    , pattern TxIx

    , TxIn
    , pattern TxIn

    -- * TxOut
    , TxOut
    , getCompactAddr
    , getValue
    , mkBasicTxOut
    , utxoFromEraTx
    , upgradeTxOutToBabbageOrLater
    , toBabbageOutput
    , toConwayOutput
    , deserializeTxOut
    , serializeTxOut
    , mkEraTxOut
    ) where

import Cardano.Read.Ledger.Tx.Tx
    ( Tx (..)
    , TxT
    )
import Cardano.Wallet.Read.Tx.CollateralInputs
    ( getCollateralInputs
    )
import Cardano.Wallet.Read.Tx.Inputs
    ( getInputs
    )
import Cardano.Wallet.Read.Tx.ScriptValidity
    ( IsValid (IsValidC)
    , getScriptValidity
    )
import Cardano.Wallet.Read.Tx.TxId
    ( EraIndependentTxBody
    , TxId
    , getTxId
    , hashFromTxId
    , txIdFromHash
    )
import Cardano.Wallet.Read.Tx.TxIn
    ( TxIn
    , TxIx
    , pattern TxIn
    , pattern TxIx
    )
import Cardano.Wallet.Read.Tx.TxOut
    ( TxOut
    , deserializeTxOut
    , getCompactAddr
    , getValue
    , mkBasicTxOut
    , mkEraTxOut
    , serializeTxOut
    , toBabbageOutput
    , toConwayOutput
    , upgradeTxOutToBabbageOrLater
    , utxoFromEraTx
    )
import Cardano.Wallet.Read.Tx.Validity
    ( ValidityInterval (..)
    , getValidityInterval
    , invalidBefore
    , invalidHereafter
    )
