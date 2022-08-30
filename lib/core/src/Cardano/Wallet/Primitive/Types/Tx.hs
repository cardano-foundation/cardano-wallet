{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- This module re-export all transaction types.
--
module Cardano.Wallet.Primitive.Types.Tx
    (
    -- * Types
      Tx (..)
    , TxIn (..)
    , TxOut (..)
    , TxChange (..)
    , TxMeta (..)
    , TxMetadata (..)
    , TxMetadataValue (..)
    , TxStatus (..)
    , UnsignedTx (..)
    , TransactionInfo (..)
    , Direction (..)
    , LocalTxSubmissionStatus (..)
    , TokenBundleSizeAssessor (..)
    , TokenBundleSizeAssessment (..)
    , TxScriptValidity(..)
    , ScriptWitnessIndex (..)
    , TxCBOR (..)

    -- * Serialisation
    , SealedTx (serialisedTx)
    , cardanoTxIdeallyNoLaterThan
    , sealedTxFromBytes
    , sealedTxFromBytes'
    , sealedTxFromCardano
    , sealedTxFromCardano'
    , sealedTxFromCardanoBody
    , getSerialisedTxParts
    , unsafeSealedTxFromBytes
    , SerialisedTx (..)
    , SerialisedTxParts (..)
    , getSealedTxBody
    , getSealedTxWitnesses
    , persistSealedTx
    , unPersistSealedTx

    -- ** Unit testing helpers
    , mockSealedTx
    , withinEra

    -- * Functions
    , fromTransactionInfo
    , inputs
    , collateralInputs
    , isPending
    , toTxHistory
    , txIns
    , txMetadataIsNull
    , txOutCoin
    , txOutAddCoin
    , txOutSubtractCoin
    , txScriptInvalid

    -- * Constants
    , txOutMinCoin
    , txOutMaxCoin
    , txOutMinTokenQuantity
    , txOutMaxTokenQuantity
    , txMintBurnMaxTokenQuantity

    -- * Constraints
    , TxConstraints (..)
    , txOutputCoinCost
    , txOutputCoinSize
    , txOutputHasValidSize
    , txOutputHasValidTokenQuantities
    , TxSize (..)
    , txSizeDistance

    -- * Queries
    , txAssetIds
    , txOutAssetIds

    -- * Transformations
    , txMapAssetIds
    , txMapTxIds
    , txRemoveAssetId
    , txOutMapAssetIds
    , txOutRemoveAssetId

    -- * Checks
    , coinIsValidForTxOut

    -- * Conversions (Unsafe)
    , unsafeCoinToTxOutCoinValue

    ) where

import Prelude

import GHC.Generics (Generic)

import Cardano.Wallet.Primitive.Types.Tx.CBOR
import Cardano.Wallet.Primitive.Types.Tx.TransactionInfo
import Cardano.Wallet.Primitive.Types.Tx.Tx
import Cardano.Wallet.Primitive.Types.Tx.TxMeta
import Cardano.Wallet.Primitive.Types.Tx.SealedTx

-- | An unsigned transaction.
--
-- See 'Tx' for a signed transaction.
--
data UnsignedTx input output change withdrawal = UnsignedTx
    { unsignedCollateral
        :: [input]
        -- Inputs used for collateral.

    , unsignedInputs
        :: [input]
        -- ^ Inputs are *necessarily* non-empty because Cardano requires at least
        -- one UTxO input per transaction to prevent replayable transactions.
        -- (each UTxO being unique, including at least one UTxO in the
        -- transaction body makes it seemingly unique).
        --
        -- *However* when used to represent the inputs known by the wallet, in
        -- contrast to all inputs, it can be empty.

    , unsignedOutputs
        :: [output]
        -- Unlike inputs, it is perfectly reasonable to have empty outputs. The
        -- main scenario where this might occur is when constructing a
        -- delegation for the sake of submitting a certificate. This type of
        -- transaction does not typically include any target output and,
        -- depending on which input(s) get selected to fuel the transaction, it
        -- may or may not include a change output should its value be less than
        -- the minimal UTxO value set by the network.

    , unsignedChange
        :: [change]

    , unsignedWithdrawals
        :: [withdrawal]
    }
    deriving (Eq, Generic, Show)