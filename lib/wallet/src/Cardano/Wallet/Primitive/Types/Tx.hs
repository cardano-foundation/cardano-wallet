{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- This module re-exports all transaction types.
--
module Cardano.Wallet.Primitive.Types.Tx
    (
    -- * Types
      Tx (..)
    , TxChange (..)
    , TxMeta (..)
    , TxMetadata (..)
    , TxMetadataValue (..)
    , TxStatus (..)
    , UnsignedTx (..)
    , TransactionInfo (..)
    , Direction (..)
    , LocalTxSubmissionStatus (..)
    , TxScriptValidity(..)
    , ScriptWitnessIndex (..)
    , TxCBOR

    -- * Serialisation
    , SealedTx (serialisedTx)
    , cardanoTxIdeallyNoLaterThan
    , cardanoTxInExactEra
    , sealedTxFromBytes
    , sealedTxFromBytes'
    , sealedTxFromCardano
    , sealedTxFromCardano'
    , sealedTxFromCardanoBody
    , unsafeSealedTxFromBytes
    , SerialisedTx (..)
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
    , txScriptInvalid

    -- * Queries
    , txAssetIds

    -- * Transformations
    , txMapAssetIds
    , txMapTxIds
    , txRemoveAssetId

    -- * Conversions (Unsafe)
    , unsafeCoinToTxOutCoinValue

    ) where

import Prelude

import Cardano.Slotting.Slot
    ( SlotNo (..) )
import Cardano.Wallet.Primitive.Types.Address
    ( Address )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( TokenMap )
import Cardano.Wallet.Primitive.Types.Tx.Constraints
    ( txOutMaxCoin, txOutMinCoin )
import Cardano.Wallet.Primitive.Types.Tx.SealedTx
    ( SealedTx (..)
    , SerialisedTx (..)
    , cardanoTxIdeallyNoLaterThan
    , cardanoTxInExactEra
    , getSealedTxBody
    , getSealedTxWitnesses
    , mockSealedTx
    , persistSealedTx
    , sealedTxFromBytes
    , sealedTxFromBytes'
    , sealedTxFromCardano
    , sealedTxFromCardano'
    , sealedTxFromCardanoBody
    , unPersistSealedTx
    , unsafeSealedTxFromBytes
    , withinEra
    )
import Cardano.Wallet.Primitive.Types.Tx.TransactionInfo
    ( TransactionInfo (..), fromTransactionInfo, toTxHistory )
import Cardano.Wallet.Primitive.Types.Tx.Tx
    ( ScriptWitnessIndex (..)
    , Tx (..)
    , TxMetadata (..)
    , TxMetadataValue (..)
    , TxScriptValidity (..)
    , collateralInputs
    , inputs
    , txAssetIds
    , txIns
    , txMapAssetIds
    , txMapTxIds
    , txMetadataIsNull
    , txRemoveAssetId
    , txScriptInvalid
    )
import Cardano.Wallet.Primitive.Types.Tx.TxMeta
    ( Direction (..), TxMeta (..), TxStatus (..), isPending )
import Cardano.Wallet.Read.Tx.CBOR
    ( TxCBOR )
import Data.Word
    ( Word64 )
import GHC.Generics
    ( Generic )
import GHC.Stack
    ( HasCallStack )

import qualified Cardano.Wallet.Primitive.Types.Coin as Coin

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

-- | Information about when a transaction was submitted to the local node.
-- This is used for scheduling resubmissions.
data LocalTxSubmissionStatus tx = LocalTxSubmissionStatus
    { txId :: Hash "Tx"
    , submittedTx :: tx
    , latestSubmission :: SlotNo
    -- ^ Time of most recent resubmission attempt.
    } deriving stock (Generic, Show, Eq, Functor)

data TxChange derivationPath = TxChange
    { address :: Address
    , amount :: Coin
    , assets :: TokenMap
    , derivationPath :: derivationPath
    } deriving (Show, Generic, Eq, Ord)

{-------------------------------------------------------------------------------
                          Conversions (Unsafe)
-------------------------------------------------------------------------------}

-- | Converts the given 'Coin' value to a value that can be included in a
--   transaction output.
--
-- Callers of this function must take responsibility for checking that the
-- given value is:
--
--   - not smaller than 'txOutMinCoin'
--   - not greater than 'txOutMaxCoin'
--
-- This function throws a run-time error if the pre-condition is violated.
--
unsafeCoinToTxOutCoinValue :: HasCallStack => Coin -> Word64
unsafeCoinToTxOutCoinValue c
    | c < txOutMinCoin =
        error $ unwords
            [ "unsafeCoinToTxOutCoinValue: coin value"
            , show c
            , "too small for transaction output"
            ]
    | c > txOutMaxCoin =
          error $ unwords
            [ "unsafeCoinToTxOutCoinValue: coin value"
            , show c
            , "too large for transaction output"
            ]
    | otherwise =
        Coin.unsafeToWord64 c
