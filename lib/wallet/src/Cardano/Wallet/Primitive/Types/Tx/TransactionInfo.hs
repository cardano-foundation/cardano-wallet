{-# LANGUAGE DataKinds #-}
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
-- This module provides the `TransactionInfo` data types used by the wallet.
--
module Cardano.Wallet.Primitive.Types.Tx.TransactionInfo
    ( TransactionInfo (..)
    , fromTransactionInfo
    , toTxHistory
    )
    where

import Prelude

import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount (..) )
import Cardano.Wallet.Primitive.Types.Tx.Tx
    ( Tx (..), TxMetadata, TxScriptValidity )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn )
import Cardano.Wallet.Primitive.Types.Tx.TxMeta
    ( TxMeta )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
    ( TxOut )
import Cardano.Wallet.Read.Tx.CBOR
    ( TxCBOR )
import Control.DeepSeq
    ( NFData (..) )
import Data.Map.Strict
    ( Map )
import Data.Quantity
    ( Quantity (..) )
import Data.Time.Clock
    ( UTCTime )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )

-- | Full expanded and resolved information about a transaction, suitable for
-- presentation to the user.
data TransactionInfo = TransactionInfo
    { txInfoId :: Hash "Tx"
    -- ^ Transaction ID of this transaction
    , txInfoCBOR :: Maybe TxCBOR
    -- ^ Serialization of this transaction
    , txInfoFee :: Maybe Coin
    -- ^ Explicit transaction fee
    , txInfoInputs :: [(TxIn, Maybe TxOut)]
    -- ^ Transaction inputs and (maybe) corresponding outputs of the
    -- source. Source information can only be provided for outgoing payments.
    , txInfoCollateralInputs :: [(TxIn, Maybe TxOut)]
    -- ^ Collateral inputs and (maybe) corresponding outputs.
    , txInfoOutputs :: [TxOut]
    -- ^ Payment destination.
    , txInfoCollateralOutput :: Maybe TxOut
    -- ^ An output that is only created if a transaction script fails.
    , txInfoWithdrawals :: Map RewardAccount Coin
    -- ^ Withdrawals on this transaction.
    , txInfoMeta :: TxMeta
    -- ^ Other information calculated from the transaction.
    , txInfoDepth :: Quantity "block" Natural
    -- ^ Number of slots since the transaction slot.
    , txInfoTime :: UTCTime
    -- ^ Creation time of the block including this transaction.
    , txInfoMetadata :: Maybe TxMetadata
    -- ^ Application-specific extension data.
    , txInfoScriptValidity :: Maybe TxScriptValidity
    -- ^ Tag indicating whether non-native scripts in this transaction passed
    -- validation. This is added by the block creator when constructing the
    -- block. May be 'Nothing' for pre-Alonzo and pending transactions.
    } deriving (Generic, Show, Eq)

instance NFData TransactionInfo

-- | Reconstruct a transaction from a transaction info.
fromTransactionInfo :: TransactionInfo -> Tx
fromTransactionInfo info = Tx
    { txId = txInfoId info
    , txCBOR = txInfoCBOR info
    , fee = txInfoFee info
    , resolvedInputs = txInfoInputs info
    , resolvedCollateralInputs = txInfoCollateralInputs info
    , outputs = txInfoOutputs info
    , collateralOutput = txInfoCollateralOutput info
    , withdrawals = txInfoWithdrawals info
    , metadata = txInfoMetadata info
    , scriptValidity = txInfoScriptValidity info
    }

-- | Drop time-specific information
toTxHistory :: TransactionInfo -> (Tx, TxMeta)
toTxHistory info = (fromTransactionInfo info, txInfoMeta info)
