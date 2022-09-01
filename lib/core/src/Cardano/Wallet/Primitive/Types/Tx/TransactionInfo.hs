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
import Cardano.Wallet.Primitive.Types.Tx.CBOR
    ( TxCBOR )
import Cardano.Wallet.Primitive.Types.Tx.Tx
    ( Tx (..), TxIn, TxMetadata, TxOut, TxScriptValidity )
import Cardano.Wallet.Primitive.Types.Tx.TxMeta
    ( TxMeta )
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
    , txInfoInputs :: [(TxIn, Coin, Maybe TxOut)]
    -- ^ Transaction inputs and (maybe) corresponding outputs of the
    -- source. Source information can only be provided for outgoing payments.
    , txInfoCollateralInputs :: [(TxIn, Coin, Maybe TxOut)]
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

-- | Reconstruct a transaction info from a transaction.
fromTransactionInfo :: TransactionInfo -> Tx
fromTransactionInfo info = Tx
    { txId = txInfoId info
    , txCBOR = txInfoCBOR info
    , fee = txInfoFee info
    , resolvedInputs = drop3rd <$> txInfoInputs info
    , resolvedCollateralInputs = drop3rd <$> txInfoCollateralInputs info
    , outputs = txInfoOutputs info
    , collateralOutput = txInfoCollateralOutput info
    , withdrawals = txInfoWithdrawals info
    , metadata = txInfoMetadata info
    , scriptValidity = txInfoScriptValidity info
    }
  where
    drop3rd :: (a, b, c) -> (a, b)
    drop3rd (a, b, _) = (a, b)


-- | Drop time-specific information
toTxHistory :: TransactionInfo -> (Tx, TxMeta)
toTxHistory info =
    (fromTransactionInfo info, txInfoMeta info)
