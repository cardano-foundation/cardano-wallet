{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- An extra interface for operation on transactions (e.g. creating witnesses,
-- estimating size...). This makes it possible to decouple those operations from
-- our wallet layer, keeping the implementation flexible to various backends.

module Cardano.Wallet.Transaction
    (
    -- * Interface
      TransactionLayer (..)
    , DelegationAction (..)
    , TransactionCtx (..)

    -- * Errors
    , ErrMkTx (..)
    , ErrDecodeSignedTx (..)
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv )
import Cardano.Api.Typed
    ( AnyCardanoEra )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), Passphrase )
import Cardano.Wallet.Primitive.CoinSelection.MA.RoundRobin
    ( SelectionCriteria, SelectionResult, SelectionSkeleton )
import Cardano.Wallet.Primitive.Types
    ( PoolId, ProtocolParameters, SlotNo (..) )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( TokenMap )
import Cardano.Wallet.Primitive.Types.Tx
    ( SealedTx (..), Tx (..), TxMetadata, TxOut )
import Cardano.Wallet.Primitive.Types.UTxOIndex
    ( UTxOIndex )
import Data.ByteString
    ( ByteString )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Quantity
    ( Quantity )
import Data.Text
    ( Text )
import Data.Word
    ( Word16, Word8 )
import GHC.Generics
    ( Generic )

data TransactionLayer k = TransactionLayer
    { mkTransaction
        :: AnyCardanoEra
            -- Era for which the transaction should be created.
        -> (XPrv, Passphrase "encryption")
            -- Reward account
        -> (Address -> Maybe (k 'AddressK XPrv, Passphrase "encryption"))
            -- Key store
        -> ProtocolParameters
            -- Current protocol parameters
        -> TransactionCtx
            -- An additional context about the transaction
        -> SelectionResult TxOut
            -- A balanced coin selection where all change addresses have been
            -- assigned.
        -> Either ErrMkTx (Tx, SealedTx)
        -- ^ Construct a standard transaction
        --
        -- " Standard " here refers to the fact that we do not deal with redemption,
        -- multisignature transactions, etc.
        --
        -- This expects as a first argument a mean to compute or lookup private
        -- key corresponding to a particular address.

    , initSelectionCriteria
        :: ProtocolParameters
            -- Current protocol parameters
        -> TransactionCtx
            -- Additional information about the transaction
        -> UTxOIndex
            -- Available UTxO from which inputs should be selected.
        -> NonEmpty TxOut
            -- A list of target outputs
        -> SelectionCriteria

    , calcMinimumCost
        :: ProtocolParameters
            -- Current protocol parameters
        -> TransactionCtx
            -- Additional information about the transaction
        -> SelectionSkeleton
            -- An intermediate representation of an ongoing selection
        -> Coin
        -- ^ Compute a minimal fee amount necessary to pay for a given selection
        -- This also include necessary deposits.

    , calcMinimumCoinValue
        :: ProtocolParameters
            -- Current protocol parameters
        -> TokenMap
            -- A bundle of native assets
        -> Coin
        -- ^ The minimum ada value needed in a UTxO carrying the asset bundle

    , estimateMaxNumberOfInputs
        :: Quantity "byte" Word16
         -- Transaction max size in bytes
        -> Maybe TxMetadata
         -- Metadata associated with the transaction.
        -> Word8
        -- Number of outputs in transaction
        -> Word8
        -- ^ Approximate maximum number of inputs.

    , decodeSignedTx
        :: AnyCardanoEra
        -> ByteString
        -> Either ErrDecodeSignedTx (Tx, SealedTx)
        -- ^ Decode an externally-signed transaction to the chain producer
    }

-- | Some additional context about a transaction. This typically contains
-- details that are known upfront about the transaction and are used to
-- construct it from inputs selected from the wallet's UTxO.
data TransactionCtx = TransactionCtx
    { txWithdrawal :: Coin
    -- ^ Withdrawal amount from a reward account, can be zero.
    , txMetadata :: Maybe TxMetadata
    -- ^ User or application-defined metadata to embed in the transaction.
    , txTimeToLive :: SlotNo
    -- ^ Transaction expiry (TTL) slot.
    , txDelegationAction :: Maybe DelegationAction
    -- ^ An additional delegation to take.
    } deriving (Show, Eq)

-- | Whether the user is attempting any particular delegation action.
data DelegationAction = RegisterKeyAndJoin PoolId | Join PoolId | Quit
    deriving (Show, Eq, Generic)

-- | Error while trying to decode externally signed transaction
data ErrDecodeSignedTx
    = ErrDecodeSignedTxWrongPayload Text
    | ErrDecodeSignedTxNotSupported
    deriving (Show, Eq)

-- | Possible signing error
data ErrMkTx
    = ErrKeyNotFoundForAddress Address
    -- ^ We tried to sign a transaction with inputs that are unknown to us?
    | ErrConstructedInvalidTx Text
    -- ^ We failed to construct a transaction for some reasons.
    | ErrInvalidEra AnyCardanoEra
    -- ^ Should never happen, means that that we have programmatically provided
    -- an invalid era.
    deriving (Eq, Show)
