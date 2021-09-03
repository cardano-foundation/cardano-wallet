{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
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
    , defaultTransactionCtx
    , Withdrawal (..)
    , withdrawalToCoin

    -- * Errors
    , ErrMkTx (..)
    , ErrDecodeSignedTx (..)

    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv, XPub )
import Cardano.Api
    ( AnyCardanoEra )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), DerivationIndex, Passphrase )
import Cardano.Wallet.Primitive.CoinSelection.Balance
    ( SelectionLimit, SelectionResult, SelectionSkeleton )
import Cardano.Wallet.Primitive.Types
    ( PoolId, ProtocolParameters, SlotNo (..), TokenBundleMaxSize (..) )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount )
import Cardano.Wallet.Primitive.Types.Tx
    ( SealedTx (..)
    , SerialisedTx (..)
    , TokenBundleSizeAssessor
    , Tx (..)
    , TxConstraints
    , TxMetadata
    , TxOut
    )
import Data.ByteString
    ( ByteString )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Text
    ( Text )
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

    , mkUnsignedTransaction
        :: AnyCardanoEra
            -- Era for which the transaction should be created.
        -> XPub
            -- Reward account public key
        -> ProtocolParameters
            -- Current protocol parameters
        -> TransactionCtx
            -- An additional context about the transaction
        -> SelectionResult TxOut
            -- A balanced coin selection where all change addresses have been
            -- assigned.
        -> Either ErrMkTx SerialisedTx
        -- ^ Construct a standard unsigned transaction
        --
        -- " Standard " here refers to the fact that we do not deal with redemption,
        -- multisignature transactions, etc.
        --
        -- The function returns CBOR-ed transaction body to be signed in another step.

    , calcMinimumCost
        :: ProtocolParameters
            -- Current protocol parameters
        -> TransactionCtx
            -- Additional information about the transaction
        -> SelectionSkeleton
            -- An intermediate representation of an ongoing selection
        -> Coin
        -- ^ Compute a minimal fee amount necessary to pay for a given selection
        -- This also includes necessary deposits.

    , computeSelectionLimit
        :: ProtocolParameters
        -> TransactionCtx
        -> [TxOut]
        -> SelectionLimit

    , tokenBundleSizeAssessor
        :: TokenBundleMaxSize -> TokenBundleSizeAssessor
        -- ^ A function to assess the size of a token bundle.

    , constraints
        :: ProtocolParameters
        -- Current protocol parameters.
        -> TxConstraints
        -- The set of constraints that apply to all transactions.

    , decodeSignedTx
        :: AnyCardanoEra
        -> ByteString
        -> Either ErrDecodeSignedTx (Tx, SealedTx)
        -- ^ Decode an externally-signed transaction to the chain producer
    }
    deriving Generic

-- | Some additional context about a transaction. This typically contains
-- details that are known upfront about the transaction and are used to
-- construct it from inputs selected from the wallet's UTxO.
data TransactionCtx = TransactionCtx
    { txWithdrawal :: Withdrawal
    -- ^ Withdrawal amount from a reward account, can be zero.
    , txMetadata :: Maybe TxMetadata
    -- ^ User or application-defined metadata to embed in the transaction.
    , txTimeToLive :: SlotNo
    -- ^ Transaction expiry (TTL) slot.
    , txDelegationAction :: Maybe DelegationAction
    -- ^ An additional delegation to take.
    } deriving (Show, Generic, Eq)

data Withdrawal
    = WithdrawalSelf !RewardAccount !(NonEmpty DerivationIndex) !Coin
    | WithdrawalExternal !RewardAccount !(NonEmpty DerivationIndex) !Coin
    | NoWithdrawal
    deriving (Show, Eq)

withdrawalToCoin :: Withdrawal -> Coin
withdrawalToCoin = \case
    WithdrawalSelf _ _ c -> c
    WithdrawalExternal _ _ c -> c
    NoWithdrawal -> Coin 0

-- | A default context with sensible placeholder. Can be used to reduce
-- repetition for changing only sub-part of the default context.
defaultTransactionCtx :: TransactionCtx
defaultTransactionCtx = TransactionCtx
    { txWithdrawal = NoWithdrawal
    , txMetadata = Nothing
    , txTimeToLive = maxBound
    , txDelegationAction = Nothing
    }

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
