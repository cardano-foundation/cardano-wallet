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
import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelection (..) )
import Cardano.Wallet.Primitive.Fee
    ( Fee, FeePolicy )
import Cardano.Wallet.Primitive.Types
    ( PoolId, SlotNo (..) )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( Metadata, SealedTx (..), Tx (..) )
import Data.ByteString
    ( ByteString )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Word
    ( Word16, Word8 )
import GHC.Generics
    ( Generic )

data TransactionLayer k = TransactionLayer
    { mkStdTx
        :: AnyCardanoEra
            -- Era for which the transaction should be created.
        -> (XPrv, Passphrase "encryption")
            -- Reward account
        -> (Address -> Maybe (k 'AddressK XPrv, Passphrase "encryption"))
            -- Key store
        -> SlotNo
            -- Transaction expiry (TTL) slot.
        -> Maybe Metadata
            -- User or application-defined metadata to embed in the transaction.
        -> CoinSelection
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

    , mkDelegationJoinTx
        :: AnyCardanoEra
            -- Era for which the transaction should be created.
        -> PoolId
            -- Pool Id to which we're planning to delegate
        -> (XPrv, Passphrase "encryption")
            -- Reward account
        -> (Address -> Maybe (k 'AddressK XPrv, Passphrase "encryption"))
            -- Key store
        -> SlotNo
            -- Transaction expiry (TTL) slot.
        -> CoinSelection
            -- A balanced coin selection where all change addresses have been
            -- assigned.
        -> Either ErrMkTx (Tx, SealedTx)
        -- ^ Construct a transaction containing a certificate for delegating to
        -- a stake pool.
        --
        -- The certificate is a combination of the 'PoolId' and the public key
        -- of the reward account. (Note that this is an address key and
        -- HD account keys are something different)

    , mkDelegationQuitTx
        :: AnyCardanoEra
            -- Era for which the transaction should be created.
        -> (XPrv, Passphrase "encryption")
            -- Reward account
        -> (Address -> Maybe (k 'AddressK XPrv, Passphrase "encryption"))
            -- Key store
        -> SlotNo
            -- Transaction expiry (TTL) slot.
        -> CoinSelection
            -- A balanced coin selection where all change addresses have been
            -- assigned.
        -> Either ErrMkTx (Tx, SealedTx)
        -- ^ Construct a transaction containing a certificate for quiting from
        -- a stake pool.
        --
        -- The certificate is the public key of the reward account.

    , initDelegationSelection
        :: FeePolicy
            -- Current fee policy
        -> DelegationAction
            -- What sort of action is going on
        -> CoinSelection
        -- ^ An initial selection where 'deposit' and/or 'reclaim' have been set
        -- accordingly.

    , minimumFee
        :: FeePolicy
        -> Maybe DelegationAction
        -> Maybe Metadata
        -> CoinSelection
        -> Fee
        -- ^ Compute a minimal fee amount necessary to pay for a given
        -- coin-selection.

    , estimateMaxNumberOfInputs
        :: Quantity "byte" Word16
            -- Max tx size
        -> Maybe Metadata
            -- Metadata associated with the transaction
        -> Word8
            -- desired number of outputs
        -> Word8
        -- ^ Calculate a "theoretical" maximum number of inputs given a maximum
        -- transaction size and desired number of outputs.
        --
        -- The actual transaction size cannot be known until it has been fully
        -- determined by coin selection.
        --
        -- This estimate will err on the side of permitting more inputs,
        -- resulting in a transaction which may be too large.

    , decodeSignedTx
        :: AnyCardanoEra
        -> ByteString
        -> Either ErrDecodeSignedTx (Tx, SealedTx)
        -- ^ Decode an externally-signed transaction to the chain producer
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
