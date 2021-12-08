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
--
module Cardano.Wallet.Transaction
    (
    -- * Interface
      TransactionLayer (..)
    , DelegationAction (..)
    , TransactionCtx (..)
    , defaultTransactionCtx
    , Withdrawal (..)
    , withdrawalToCoin
    , TxUpdate (..)

    -- * Errors
    , ErrSignTx (..)
    , ErrMkTransaction (..)
    , ErrCannotJoin (..)
    , ErrCannotQuit (..)
    , ErrUpdateSealedTx (..)
    , ErrAssignRedeemers(..)
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv, XPub )
import Cardano.Api
    ( AnyCardanoEra )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), DerivationIndex, Passphrase )
import Cardano.Wallet.Primitive.CoinSelection
    ( SelectionCollateralRequirement (..), SelectionOf (..) )
import Cardano.Wallet.Primitive.CoinSelection.Balance
    ( SelectionLimit, SelectionSkeleton )
import Cardano.Wallet.Primitive.Slotting
    ( PastHorizonException, TimeInterpreter )
import Cardano.Wallet.Primitive.Types
    ( PoolId
    , ProtocolParameters
    , SlotNo (..)
    , TokenBundleMaxSize (..)
    , WalletId
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash )
import Cardano.Wallet.Primitive.Types.Redeemer
    ( Redeemer )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( TokenMap )
import Cardano.Wallet.Primitive.Types.Tx
    ( TokenBundleSizeAssessor
    , Tx (..)
    , TxConstraints
    , TxIn
    , TxMetadata
    , TxOut
    )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Text
    ( Text )
import Fmt
    ( Buildable (..), genericF )
import GHC.Generics
    ( Generic )

import qualified Cardano.Api.Shelley as Node
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap

data TransactionLayer k tx = TransactionLayer
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
        -> SelectionOf TxOut
            -- A balanced coin selection where all change addresses have been
            -- assigned.
        -> Either ErrMkTransaction (Tx, tx)
        -- ^ Construct a standard transaction
        --
        -- " Standard " here refers to the fact that we do not deal with redemption,
        -- multisignature transactions, etc.
        --
        -- This expects as a first argument a mean to compute or lookup private
        -- key corresponding to a particular address.

    , addVkWitnesses
        :: AnyCardanoEra
            -- Era for which the transaction should be created.
        -> (XPrv, Passphrase "encryption")
            -- Reward account
        -> (Address -> Maybe (k 'AddressK XPrv, Passphrase "encryption"))
            -- Key store / address resolution
        -> (TxIn -> Maybe Address)
            -- Input resolution
        -> tx
            -- The transaction to sign
        -> tx
        -- ^ Add Vk witnesses to a transaction for known inputs.
        --
        -- If inputs can't be resolved, they are simply skipped, hence why this
        -- function cannot fail.

    , mkUnsignedTransaction
        :: AnyCardanoEra
            -- Era for which the transaction should be created.
        -> XPub
            -- Reward account public key
        -> ProtocolParameters
            -- Current protocol parameters
        -> TransactionCtx
            -- An additional context about the transaction
        -> SelectionOf TxOut
            -- A balanced coin selection where all change addresses have been
            -- assigned.
        -> Either ErrMkTransaction tx
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

    , maxScriptExecutionCost
        :: ProtocolParameters
            -- Current protocol parameters
        -> [Redeemer]
            -- Redeemers for this transaction
        -> Coin
        -- ^ Compute the maximum execution cost of scripts in a given transaction.

    , evaluateMinimumFee
        :: Node.ProtocolParameters
            -- Current protocol parameters
        -> tx
            -- The sealed transaction
        -> Maybe Coin
        -- ^ Evaluate a minimal fee amount necessary to pay for a given tx
        -- using ledger's functionality
        --
        -- Will estimate how many witnesses there /should be/, so it works even
        -- for unsigned transactions.
        --
        -- Returns `Nothing` for ByronEra transactions.

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

    , decodeTx :: tx -> (Tx, TokenMap, TokenMap)
    -- ^ Decode an externally-created transaction.

    , updateTx
        :: tx
        -> TxUpdate
        -> Either ErrUpdateSealedTx tx
        -- ^ Update tx by adding additional inputs and outputs

    , assignScriptRedeemers
        :: Node.ProtocolParameters
            -- Current protocol parameters
        -> TimeInterpreter (Either PastHorizonException)
            -- Time interpreter in the Monad m
        -> (TxIn -> Maybe (TxOut, Maybe (Hash "Datum")))
            -- A input resolver for transactions' inputs containing scripts.
        -> [Redeemer]
            -- A list of redeemers to set on the transaction.
        -> tx
            -- Transaction containing scripts
        -> (Either ErrAssignRedeemers tx)
    }
    deriving Generic

-- | Describes modifications that can be made to a `Tx` using `updateTx`.
data TxUpdate = TxUpdate
    { extraInputs :: [(TxIn, TxOut)]
    , extraCollateral :: [TxIn]
       -- ^ Only used in the Alonzo era and later. Will be silently ignored in
       -- previous eras.
    , extraOutputs :: [TxOut]
    , newFee :: Maybe Coin
        -- ^ Set the new fee, given the old one.
        --
        -- Note that you most likely won't care about the old fee at all. But it
        -- is useful to allow defining a no-op `TxUpdate` for the sake
        -- of testing.
    }

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
    , txPlutusScriptExecutionCost :: Coin
    -- ^ Total execution cost of plutus scripts, determined by their execution units
    -- and prices obtained from network.
    , txAssetsToMint :: TokenMap
    -- ^ The assets to mint.
    , txAssetsToBurn :: TokenMap
    -- ^ The assets to burn.
    , txCollateralRequirement :: SelectionCollateralRequirement
    -- ^ The collateral requirement.
    , txFeePadding :: !Coin
    -- ^ Extra fees. Some parts of a transction are not representable using
    -- cardano-wallet types, which makes it useful to account for them like
    -- this. For instance: datums.
    } deriving (Show, Generic, Eq)

data Withdrawal
    = WithdrawalSelf RewardAccount (NonEmpty DerivationIndex) Coin
    | WithdrawalExternal RewardAccount (NonEmpty DerivationIndex) Coin
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
    , txPlutusScriptExecutionCost = Coin 0
    , txAssetsToMint = TokenMap.empty
    , txAssetsToBurn = TokenMap.empty
    , txCollateralRequirement = SelectionCollateralNotRequired
    , txFeePadding = Coin 0
    }

-- | Whether the user is attempting any particular delegation action.
data DelegationAction = RegisterKeyAndJoin PoolId | Join PoolId | Quit
    deriving (Show, Eq, Generic)

instance Buildable DelegationAction where
    build = genericF

data ErrMkTransaction
    = ErrMkTransactionNoSuchWallet WalletId
    | ErrMkTransactionTxBodyError Text
    -- ^ We failed to construct a transaction for some reasons.
    | ErrMkTransactionInvalidEra AnyCardanoEra
    -- ^ Should never happen, means that that we have programmatically provided
    -- an invalid era.
    | ErrMkTransactionJoinStakePool ErrCannotJoin
    | ErrMkTransactionQuitStakePool ErrCannotQuit
    | ErrMkTransactionIncorrectTTL PastHorizonException
    deriving (Generic, Eq, Show)

data ErrAssignRedeemers
    = ErrAssignRedeemersScriptFailure Redeemer String
    -- ^ Failed to assign execution units for a particular redeemer. The
    -- 'String' indicates the reason of the failure.
    --
    -- TODO: Refine this type to avoid the 'String' and provides a better
    -- sum-type of possible errors.
    | ErrAssignRedeemersTargetNotFound Redeemer
    -- ^ The given redeemer target couldn't be located in the transaction.
    | ErrAssignRedeemersInvalidData Redeemer String
    -- ^ Redeemer's data isn't a valid Plutus' data.
    | ErrAssignRedeemersPastHorizon PastHorizonException
    -- ^ Evaluating the Plutus script failed past the visible horizon.
    deriving (Generic, Eq, Show)

-- | Possible signing error
data ErrSignTx
    = ErrSignTxAddressUnknown TxIn
    -- ^ We tried to sign a transaction with inputs that are unknown to us?
    | ErrSignTxUnimplemented
    -- ^ TODO: [ADP-919] Remove ErrSignTxUnimplemented
    deriving (Generic, Eq, Show)

data ErrCannotJoin
    = ErrAlreadyDelegating PoolId
    | ErrNoSuchPool PoolId
    deriving (Generic, Eq, Show)

data ErrCannotQuit
    = ErrNotDelegatingOrAboutTo
    | ErrNonNullRewards Coin
    deriving (Eq, Show)

data ErrUpdateSealedTx
    = ErrExistingKeyWitnesses Int
    -- ^ The `SealedTx` couldn't not be updated because the *n* existing
    -- key-witnesses would have been rendered invalid.
    | ErrByronTxNotSupported
    deriving (Generic, Eq, Show)
