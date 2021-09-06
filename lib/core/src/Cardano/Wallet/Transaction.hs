{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
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
    , TransactionCtx (..)
    , defaultTransactionCtx
    , Withdrawal (..)
    , withdrawalToCoin
    , withdrawalRewardAccount
    , addResolvedInputs

    -- * Keys and Signing
    , SignTransactionResult (..)
    , SignTransactionWitness (..)
    , SignTransactionWitnessStake (..)
    , DecryptedSigningKey (..)
    , SignTransactionKeyStore (..)
    , keyStoreLookup
    , keyStoreLookupStake

    -- * Errors
    , ErrSignTx (..)
    , ErrMkTransaction (..)
    , ErrCannotJoin (..)
    , ErrCannotQuit (..)
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv )
import Cardano.Api
    ( AnyCardanoEra )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), DerivationIndex, Passphrase )
import Cardano.Wallet.Primitive.CoinSelection.Balance
    ( SelectionLimit, SelectionResult (..), SelectionSkeleton )
import Cardano.Wallet.Primitive.Slotting
    ( PastHorizonException )
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
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( DelegationAction, RewardAccount )
import Cardano.Wallet.Primitive.Types.Tx
    ( TokenBundleSizeAssessor
    , Tx (..)
    , TxConstraints
    , TxIn
    , TxMetadata
    , TxOut
    , txOutCoin
    )
import Data.Bifunctor
    ( Bifunctor (..) )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import Fmt
    ( Buildable (..), blockMapF, genericF, listF' )
import GHC.Generics
    ( Generic )

import qualified Data.List.NonEmpty as NE

data TransactionLayer k tx = TransactionLayer
    { mkTransactionBody
        :: (AnyCardanoEra, ProtocolParameters)
            -- Era and protocol parameters for which the transaction should be
            -- created.
        -> TransactionCtx
            -- Additional context about the transaction.
        -> SelectionResult TxOut
            -- A balanced coin selection where all change addresses have been
            -- assigned.
        -> Either ErrMkTransaction tx
        -- ^ Construct a standard unsigned transaction
        --
        -- " Standard " here refers to the fact that we do not deal with redemption,
        -- multisignature transactions, etc.
        --
        -- The function returns CBOR-ed transaction body to be signed in another step.

    , mkSignedTransaction
        :: SignTransactionKeyStore (k 'AddressK XPrv) XPrv
            -- Key store
        -> tx
            -- serialized unsigned transaction
        -> SignTransactionResult (k 'AddressK XPrv) XPrv () tx
        -- ^ Sign a transaction using the given key store.

    , calcMinimumCost
        :: ProtocolParameters
            -- Current protocol parameters
        -> TransactionCtx
            -- Additional information about the transaction
        -> SelectionSkeleton
            -- An intermediate representation of an ongoing selection
        -> Coin
        -- ^ Compute a minimal transaction fee amount necessary to pay for a
        -- given selection. This amount does not include certificate deposits -
        -- they are calculated separately.

    , computeSelectionLimit
        :: ProtocolParameters
        -> TransactionCtx
        -> [TxOut]
        -> SelectionLimit

    , tokenBundleSizeAssessor :: TokenBundleMaxSize -> TokenBundleSizeAssessor
    -- ^ A function to assess the size of a token bundle.

    , constraints :: ProtocolParameters -> TxConstraints
    -- ^ The set of constraints that apply to all transactions, given the
    -- current protocol parameters.

    , decodeTx :: tx -> Tx
    -- ^ Decode an externally-created transaction.
    }
    deriving Generic

-- | Use coin selection result to provide resolved inputs of transaction.
addResolvedInputs :: SelectionResult change -> Tx -> Tx
addResolvedInputs cs tx = tx
    { resolvedInputs = fmap txOutCoin <$> NE.toList (inputsSelected cs) }

-- | Some additional context about a transaction. This typically contains
-- details that are known upfront about the transaction and are used to
-- construct it from inputs selected from the wallet's UTxO.
data TransactionCtx = TransactionCtx
    { txRewardAccount :: Maybe RewardAccount
    -- ^ Reward account used for withdrawals and delegation actions.
    , txWithdrawal :: Withdrawal
    -- ^ Withdrawal amount from a reward account, can be zero.
    , txDelegationActions :: [DelegationAction]
    -- ^ Joining and/or leaving stake pools.
    , txMetadata :: Maybe TxMetadata
    -- ^ User or application-defined metadata to embed in the transaction.
    , txTimeToLive :: SlotNo
    -- ^ Transaction expiry (TTL) slot.
    } deriving (Show, Generic, Eq)

instance Buildable TransactionCtx where
    build = genericF

-- | A default context with sensible placeholder. Can be used to reduce
-- repetition for changing only sub-part of the default context.
defaultTransactionCtx :: TransactionCtx
defaultTransactionCtx = TransactionCtx
    { txRewardAccount = Nothing
    , txWithdrawal = NoWithdrawal
    , txDelegationActions = mempty
    , txMetadata = Nothing
    , txTimeToLive = maxBound
    }

-- TODO: Split out RewardAccount from Withdrawal
-- | A specification from the user as to whether a transaction should include a
-- reward account withdrawal, and if so, from which reward account.
data Withdrawal
    = WithdrawalSelf !RewardAccount !(NonEmpty DerivationIndex) !Coin
    -- ^ Withdraw from a reward account belonging to this wallet.
    | WithdrawalExternal !RewardAccount !(NonEmpty DerivationIndex) !Coin
    -- ^ Withdraw from a reward account belonging to another wallet.
    | NoWithdrawal
    -- ^ Don't withdraw anything in this transaction.
    deriving (Show, Eq)

instance Buildable Withdrawal where
    build = \case
        NoWithdrawal -> "None"
        WithdrawalSelf acct ix amt -> format "this wallet" acct ix amt
        WithdrawalExternal acct ix amt -> format "other wallet" acct ix amt
      where
        format f acct ix amt = blockMapF
            [ ("From" :: Text, f)
            , ("Reward Account", build acct)
            , ("Derivation Index", listF' (build . toText) ix)
            , ("Amount", build amt)
            ]

-- | The amount expected to be withdrawn, or zero if no withdrawal.
withdrawalToCoin :: Withdrawal -> Coin
withdrawalToCoin = \case
    WithdrawalSelf _ _ c -> c
    WithdrawalExternal _ _ c -> c
    NoWithdrawal -> Coin 0

-- | The reward account to use for this withdrawal (if any).
withdrawalRewardAccount :: Withdrawal -> Maybe RewardAccount
withdrawalRewardAccount = \case
    WithdrawalSelf acct _ _ -> Just acct
    WithdrawalExternal acct _ _ -> Just acct
    NoWithdrawal -> Nothing

-- | A transaction with information about the witnesses (if any) which were
-- added by 'signTransaction`.
data SignTransactionResult psk ssk wit tx = SignTransactionResult
    { tx :: !tx
    , addressWitnesses :: ![SignTransactionWitness psk wit]
    , withdrawalWitnesses :: ![SignTransactionWitnessStake ssk wit]
    , certificateWitnesses :: ![SignTransactionWitnessStake ssk wit]
    } deriving (Show, Eq, Generic, Functor)

instance Bifunctor (SignTransactionResult psk ssk) where
    -- | Map over the transaction type parameter.
    second f (SignTransactionResult t a w c) = SignTransactionResult (f t) a w c
    -- | Map over the witness type parameter.
    first f (SignTransactionResult t a w c) = SignTransactionResult t
            (fmap (fmap f) a) (fmap (fmap f) w) (fmap (fmap f) c)

instance (Buildable wit, Buildable tx) => Buildable (SignTransactionResult psk ssk wit tx) where
    build = genericF

-- | In the wallet, signing keys are symmetrically encrypted at rest using a key
-- derived from the user's spending passphrasee. This data type pairs an
-- encrypted signing key with the passphrase needed to decrypt it.
--
-- We call this type "decrypted", because although the key is still encrypted,
-- the passphrase is right there, so it may as well be decrypted.
--
-- Use 'decryptSigningKey' to actually get the key which can be used for
-- witnessing transactions.
data DecryptedSigningKey sk = DecryptedSigningKey
    { signingKey :: !sk
    -- ^ Encrypted signing key.
    , passphrase :: !(Passphrase "encryption")
    -- ^ The prepared passphrase for decrypting that signing key.
    } deriving (Show, Eq, Generic, Functor)

-- | Produces signing keys for transaction inputs.
data SignTransactionKeyStore psk ssk = SignTransactionKeyStore
    { stakeCreds :: RewardAccount -> Maybe (DecryptedSigningKey ssk)
    -- ^ Optional key credential for withdrawing from the wallet's reward account.
    , resolver :: TxIn -> Maybe Address
    -- ^ A function to lookup the output address from a transaction input.
    , keyFrom :: Address -> Maybe (DecryptedSigningKey psk)
    -- ^ A function to lookup the vkey/bootstrap credential for an address.
    } deriving Generic

instance Buildable (DecryptedSigningKey sk) where
    build _ = "<protected>"

-- | Information about a transaction witness that was added by
-- 'signTransaction', or not added, as the case may be.
data SignTransactionWitness sk wit = SignTransactionWitness
    { txIn :: TxIn
    -- ^ The transaction input being signed for.
    , address :: Maybe Address
    -- ^ The address corresponding to 'txIn', if the address belongs to this
    -- wallet.
    , cred :: !(Maybe (DecryptedSigningKey sk))
    -- ^ Credential used in signing for the address, if the wallet has the
    -- necessary key.
    , witness :: !(Maybe wit)
    -- ^ The actual signature for this transaction input, if it was possible to
    -- produce a witness.
    } deriving (Show, Eq, Generic, Functor)

instance Buildable wit => Buildable (SignTransactionWitness sk wit) where
    build = genericF

keyStoreLookup
    :: SignTransactionKeyStore psk ssk
    -> (DecryptedSigningKey psk -> Address -> wit)
    -> TxIn
    -> SignTransactionWitness psk wit
keyStoreLookup SignTransactionKeyStore{resolver,keyFrom} mkWit txIn =
    case resolver txIn of
        address@(Just addr) -> case keyFrom addr of
            cred@(Just k) -> res {address, cred, witness = Just (mkWit k addr)}
            Nothing -> res {txIn, address}
        Nothing -> res
  where
    res = SignTransactionWitness
        { txIn, address = Nothing, cred = Nothing, witness = Nothing }

-- | Information about a transaction witness that was added by
-- 'signTransaction', or not added, as the case may be. This is similar to
-- 'SignTransactionWitness', except for witnesses involved in the signing of a
-- delegation certificate, or spending delegation rewards.
data SignTransactionWitnessStake k wit = SignTransactionWitnessStake
    { rewardAccount :: RewardAccount
    -- ^ The 'RewardAccount' being signed for.
    , cred :: !(Maybe (DecryptedSigningKey k))
    -- ^ Credential used in signing for the address, if the wallet has the
    -- necessary key.
    , witness :: !(Maybe wit)
    -- ^ The actual signature, if it was possible to produce a witness.
    } deriving (Show, Eq, Generic, Functor)

instance Buildable wit => Buildable (SignTransactionWitnessStake k wit) where
    build = genericF

keyStoreLookupStake
    :: SignTransactionKeyStore psk ssk
    -> (DecryptedSigningKey ssk -> wit)
    -> RewardAccount
    -> SignTransactionWitnessStake ssk wit
keyStoreLookupStake SignTransactionKeyStore{stakeCreds} mkWit rewardAccount =
  SignTransactionWitnessStake { rewardAccount, cred, witness }
  where
    cred = stakeCreds rewardAccount
    witness = mkWit <$> cred

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

-- | Possible signing error
data ErrSignTx
    = ErrSignTxAddressUnknown TxIn
    -- ^ We tried to sign a transaction with inputs that are unknown to us?
    | ErrSignTxKeyNotFound Address
    -- ^ We tried to sign a transaction with inputs that are unknown to us?
    deriving (Generic, Eq, Show)

data ErrCannotJoin
    = ErrAlreadyDelegating PoolId
    | ErrNoSuchPool PoolId
    deriving (Generic, Eq, Show)

data ErrCannotQuit
    = ErrNotDelegatingOrAboutTo
    | ErrNonNullRewards Coin
    deriving (Generic, Eq, Show)
