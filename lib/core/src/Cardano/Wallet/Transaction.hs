{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
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
    , mkTransaction
    , DelegationAction (..)
    , TransactionCtx (..)
    , defaultTransactionCtx
    , Withdrawal (..)
    , withdrawalToCoin
    , withdrawalRewardAccount
    , addResolvedInputs

    -- * Keys and Signing
    , SignTransactionResult (..)
    , SignTransactionWitness (..)
    , DecryptedSigningKey (..)
    , SignTransactionKeyStore (..)
    , keyStoreLookup
    , keyStoreLookupWithdrawal

    -- * Errors
    , ErrSignTx (..)
    , ErrDecodeSignedTx (..)
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
    ( SelectionLimit, SelectionResult, SelectionSkeleton )
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
    ( RewardAccount )
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
import Data.ByteString
    ( ByteString )
import Data.Generics.Internal.VL.Lens
    ( view )
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
        -> Maybe RewardAccount
            -- Hash of stake address public key, if there is one.
        -> TransactionCtx
            -- An additional context about the transaction
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
        :: SignTransactionKeyStore (k 'AddressK XPrv)
            -- Key store
        -> tx
            -- serialized unsigned transaction
        -> SignTransactionResult (k 'AddressK XPrv) () tx
        -- ^ Sign a transaction

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

    , decodeTx
        :: tx
        -> Tx
        -- ^ Decode an externally-signed transaction to the chain producer
    }
    deriving Generic

-- | Construct a standard transaction
--
-- "Standard" here refers to the fact that we do not deal with redemption,
-- multisignature transactions, etc.
--
-- This expects as a first argument a mean to compute or lookup private
-- key corresponding to a particular address.
mkTransaction
    :: TransactionLayer k tx
    -> (AnyCardanoEra, ProtocolParameters)
    -- ^ Era and protocol parameters for which the transaction should be
    -- created.
    -> SignTransactionKeyStore (k 'AddressK XPrv)
    -- ^ Key store
    -> TransactionCtx
    -- ^ An additional context about the transaction
    -> SelectionResult TxOut
    -- ^ A balanced coin selection where all change addresses have been
    -- assigned.
    -> Either ErrMkTransaction (Tx, tx)
mkTransaction tl eraPP keyStore ctx cs = do
    let rewardAcct = withdrawalRewardAccount (txWithdrawal ctx)
    unsigned <- mkTransactionBody tl eraPP rewardAcct ctx cs
    let signed = view #tx $ mkSignedTransaction tl keyStore unsigned
    pure (addResolvedInputs cs (decodeTx tl signed), signed)

-- | Use coin selection to provide resolved inputs of transaction.
addResolvedInputs :: SelectionResult change -> Tx -> Tx
addResolvedInputs cs tx = tx
    { resolvedInputs = fmap txOutCoin <$> NE.toList (inputsSelected cs) }

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
    , txDelegationActions :: [DelegationAction]
    -- ^ Joining and/or leaving stake pools.
    } deriving (Show, Generic, Eq)

instance Buildable TransactionCtx where
    build = genericF

-- | A default context with sensible placeholder. Can be used to reduce
-- repetition for changing only sub-part of the default context.
defaultTransactionCtx :: TransactionCtx
defaultTransactionCtx = TransactionCtx
    { txWithdrawal = NoWithdrawal
    , txMetadata = Nothing
    , txTimeToLive = maxBound
    , txDelegationAction = Nothing
    }

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

withdrawalRewardAccount :: Withdrawal -> Maybe RewardAccount
withdrawalRewardAccount = \case
    WithdrawalSelf acct _ _ -> Just acct
    WithdrawalExternal acct _ _ -> Just acct
    NoWithdrawal -> Nothing

-- | A default context with sensible placeholder. Can be used to reduce
-- repetition for changing only sub-part of the default context.
defaultTransactionCtx :: TransactionCtx
defaultTransactionCtx = TransactionCtx
    { txWithdrawal = NoWithdrawal
    , txMetadata = Nothing
    , txTimeToLive = maxBound
    , txDelegationActions = mempty
    }

-- | Whether the user is attempting any particular delegation action.
data DelegationAction = RegisterKey | Join PoolId | Quit
    deriving (Show, Eq, Generic)

instance Buildable DelegationAction where
    build = genericF

data SignTransactionResult sk wit tx = SignTransactionResult
    { tx :: !tx
    , addressWitnesses :: ![SignTransactionWitness sk wit]
    , withdrawalWitnesses :: ![(RewardAccount, wit)]
    , certificateWitnesses :: ![(RewardAccount, wit)]
    } deriving (Show, Eq, Generic, Functor)

instance Bifunctor (SignTransactionResult sk) where
    first f (SignTransactionResult t a w c) =
        SignTransactionResult t (fmap (fmap f) a) (fmap (fmap f) w) (fmap (fmap f) c)
    second f (SignTransactionResult t a w c) =
        SignTransactionResult (f t) a w c

-- | In the wallet, signing keys are symmetrically encrypted at rest using a key
-- derived from the user's spending passphrasee. This data type pairs an
-- encrypted signing key with the passphrase needed to decrypt it.
--
-- We call this type "decrypted", because although the key is still encrypted, the passphrase is right there, so it may as well be decrypted.
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
data SignTransactionKeyStore k = SignTransactionKeyStore
    { stakeCreds :: RewardAccount -> Maybe (DecryptedSigningKey XPrv)
    -- ^ Optional key credential for withdrawing from the wallet's reward account.
    , resolver :: TxIn -> Maybe Address
    -- ^ A function to lookup the output address from a transaction input.
    , keyFrom :: Address -> Maybe (DecryptedSigningKey k)
    -- ^ A function to lookup the vkey/bootstrap credential for an address.
    } deriving Generic

keyStoreLookup
    :: SignTransactionKeyStore k
    -> (DecryptedSigningKey k -> Address -> wit)
    -> TxIn
    -> SignTransactionWitness k wit
keyStoreLookup SignTransactionKeyStore{resolver,keyFrom} mkWit txIn =
    case resolver txIn of
        address@(Just addr) -> case keyFrom addr of
            cred@(Just k) -> res {address, cred, witness = Just (mkWit k addr)}
            Nothing -> res {txIn, address}
        Nothing -> res
  where
    res = SignTransactionWitness
        { txIn, address = Nothing, cred = Nothing, witness = Nothing }

keyStoreLookupWithdrawal
    :: SignTransactionKeyStore k
    -> (DecryptedSigningKey XPrv -> wit)
    -> RewardAccount
    -> Maybe (RewardAccount, wit)
keyStoreLookupWithdrawal SignTransactionKeyStore{stakeCreds} mkWit acct =
    (acct,) . mkWit <$> stakeCreds acct

data SignTransactionWitness sk wit = SignTransactionWitness
    { txIn :: TxIn
    , address :: Maybe Address
    , cred :: !(Maybe (DecryptedSigningKey sk))
    , witness :: !(Maybe wit)
    } deriving (Show, Eq, Generic, Functor)

instance (Buildable wit, Buildable tx) => Buildable (SignTransactionResult sk wit tx) where
    build = genericF

instance Buildable wit => Buildable (SignTransactionWitness sk wit) where
    build = genericF

instance Buildable (DecryptedSigningKey sk) where
    build _ = "<protected>"

-- | Error while trying to decode externally signed transaction
data ErrDecodeSignedTx
    = ErrDecodeSignedTxWrongPayload Text
    | ErrDecodeSignedTxNotSupported
    deriving (Generic, Show, Eq)

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
