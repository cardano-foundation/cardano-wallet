{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
    , TxValidityInterval
    , TransactionCtx (..)
    , PreSelection (..)
    , defaultTransactionCtx
    , Withdrawal (..)
    , withdrawalToCoin
    , TokenMapWithScripts (..)
    , emptyTokenMapWithScripts
    , AnyExplicitScript (..)
    , AnyScript (..)
    , PlutusScriptInfo (..)
    , PlutusVersion (..)
    , ScriptReference (..)
    , ReferenceInput (..)
    , ValidityIntervalExplicit (..)
    , WitnessCount (..)
    , emptyWitnessCount
    , WitnessCountCtx (..)
    , toKeyRole

    -- * Errors
    , ErrSignTx (..)
    , ErrMkTransaction (..)
    , ErrMkTransactionOutputTokenQuantityExceedsLimitError (..)
    , ErrCannotJoin (..)
    , ErrCannotQuit (..)
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv, XPub )
import Cardano.Address.Script
    ( KeyHash (..), KeyRole (..), Script, ScriptHash, ScriptTemplate )
import Cardano.Api
    ( AnyCardanoEra )
import Cardano.Api.Extra
    ()
import Cardano.Pool.Types
    ( PoolId )
import Cardano.Wallet.Address.Derivation
    ( Depth (..), DerivationIndex )
import Cardano.Wallet.Primitive.Passphrase.Types
    ( Passphrase )
import Cardano.Wallet.Primitive.Slotting
    ( PastHorizonException )
import Cardano.Wallet.Primitive.Types
    ( Certificate, ProtocolParameters, SlotNo (..) )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId, TokenMap )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenPolicyId )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity )
import Cardano.Wallet.Primitive.Types.Tx.Tx
    ( Tx (..), TxMetadata )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn (..) )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
    ( TxOut (..) )
import Cardano.Wallet.TxWitnessTag
    ( TxWitnessTag )
import Control.DeepSeq
    ( NFData (..) )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Map.Strict
    ( Map )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..), TextDecodingError (..), ToText (..) )
import Data.Word
    ( Word64, Word8 )
import Fmt
    ( Buildable (..), genericF )
import GHC.Generics
    ( Generic )

import qualified Cardano.Api as Cardano
import qualified Cardano.Tx.Balance.Internal.CoinSelection as CS.Internal
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Cardano.Wallet.Write.Tx as Write
import qualified Data.List as L
import qualified Data.Map.Strict as Map

data TransactionLayer k ktype tx = TransactionLayer
    { mkTransaction
        :: AnyCardanoEra
            -- Era for which the transaction should be created.
        -> (XPrv, Passphrase "encryption")
            -- Reward account
        -> (Address -> Maybe (k 'CredFromKeyK XPrv, Passphrase "encryption"))
            -- Key store
        -> ProtocolParameters
            -- Current protocol parameters
        -> TransactionCtx
            -- An additional context about the transaction
        -> CS.Internal.SelectionOf TxOut
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
            -- Preferred latest era
        -> WitnessCountCtx
        -> [(XPrv, Passphrase "encryption")]
            -- Reward accounts
        -> Maybe (KeyHash, XPrv, Passphrase "encryption")
            -- policy key hash and private key
        -> Maybe (KeyHash, XPrv, Passphrase "encryption")
            -- optional staking key hash and private key
        -> (Address -> Maybe (k ktype XPrv, Passphrase "encryption"))
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
        :: forall era
         . Write.IsRecentEra era
        => Either XPub (Maybe (Script KeyHash))
            -- Reward account public key or optional script hash
        -> TransactionCtx
            -- An additional context about the transaction
        -> Either PreSelection (CS.Internal.SelectionOf TxOut)
            -- A balanced coin selection where all change addresses have been
            -- assigned.
        -> Either ErrMkTransaction (Cardano.TxBody era)
        -- ^ Construct a standard unsigned transaction
        --
        -- " Standard " here refers to the fact that we do not deal with redemption,
        -- multisignature transactions, etc.
        --
        -- The function returns CBOR-ed transaction body to be signed in another step.

    , decodeTx
        :: AnyCardanoEra
        -> WitnessCountCtx
        -> tx ->
            ( Tx
            , TokenMapWithScripts
            , TokenMapWithScripts
            , [Certificate]
            , Maybe ValidityIntervalExplicit
            , WitnessCount
            )
    -- ^ Decode an externally-created transaction.

    , transactionWitnessTag :: TxWitnessTag
    }

type TxValidityInterval = (Maybe SlotNo, SlotNo)

-- | Some additional context about a transaction. This typically contains
-- details that are known upfront about the transaction and are used to
-- construct it from inputs selected from the wallet's UTxO.
data TransactionCtx = TransactionCtx
    { txWithdrawal :: Withdrawal
    -- ^ Withdrawal amount from a reward account, can be zero.
    , txMetadata :: Maybe TxMetadata
    -- ^ User or application-defined metadata to embed in the transaction.
    , txValidityInterval :: TxValidityInterval
    -- ^ Transaction optional starting slot and expiry (TTL) slot for which the
    -- transaction is valid.
    , txDelegationAction :: Maybe DelegationAction
    -- ^ An additional delegation to take.
    , txAssetsToMint :: (TokenMap, Map AssetId (Script KeyHash))
    -- ^ The assets to mint.
    , txAssetsToBurn :: (TokenMap, Map AssetId (Script KeyHash))
    -- ^ The assets to burn.
    , txPaymentCredentialScriptTemplate :: Maybe ScriptTemplate
    -- ^ Script template regulating payment credentials
    , txStakingCredentialScriptTemplate :: Maybe ScriptTemplate
    -- ^ Script template regulating delegation credentials
    , txNativeScriptInputs :: Map TxIn (Script KeyHash)
    -- ^ A map of script hashes related to inputs. Only for multisig wallets
    , txCollateralRequirement :: CS.Internal.SelectionCollateralRequirement
    -- ^ The collateral requirement.
    } deriving Generic

-- | Represents a preliminary selection of tx outputs typically made by user.
newtype PreSelection = PreSelection { outputs :: [TxOut] }
    deriving stock (Generic, Show)
    deriving newtype (Eq)

data Withdrawal
    = WithdrawalSelf RewardAccount (NonEmpty DerivationIndex) Coin
    | WithdrawalExternal
        RewardAccount
        (NonEmpty DerivationIndex)
        Coin
        XPrv
        -- ^ The 'XPrv' to be used for signing. Must be unencrypted.
    | NoWithdrawal

withdrawalToCoin :: Withdrawal -> Coin
withdrawalToCoin = \case
    WithdrawalSelf _ _ c -> c
    WithdrawalExternal _ _ c _ -> c
    NoWithdrawal -> Coin 0

-- | A default context with sensible placeholder. Can be used to reduce
-- repetition for changing only sub-part of the default context.
defaultTransactionCtx :: TransactionCtx
defaultTransactionCtx = TransactionCtx
    { txWithdrawal = NoWithdrawal
    , txMetadata = Nothing
    , txValidityInterval = (Nothing, maxBound)
    , txDelegationAction = Nothing
    , txAssetsToMint = (TokenMap.empty, Map.empty)
    , txAssetsToBurn = (TokenMap.empty, Map.empty)
    , txPaymentCredentialScriptTemplate = Nothing
    , txStakingCredentialScriptTemplate = Nothing
    , txNativeScriptInputs = Map.empty
    , txCollateralRequirement = CS.Internal.SelectionCollateralNotRequired
    }

-- | User-requested action related to a delegation
-- that is taken into account when constructing a transaction.
data DelegationAction
    = JoinRegisteringKey PoolId
    -- ^ Join stake pool, registering stake key.
    | Join PoolId
    -- ^ Join stake pool, assuming that stake key has been registered before.
    | Quit
    -- ^ Quit all stake pools
    deriving (Show, Eq, Generic)

instance Buildable DelegationAction where
    build = genericF

data PlutusVersion = PlutusVersionV1 | PlutusVersionV2 | PlutusVersionV3
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

instance ToText PlutusVersion where
    toText PlutusVersionV1 = "v1"
    toText PlutusVersionV2 = "v2"
    toText PlutusVersionV3 = "v3"

instance FromText PlutusVersion where
    fromText txt = case txt of
        "v1" -> Right PlutusVersionV1
        "v2" -> Right PlutusVersionV2
        "v3" -> Right PlutusVersionV3
        _ -> Left $ TextDecodingError $ unwords
            [ "I couldn't parse the given plutus version."
            , "I am expecting one of the words 'v1' or"
            , "'v2'."]

data PlutusScriptInfo = PlutusScriptInfo
    { languageVersion :: PlutusVersion
    , scriptHash :: ScriptHash
    }
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

newtype ReferenceInput = ReferenceInput TxIn
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

-- | ScriptReference depicts whether the script is referenced via spending
-- and is bound to be used in the same transaction or is referenced via
-- reference inputs and is to be used in other transactions. The the latter
-- case the script is referenced in other trasactions
data ScriptReference =
      ViaSpending
    | ViaReferenceInput ReferenceInput
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

data AnyScript =
      NativeScript !(Script KeyHash) !ScriptReference
    | PlutusScript !PlutusScriptInfo !ScriptReference
    | AnyScriptReference !ScriptHash ![ReferenceInput]
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

data TokenMapWithScripts = TokenMapWithScripts
    { txTokenMap :: !TokenMap
    , txScripts :: !(Map TokenPolicyId AnyScript)
    } deriving (Show, Generic, Eq)

emptyTokenMapWithScripts :: TokenMapWithScripts
emptyTokenMapWithScripts = TokenMapWithScripts
    { txTokenMap = mempty
    , txScripts = Map.empty
    }

data AnyExplicitScript =
      NativeExplicitScript !(Script KeyHash) !ScriptReference
    | PlutusExplicitScript !PlutusScriptInfo !ScriptReference
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

data WitnessCount = WitnessCount
    { verificationKey :: Word8
    , scripts :: [AnyExplicitScript]
    , bootstrap :: Word8
    }
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

emptyWitnessCount :: WitnessCount
emptyWitnessCount = WitnessCount
    { verificationKey = 0
    , scripts = []
    , bootstrap = 0
    }

-- WitnessCount context is needed to differentiate verification keys present
-- in native scripts.
-- In shelley wallets they could be present due to only policy verification key.
-- In multisig wallet they could stem from payment, policy and delegation roles,
-- and as minting/burning and delegation support comes will be extended in additional
-- data attached in SharedWalletCtx to differentiate that.
-- WitnessCount is needed only during or after signing, in other phases it is not used.
data WitnessCountCtx =
      ShelleyWalletCtx KeyHash -- Policy
    | SharedWalletCtx [KeyHash] -- Delegation key hashes of all cosigners
    | AnyWitnessCountCtx
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

toKeyRole :: WitnessCountCtx -> Hash "VerificationKey" -> KeyRole
toKeyRole witCtx (Hash key) = case witCtx of
    ShelleyWalletCtx (KeyHash _ mypolicykey) ->
        if key == mypolicykey then
            Policy
        else
            Unknown
    SharedWalletCtx stakingKeyHashes ->
        let toStakeKey (KeyHash _ k) = k
            isStakeKey = L.elem key (map toStakeKey stakingKeyHashes)
        in if isStakeKey then
               Delegation
           else
               Payment
    AnyWitnessCountCtx -> Unknown

data ErrMkTransaction
    =  ErrMkTransactionTxBodyError Text
    -- ^ We failed to construct a transaction for some reasons.
    | ErrMkTransactionOutputTokenQuantityExceedsLimit
        ErrMkTransactionOutputTokenQuantityExceedsLimitError
    | ErrMkTransactionInvalidEra AnyCardanoEra
    -- ^ Should never happen, means that that we have programmatically provided
    -- an invalid era.
    | ErrMkTransactionJoinStakePool ErrCannotJoin
    | ErrMkTransactionQuitStakePool ErrCannotQuit
    | ErrMkTransactionIncorrectTTL PastHorizonException
    deriving (Generic, Eq, Show)

data ErrMkTransactionOutputTokenQuantityExceedsLimitError =
    ErrMkTransactionOutputTokenQuantityExceedsLimitError
    { address :: Address
      -- ^ The address to which this token quantity was to be sent.
    , asset :: AssetId
      -- ^ The asset identifier to which this token quantity corresponds.
    , quantity :: TokenQuantity
      -- ^ The token quantity that exceeded the bound.
    , quantityMaxBound :: TokenQuantity
      -- ^ The maximum allowable token quantity.
    }
    deriving (Eq, Generic, Show)

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

data ValidityIntervalExplicit = ValidityIntervalExplicit
    { invalidBefore :: !(Quantity "slot" Word64)
    , invalidHereafter :: !(Quantity "slot" Word64)
    }
    deriving (Generic, Eq, Show)
    deriving anyclass NFData
