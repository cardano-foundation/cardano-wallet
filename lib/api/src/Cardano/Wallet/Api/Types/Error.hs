{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

-- |
-- Copyright: © 2018-2022 IOHK, 2023 Cardano Foundation
-- License: Apache-2.0
--
-- API error types.
--
module Cardano.Wallet.Api.Types.Error
    (
    -- * General API error types
      ApiError (..)
    , ApiErrorInfo (..)
    , ApiErrorMessage (..)

    -- * Specific API error types
    , ApiErrorSharedWalletNoSuchCosigner (..)
    , ApiErrorTxOutputLovelaceInsufficient (..)
    , ApiErrorBalanceTxUnderestimatedFee (..)
    , ApiErrorNodeNotYetInRecentEra (..)
    , ApiErrorNotEnoughMoney (..)
    , ApiErrorNotEnoughMoneyShortfall (..)
    , ApiErrorMissingWitnessesInTransaction (..)
    , ApiErrorNoSuchPool (..)
    , ApiErrorNoSuchTransaction (..)
    , ApiErrorNoSuchWallet (..)
    , ApiErrorOutputTokenBundleSizeExceedsLimit (..)
    , ApiErrorOutputTokenQuantityExceedsLimit (..)
    , ApiErrorStartTimeLaterThanEndTime (..)
    , ApiErrorTransactionAlreadyInLedger (..)
    , ApiErrorUnsupportedEra (..)
    , ApiErrorWrongEncryptionPassphrase (..)
    )
    where

import Prelude

import Cardano.Wallet.Api.Lib.Options
    ( DefaultRecord (..)
    , defaultSumTypeOptions
    )
import Cardano.Wallet.Api.Types
    ( ApiCosignerIndex (..)
    , ApiCredentialType (..)
    , ApiT
    )
import Cardano.Wallet.Api.Types.Amount
    ( ApiAmount
    )
import Cardano.Wallet.Api.Types.Era
    ( ApiEra
    )
import Cardano.Wallet.Api.Types.WalletAssets
    ( ApiWalletAssets
    )
import Cardano.Wallet.Primitive.Types
    ( WalletId
    )
import Cardano.Wallet.Primitive.Types.AssetName
    ( AssetName
    )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash
    )
import Cardano.Wallet.Primitive.Types.Pool
    ( PoolId
    )
import Cardano.Wallet.Primitive.Types.TokenPolicyId
    ( TokenPolicyId
    )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity
    )
import Control.DeepSeq
    ( NFData (..)
    )
import Data.Aeson
    ( genericParseJSON
    , genericToJSON
    )
import Data.Aeson.Extra
    ( objectUnion
    )
import Data.Aeson.Types
    ( FromJSON (..)
    , Options (..)
    , SumEncoding (..)
    , ToJSON (..)
    )
import Data.Data
    ( Data
    )
import Data.Maybe
    ( fromMaybe
    )
import Data.Set
    ( Set
    )
import Data.Text
    ( Text
    )
import Data.Time.Clock
    ( UTCTime
    )
import Data.Typeable
    ( Typeable
    )
import Data.Word
    ( Word32
    )
import GHC.Generics
    ( Generic
    )
import Numeric.Natural
    ( Natural
    )

data ApiError = ApiError
    { info :: !ApiErrorInfo
    , message :: !ApiErrorMessage
    }
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

instance ToJSON ApiError where
    toJSON ApiError {info, message}
        = fromMaybe (error "ToJSON ApiError: Unexpected encoding")
        $ toJSON info `objectUnion` toJSON message

instance FromJSON ApiError where
    parseJSON o = ApiError <$> parseJSON o <*> parseJSON o

newtype ApiErrorMessage = ApiErrorMessage {message :: Text}
    deriving (Eq, Generic, Show)
    deriving (FromJSON, ToJSON) via DefaultRecord ApiErrorMessage
    deriving anyclass NFData

data ApiErrorInfo
    = AddressAlreadyExists
    | AlreadyWithdrawing
    | AssetNameTooLong
    | AssetNotPresent
    | BadRequest
    | BalanceTxConflictingNetworks
    | BalanceTxExistingCollateral
    | BalanceTxExistingKeyWitnesses
    | BalanceTxExistingReturnCollateral
    | BalanceTxExistingTotalCollateral
    | BalanceTxInternalError
    | BalanceTxUnderestimatedFee
        ApiErrorBalanceTxUnderestimatedFee
    | CannotCoverFee
    | CreatedInvalidTransaction
    | CreatedMultiaccountTransaction
    | CreatedMultidelegationTransaction
    | CreatedWrongPolicyScriptTemplate
    | DelegationInvalid
    | ExistingKeyWitnesses
    | ForeignTransaction
    | HardenedDerivationRequired
    | InputResolutionConflicts
    | InputsDepleted
    | InsufficientCollateral
    | InvalidCoinSelection
    | InvalidMetadataEncryption
    | InvalidValidityBounds
    | InvalidWalletType
    | KeyNotFoundForAddress
    | MalformedTxPayload
    | MempoolIsFull
    | MethodNotAllowed
    | MinWithdrawalWrong
    | MintOrBurnAssetQuantityOutOfBounds
    | MissingPolicyPublicKey
    | MissingRewardAccount
    | MissingWitnessesInTransaction
        !ApiErrorMissingWitnessesInTransaction
    | NetworkMisconfigured
    | NetworkQueryFailed
    | NetworkUnreachable
    | NoRootKey
    | NoSuchPool
        !ApiErrorNoSuchPool
    | NoSuchTransaction
        !ApiErrorNoSuchTransaction
    | NoSuchWallet
        !ApiErrorNoSuchWallet
    | WalletNotInitialized
    | NoUtxosAvailable
    | NodeNotYetInRecentEra
        !ApiErrorNodeNotYetInRecentEra
    | NonNullRewards
    | NotAcceptable
    | NotDelegatingTo
    | NotEnoughMoney
        !ApiErrorNotEnoughMoney
    | NotFound
    | NotImplemented
    | NotSynced
    | NothingToMigrate
    | OutputTokenBundleSizeExceedsLimit
        !ApiErrorOutputTokenBundleSizeExceedsLimit
    | OutputTokenQuantityExceedsLimit
        !ApiErrorOutputTokenQuantityExceedsLimit
    | PastHorizon
    | PoolAlreadyJoined
    | PoolAlreadyJoinedSameVote
    | QueryParamMissing
    | RedeemerInvalidData
    | RedeemerScriptFailure
    | RedeemerTargetNotFound
    | RejectedByCoreNode
    | SameVote
    | SharedWalletActive
    | SharedWalletCannotUpdateKey
    | SharedWalletIncomplete
    | SharedWalletKeyAlreadyExists
    | SharedWalletNoDelegationTemplate
    | SharedWalletNoSuchCosigner
        !ApiErrorSharedWalletNoSuchCosigner
    | SharedWalletScriptTemplateInvalid
    | SoftDerivationRequired
    | StartTimeLaterThanEndTime
        !ApiErrorStartTimeLaterThanEndTime
    | TokensMintedButNotSpentOrBurned
    | TransactionAlreadyBalanced
    | TransactionAlreadyInLedger
        !ApiErrorTransactionAlreadyInLedger
    | TransactionIsTooBig
    | TranslationError
    | TxNotInNodeEra
    | UnableToAssignInputOutput
    | UnableToDetermineCurrentEpoch
    | UnexpectedError
    | UnresolvedInputs
    | UnsupportedMediaType
    | UtxoTooSmall
        !ApiErrorTxOutputLovelaceInsufficient
    | ValidityIntervalNotInsideScriptTimelock
    | VotingInInvalidEra
    | WalletAlreadyExists
    | WalletMetadataNotFound
    | WalletNotResponding
    | WithdrawalNotBeneficial
    | WrongEncryptionPassphrase
        !ApiErrorWrongEncryptionPassphrase
    | WithdrawalNotPossibleWithoutVote
    | WrongMnemonic
    | BlockHeaderNotFound
    | TranslationByronTxOutInContext
    | BalanceTxInlinePlutusV3ScriptNotSupportedInBabbage
    | UnsupportedEra !ApiErrorUnsupportedEra

    deriving (Eq, Generic, Show, Data, Typeable)
    deriving anyclass NFData

instance FromJSON ApiErrorInfo where
    parseJSON = genericParseJSON apiErrorInfoOptions

instance ToJSON ApiErrorInfo where
    toJSON = genericToJSON apiErrorInfoOptions

apiErrorInfoOptions :: Options
apiErrorInfoOptions = defaultSumTypeOptions
    { sumEncoding = TaggedObject
        { tagFieldName = "code"
        , contentsFieldName = "info"
        }
    }

data ApiErrorUnsupportedEra = ApiErrorUnsupportedEra
    { unsupportedEra :: !ApiEra
    -- ^ The unsupported era (as specified by the caller).
    , supportedEras :: !(Set ApiEra)
    -- ^ The set of eras that we currently support.
    }
    deriving (Data, Eq, Generic, Show, Typeable)
    deriving (FromJSON, ToJSON) via DefaultRecord ApiErrorUnsupportedEra
    deriving anyclass NFData

data ApiErrorSharedWalletNoSuchCosigner = ApiErrorSharedWalletNoSuchCosigner
    { cosignerIndex
        :: !ApiCosignerIndex
    , credentialType
        :: !ApiCredentialType
    }
    deriving (Data, Eq, Generic, Show, Typeable)
    deriving (FromJSON, ToJSON)
        via DefaultRecord ApiErrorSharedWalletNoSuchCosigner
    deriving anyclass NFData

data ApiErrorTxOutputLovelaceInsufficient = ApiErrorTxOutputLovelaceInsufficient
    { txOutputIndex
        :: !Word32
    , txOutputLovelaceSpecified
        :: !ApiAmount
    , txOutputLovelaceRequiredMinimum
        :: !ApiAmount
    }
    deriving (Data, Eq, Generic, Show, Typeable)
    deriving (FromJSON, ToJSON)
        via DefaultRecord ApiErrorTxOutputLovelaceInsufficient
    deriving anyclass NFData

data ApiErrorOutputTokenQuantityExceedsLimit =
    ApiErrorOutputTokenQuantityExceedsLimit
        { address
            :: !Text
        , policyId
            :: !(ApiT TokenPolicyId)
        , assetName
            :: !(ApiT AssetName)
        , quantity
            :: !TokenQuantity
        , maxQuantity
            :: !TokenQuantity
        }
    deriving (Data, Eq, Generic, Show, Typeable)
    deriving (FromJSON, ToJSON)
        via DefaultRecord ApiErrorOutputTokenQuantityExceedsLimit
    deriving anyclass NFData

data ApiErrorOutputTokenBundleSizeExceedsLimit =
    ApiErrorOutputTokenBundleSizeExceedsLimit
        { address
            :: !Text
        , bundleSize
            :: !Natural
        }
    deriving (Data, Eq, Generic, Show, Typeable)
    deriving (FromJSON, ToJSON)
        via DefaultRecord ApiErrorOutputTokenBundleSizeExceedsLimit
    deriving anyclass NFData

data ApiErrorBalanceTxUnderestimatedFee = ApiErrorBalanceTxUnderestimatedFee
    { underestimation :: !ApiAmount
    , estimatedNumberOfKeyWits :: Natural
    , estimatedNumberOfBootstrapKeyWits :: Natural
    , candidateTxHex :: Text
    , candidateTxReadable :: Text
    }
    deriving (Data, Eq, Generic, Show, Typeable)
    deriving (FromJSON, ToJSON)
        via DefaultRecord ApiErrorBalanceTxUnderestimatedFee
    deriving anyclass NFData

data ApiErrorNodeNotYetInRecentEra = ApiErrorNodeNotYetInRecentEra
    { nodeEra :: ApiEra
    , supportedRecentEras :: Set ApiEra
    }
    deriving (Data, Eq, Generic, Show, Typeable)
    deriving (FromJSON, ToJSON)
        via DefaultRecord ApiErrorNodeNotYetInRecentEra
    deriving anyclass NFData

data ApiErrorMissingWitnessesInTransaction =
    ApiErrorMissingWitnessesInTransaction
        { expectedNumberOfKeyWits :: !Natural
        , detectedNumberOfKeyWits :: !Natural
        }
    deriving (Data, Eq, Generic, Show, Typeable)
    deriving (FromJSON, ToJSON)
        via DefaultRecord ApiErrorMissingWitnessesInTransaction
    deriving anyclass NFData

data ApiErrorNotEnoughMoney = ApiErrorNotEnoughMoney
    { shortfall :: !ApiErrorNotEnoughMoneyShortfall
    }
    deriving (Data, Eq, Generic, Show, Typeable)
    deriving (FromJSON, ToJSON) via DefaultRecord ApiErrorNotEnoughMoney
    deriving anyclass NFData

data ApiErrorNotEnoughMoneyShortfall = ApiErrorNotEnoughMoneyShortfall
    { ada :: !ApiAmount
    , assets :: !ApiWalletAssets
    }
    deriving (Data, Eq, Generic, Show, Typeable)
    deriving (FromJSON, ToJSON) via
        DefaultRecord ApiErrorNotEnoughMoneyShortfall
    deriving anyclass NFData

data ApiErrorNoSuchPool = ApiErrorNoSuchPool
    { poolId :: !PoolId
    }
    deriving (Data, Eq, Generic, Show, Typeable)
    deriving (FromJSON, ToJSON) via DefaultRecord ApiErrorNoSuchPool
    deriving anyclass NFData

data ApiErrorNoSuchWallet = ApiErrorNoSuchWallet
    { walletId :: !(ApiT WalletId)
    }
    deriving (Data, Eq, Generic, Show, Typeable)
    deriving (FromJSON, ToJSON) via DefaultRecord ApiErrorNoSuchWallet
    deriving anyclass NFData

data ApiErrorNoSuchTransaction = ApiErrorNoSuchTransaction
    { transactionId :: !(ApiT (Hash "Tx"))
    }
    deriving (Data, Eq, Generic, Show, Typeable)
    deriving (FromJSON, ToJSON) via DefaultRecord ApiErrorNoSuchTransaction
    deriving anyclass NFData

data ApiErrorTransactionAlreadyInLedger = ApiErrorTransactionAlreadyInLedger
    { transactionId :: !(ApiT (Hash "Tx"))
    }
    deriving (Data, Eq, Generic, Show, Typeable)
    deriving (FromJSON, ToJSON)
        via DefaultRecord ApiErrorTransactionAlreadyInLedger
    deriving anyclass NFData

data ApiErrorStartTimeLaterThanEndTime = ApiErrorStartTimeLaterThanEndTime
    { startTime :: UTCTime
    , endTime :: UTCTime
    }
    deriving (Data, Eq, Generic, Show, Typeable)
    deriving (FromJSON, ToJSON)
        via DefaultRecord ApiErrorStartTimeLaterThanEndTime
    deriving anyclass NFData

data ApiErrorWrongEncryptionPassphrase = ApiErrorWrongEncryptionPassphrase
    { walletId :: !(ApiT WalletId)
    }
    deriving (Data, Eq, Generic, Show, Typeable)
    deriving (FromJSON, ToJSON)
        via DefaultRecord ApiErrorWrongEncryptionPassphrase
    deriving anyclass NFData
