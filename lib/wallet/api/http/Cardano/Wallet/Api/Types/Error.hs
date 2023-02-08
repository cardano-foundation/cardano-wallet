{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

-- |
-- Copyright: © 2018-2022 IOHK
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
    )
    where

import Prelude

import Cardano.Wallet.Api.Lib.Options
    ( DefaultRecord (..), defaultSumTypeOptions )
import Cardano.Wallet.Api.Types
    ( ApiCosignerIndex (..), ApiCredentialType (..) )
import Control.DeepSeq
    ( NFData (..) )
import Data.Aeson
    ( genericParseJSON, genericToJSON )
import Data.Aeson.Extra
    ( objectUnion )
import Data.Aeson.Types
    ( FromJSON (..), Options (..), SumEncoding (..), ToJSON (..) )
import Data.Data
    ( Data )
import Data.Maybe
    ( fromMaybe )
import Data.Quantity
    ( Quantity )
import Data.Text
    ( Text )
import Data.Typeable
    ( Typeable )
import Data.Word
    ( Word32 )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )

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
    | BalanceTxEraNotSupported
    | BalanceTxExistingCollateral
    | BalanceTxExistingKeyWitnesses
    | BalanceTxExistingReturnCollateral
    | BalanceTxExistingTotalCollateral
    | BalanceTxInlineDatumsNotSupportedInAlonzo
    | BalanceTxInlineScriptsNotSupportedInAlonzo
    | BalanceTxInternalError
    | BalanceTxMaxSizeLimitExceeded
    | BalanceTxUnderestimatedFee
    | CannotCoverFee
    | CreatedInvalidTransaction
    | CreatedMultiaccountTransaction
    | CreatedMultidelegationTransaction
    | CreatedWrongPolicyScriptTemplate
    | ExistingKeyWitnesses
    | ForeignTransaction
    | HardenedDerivationRequired
    | InputResolutionConflicts
    | InputsDepleted
    | InsufficientCollateral
    | InvalidCoinSelection
    | InvalidValidityBounds
    | InvalidWalletType
    | KeyNotFoundForAddress
    | MalformedTxPayload
    | MempoolIsFull
    | MethodNotAllowed
    | MinWithdrawalWrong
    | MintOrBurnAssetQuantityOutOfBounds
    | MissingPolicyPublicKey
    | MissingWitnessesInTransaction
    | NetworkMisconfigured
    | NetworkQueryFailed
    | NetworkUnreachable
    | NoRootKey
    | NoSuchPool
    | NoSuchTransaction
    | NoSuchWallet
    | NonNullRewards
    | NotAcceptable
    | NotDelegatingTo
    | NotEnoughMoney
    | NotFound
    | NotImplemented
    | NotSynced
    | NothingToMigrate
    | OutputTokenBundleSizeExceedsLimit
    | OutputTokenQuantityExceedsLimit
    | PastHorizon
    | PoolAlreadyJoined
    | QueryParamMissing
    | RedeemerInvalidData
    | RedeemerScriptFailure
    | RedeemerTargetNotFound
    | RejectedByCoreNode
    | SharedWalletActive
    | SharedWalletCannotUpdateKey
    | SharedWalletIncomplete
    | SharedWalletKeyAlreadyExists
    | SharedWalletNoDelegationTemplate
    | SharedWalletNoSuchCosigner
        !ApiErrorSharedWalletNoSuchCosigner
    | SharedWalletScriptTemplateInvalid
    | SoftDerivationRequired
    | StakingInvalid
    | StartTimeLaterThanEndTime
    | TokensMintedButNotSpentOrBurned
    | TransactionAlreadyBalanced
    | TransactionAlreadyInLedger
    | TransactionIsTooBig
    | TranslationError
    | UnableToAssignInputOutput
    | UnableToDetermineCurrentEpoch
    | UnexpectedError
    | UnresolvedInputs
    | UnsupportedMediaType
    | UtxoTooSmall
        !ApiErrorTxOutputLovelaceInsufficient
    | ValidityIntervalNotInsideScriptTimelock
    | WalletAlreadyExists
    | WalletMetadataNotFound
    | WalletNotResponding
    | WithdrawalNotBeneficial
    | WrongEncryptionPassphrase
    | WrongMnemonic
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
        :: !(Quantity "lovelace" Natural)
    , txOutputLovelaceRequiredMinimum
        :: !(Quantity "lovelace" Natural)
    }
    deriving (Data, Eq, Generic, Show, Typeable)
    deriving (FromJSON, ToJSON)
        via DefaultRecord ApiErrorTxOutputLovelaceInsufficient
    deriving anyclass NFData
