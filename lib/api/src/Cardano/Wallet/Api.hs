{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- HLINT ignore "Use newtype instead of data" -}

module Cardano.Wallet.Api
    ( -- * API
      Api
    , ApiV2

      -- * Type Families
    , PostData

      -- * Shelley
    , Wallets
        , DeleteWallet
        , GetWallet
        , ListWallets
        , PostWallet
        , PutWallet
        , PutWalletPassphrase
        , GetUTxOsStatistics
        , GetWalletUtxoSnapshot

    , WalletKeys
        , GetWalletKey
        , SignMetadata
        , PostAccountKey
        , GetAccountKey
        , GetPolicyKey
        , PostPolicyKey
        , PostPolicyId

    , Assets
        , ListAssets
        , GetAsset
        , GetAssetDefault

    , Addresses
        , ListAddresses
        , InspectAddress
        , PostAnyAddress

    , CoinSelections
        , SelectCoins

    , ShelleyTransactions
        , ConstructTransaction
        , SignTransaction
        , ListTransactions
        , GetTransaction
        , DeleteTransaction
        , CreateTransactionOld
        , PostTransactionFeeOld
        , BalanceTransaction
        , DecodeTransaction
        , SubmitTransaction

    , StakePools
        , ListStakePools
        , JoinStakePool
        , QuitStakePool
        , DelegationFee
        , ListStakeKeys
        , PoolMaintenance
        , PostPoolMaintenance
        , GetPoolMaintenance

    , DReps
        , JoinDRep

    , ShelleyMigrations
        , MigrateShelleyWallet
        , CreateShelleyWalletMigrationPlan

    -- * Settings
    , Settings
        , PutSettings
        , GetSettings

    -- * Byron
    , ByronWallets
        , DeleteByronWallet
        , GetByronWallet
        , ListByronWallets
        , PostByronWallet
        , PutByronWallet
        , GetByronUTxOsStatistics
        , GetByronWalletUtxoSnapshot
        , PutByronWalletPassphrase

    , ByronAssets
        , ListByronAssets
        , GetByronAsset
        , GetByronAssetDefault

    , ByronAddresses
        , PostByronAddress
        , PutByronAddress
        , PutByronAddresses
        , ListByronAddresses

    , ByronCoinSelections
        , ByronSelectCoins

    , ByronTransactions
        , ListByronTransactions
        , GetByronTransaction
        , DeleteByronTransaction
        , CreateByronTransactionOld
        , PostByronTransactionFeeOld

    , ByronMigrations
        , MigrateByronWallet
        , CreateByronWalletMigrationPlan

    -- * Miscellaneous
    , Network
        , GetNetworkInformation
        , GetNetworkParameters
        , GetNetworkClock
    , SMASH
        , GetCurrentSMASHHealth

      -- * Shared Wallets
    , SharedWallets
        , PostSharedWallet
        , GetSharedWallet
        , PutSharedWallet
        , ListSharedWallets
        , PatchSharedWalletInPayment
        , PatchSharedWalletInDelegation
        , DeleteSharedWallet
        , GetUTxOsStatisticsShared
        , GetSharedWalletUtxoSnapshot

    , SharedWalletKeys
        , GetSharedWalletKey
        , PostAccountKeyShared
        , GetAccountKeyShared

    , SharedAddresses
        , ListSharedAddresses

    , SharedTransactions
        , ConstructSharedTransaction
        , SignSharedTransaction
        , DecodeSharedTransaction
        , SubmitSharedTransaction
        , GetSharedTransaction
        , ListSharedTransactions

    , GetBlocksLatestHeader
    , Proxy_
        , PostExternalTransaction

      -- * Api Layer
    , ApiLayer (..)
    , HasWorkerRegistry
    , workerRegistry
    , WalletLock (..)
    , walletLocks
    , HasDBFactory
    , dbFactory
    , tokenMetadataClient
    , HasTokenMetadataClient
    ) where

import Prelude

import Cardano.Wallet
    ( TxSubmitLog
    , WalletLayer (..)
    , WalletWorkerLog
    )
import Cardano.Wallet.Address.Derivation
    ( DerivationIndex
    , Role
    )
import Cardano.Wallet.Api.Types
    ( AnyAddress
    , ApiAccountKey
    , ApiAccountKeyShared
    , ApiAddressData
    , ApiAddressIdT
    , ApiAddressInspect
    , ApiAddressInspectData
    , ApiAddressT
    , ApiAsset
    , ApiBalanceTransactionPostDataT
    , ApiByronWallet
    , ApiCoinSelectionT
    , ApiConstructTransactionDataT
    , ApiConstructTransactionT
    , ApiDRepSpecifier
    , ApiDecodeTransactionPostData
    , ApiDecodedTransactionT
    , ApiFee
    , ApiHealthCheck
    , ApiMaintenanceAction
    , ApiMaintenanceActionPostData
    , ApiNetworkClock
    , ApiNetworkInformation
    , ApiNetworkParameters
    , ApiPolicyId
    , ApiPolicyKey
    , ApiPoolSpecifier
    , ApiPostAccountKeyData
    , ApiPostAccountKeyDataWithPurpose
    , ApiPostPolicyIdData
    , ApiPostPolicyKeyData
    , ApiPostRandomAddressData
    , ApiPutAddressesDataT
    , ApiSelectCoinsDataT
    , ApiSerialisedTransaction
    , ApiSharedWallet
    , ApiSharedWalletPatchData
    , ApiSharedWalletPostData
    , ApiSignTransactionPostData
    , ApiStakeKeysT
    , ApiT
    , ApiTransactionT
    , ApiTxId
    , ApiUtxoStatistics
    , ApiVerificationKeyShared
    , ApiVerificationKeyShelley
    , ApiWallet
    , ApiWalletMigrationPlan
    , ApiWalletMigrationPlanPostDataT
    , ApiWalletMigrationPostDataT
    , ApiWalletPassphrase
    , ApiWalletPutData
    , ApiWalletPutDataExtended
    , ApiWalletSignData
    , ApiWalletUtxoSnapshot
    , ByronWalletPutPassphraseData
    , Iso8601Time
    , KeyFormat
    , MinWithdrawal
    , PostTransactionFeeOldDataT
    , PostTransactionOldDataT
    , SettingsPutData
    , SomeByronWalletPostData
    , WalletOrAccountPostData
    , WalletPutPassphraseData
    )
import Cardano.Wallet.Api.Types.BlockHeader
    ( ApiBlockHeader
    )
import Cardano.Wallet.Api.Types.Transaction
    ( ApiLimit
    )
import Cardano.Wallet.DB
    ( DBFactory
    , DBLayer
    )
import Cardano.Wallet.Flavor
    ( CredFromOf
    , KeyOf
    )
import Cardano.Wallet.Network
    ( NetworkLayer
    )
import Cardano.Wallet.Pools
    ( StakePool
    )
import Cardano.Wallet.Primitive.Types
    ( Block
    , NetworkParameters
    , SmashServer (..)
    , SortOrder (..)
    , WalletId (..)
    )
import Cardano.Wallet.Primitive.Types.Address
    ( AddressState
    )
import Cardano.Wallet.Primitive.Types.AssetName
    ( AssetName
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..)
    )
import Cardano.Wallet.Primitive.Types.TokenPolicyId
    ( TokenPolicyId
    )
import Cardano.Wallet.Primitive.Types.Tx
    ( SealedTx
    )
import Cardano.Wallet.Registry
    ( HasWorkerCtx (..)
    , WorkerLog
    , WorkerRegistry
    )
import Cardano.Wallet.TokenMetadata
    ( TokenMetadataClient
    )
import Cardano.Wallet.Transaction
    ( TransactionLayer
    )
import Control.Concurrent.Concierge
    ( Concierge
    )
import Control.Tracer
    ( Tracer
    , contramap
    )
import Data.ByteString
    ( ByteString
    )
import Data.Generics.Internal.VL.Lens
    ( Lens'
    )
import Data.Generics.Labels
    ()
import Data.Generics.Product.Typed
    ( HasType
    , typed
    )
import Data.Kind
    ( Type
    )
import Data.List.NonEmpty
    ( NonEmpty
    )
import GHC.Generics
    ( Generic
    )
import Servant.API
    ( Capture
    , JSON
    , OctetStream
    , QueryFlag
    , QueryParam
    , ReqBody
    , (:<|>)
    , (:>)
    )
import Servant.API.Verbs
    ( DeleteAccepted
    , DeleteNoContent
    , Get
    , Patch
    , Post
    , PostAccepted
    , PostCreated
    , PostNoContent
    , Put
    , PutAccepted
    , PutNoContent
    )

import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Read as Read

type ApiV2 n = "v2" :> Api n

-- | The full cardano-wallet API.
type Api n =
         Wallets
    :<|> WalletKeys
    :<|> Assets
    :<|> Addresses n
    :<|> CoinSelections n
    :<|> ShelleyTransactions n
    :<|> ShelleyMigrations n
    :<|> StakePools n
    :<|> DReps n
    :<|> ByronWallets
    :<|> ByronAssets
    :<|> ByronAddresses n
    :<|> ByronCoinSelections n
    :<|> ByronTransactions n
    :<|> ByronMigrations n
    :<|> Network
    :<|> Proxy_
    :<|> Settings
    :<|> SMASH
    :<|> SharedWallets
    :<|> SharedWalletKeys
    :<|> SharedAddresses n
    :<|> SharedTransactions n
    :<|> GetBlocksLatestHeader

{-------------------------------------------------------------------------------
                                  Wallets

  See also: https://cardano-foundation.github.io/cardano-wallet/api/#tag/Wallets
-------------------------------------------------------------------------------}

type Wallets =
    DeleteWallet
    :<|> GetWallet
    :<|> ListWallets
    :<|> PostWallet
    :<|> PutWallet
    :<|> PutWalletPassphrase
    :<|> GetWalletUtxoSnapshot
    :<|> GetUTxOsStatistics

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/deleteWallet
type DeleteWallet = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> DeleteNoContent

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/getWallet
type GetWallet = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> Get '[JSON] ApiWallet

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/listWallets
type ListWallets = "wallets"
    :> Get '[JSON] [ApiWallet]

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/postWallet
type PostWallet = "wallets"
    :> ReqBody '[JSON] (PostData ApiWallet)
    :> PostCreated '[JSON] ApiWallet

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/putWallet
type PutWallet = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> ReqBody '[JSON] ApiWalletPutDataExtended
    :> Put '[JSON] ApiWallet

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/putWalletPassphrase
type PutWalletPassphrase = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "passphrase"
    :> ReqBody '[JSON] WalletPutPassphraseData
    :> PutNoContent

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/getWalletUtxoSnapshot
type GetWalletUtxoSnapshot = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "utxo"
    :> Get '[JSON] ApiWalletUtxoSnapshot

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/getUTxOsStatistics
type GetUTxOsStatistics = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "statistics"
    :> "utxos"
    :> Get '[JSON] ApiUtxoStatistics

{-------------------------------------------------------------------------------
                                  Wallet Keys
  See also: https://cardano-foundation.github.io/cardano-wallet/api/#tag/Keys
-------------------------------------------------------------------------------}

type WalletKeys =
    GetWalletKey
    :<|> SignMetadata
    :<|> PostAccountKey
    :<|> GetAccountKey
    :<|> GetPolicyKey
    :<|> PostPolicyKey
    :<|> PostPolicyId

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/getWalletKey
type GetWalletKey = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "keys"
    :> Capture "role" (ApiT Role)
    :> Capture "index" (ApiT DerivationIndex)
    :> QueryParam "hash" Bool
    :> Get '[JSON] ApiVerificationKeyShelley

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/signMetadata
type SignMetadata = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "signatures"
    :> Capture "role" (ApiT Role)
    :> Capture "index" (ApiT DerivationIndex)
    :> ReqBody '[JSON] ApiWalletSignData
    :> Post '[OctetStream] ByteString

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/postAccountKey
type PostAccountKey = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "keys"
    :> Capture "index" (ApiT DerivationIndex)
    :> ReqBody '[JSON] ApiPostAccountKeyDataWithPurpose
    :> PostAccepted '[JSON] ApiAccountKey

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/getAccountKey
type GetAccountKey = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "keys"
    :> QueryParam "format" KeyFormat
    :> Get '[JSON] ApiAccountKey

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/getPolicyKey
type GetPolicyKey = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "policy-key"
    :> QueryParam "hash" Bool
    :> Get '[JSON] ApiPolicyKey

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/postPolicyKey
type PostPolicyKey = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "policy-key"
    :> QueryParam "hash" Bool
    :> ReqBody '[JSON] ApiPostPolicyKeyData
    :> PostAccepted '[JSON] ApiPolicyKey

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/postPolicyId
type PostPolicyId = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "policy-id"
    :> ReqBody '[JSON] ApiPostPolicyIdData
    :> PostAccepted '[JSON] ApiPolicyId

{-------------------------------------------------------------------------------
                                  Assets

  See also: https://cardano-foundation.github.io/cardano-wallet/api/#tag/Assets
-------------------------------------------------------------------------------}

type Assets =
    ListAssets
    :<|> GetAsset
    :<|> GetAssetDefault

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/listAssets
type ListAssets = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "assets"
    :> Get '[JSON] [ApiAsset]

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/getAsset
type GetAsset = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "assets"
    :> Capture "policyId" (ApiT TokenPolicyId)
    :> Capture "assetName" (ApiT AssetName)
    :> Get '[JSON] ApiAsset

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/getAssetDefault
type GetAssetDefault = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "assets"
    :> Capture "policyId" (ApiT TokenPolicyId)
    :> Get '[JSON] ApiAsset

{-------------------------------------------------------------------------------
                                  Addresses

  See also: https://cardano-foundation.github.io/cardano-wallet/api/#tag/Addresses
-------------------------------------------------------------------------------}

type Addresses n =
    ListAddresses n
    :<|> InspectAddress
    :<|> PostAnyAddress n

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/listAddresses
type ListAddresses n = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "addresses"
    :> QueryParam "state" (ApiT AddressState)
    :> Get '[JSON] [ApiAddressT n]

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/inspectAddress
type InspectAddress = "addresses"
    :> Capture "addressId" ApiAddressInspectData
    :> Get '[JSON] ApiAddressInspect

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/postAnyAddress
type PostAnyAddress n = "addresses"
    :> ReqBody '[JSON] ApiAddressData
    :> PostAccepted '[JSON] AnyAddress

{-------------------------------------------------------------------------------
                               Coin Selections

  See also:
  https://cardano-foundation.github.io/cardano-wallet/api/#tag/Coin-Selections
-------------------------------------------------------------------------------}

type CoinSelections n =
    SelectCoins n

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/selectCoins
type SelectCoins n = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "coin-selections"
    :> "random"
    :> ReqBody '[JSON] (ApiSelectCoinsDataT n)
    :> Post '[JSON] (ApiCoinSelectionT n)

{-------------------------------------------------------------------------------
                                  ShelleyTransactions

  See also: https://cardano-foundation.github.io/cardano-wallet/api/#tag/Transactions
-------------------------------------------------------------------------------}

type ShelleyTransactions n =
         ConstructTransaction n
    :<|> SignTransaction n
    :<|> ListTransactions n
    :<|> GetTransaction n
    :<|> DeleteTransaction
    :<|> CreateTransactionOld n
    :<|> PostTransactionFeeOld n
    :<|> BalanceTransaction n
    :<|> DecodeTransaction n
    :<|> SubmitTransaction

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/constructTransaction
type ConstructTransaction n = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions-construct"
    :> ReqBody '[JSON] (ApiConstructTransactionDataT n)
    :> PostAccepted '[JSON] (ApiConstructTransactionT n)

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/signTransaction
type SignTransaction n = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions-sign"
    :> ReqBody '[JSON] ApiSignTransactionPostData
    :> PostAccepted '[JSON] ApiSerialisedTransaction

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/postTransaction
type CreateTransactionOld n = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions"
    :> ReqBody '[JSON] (PostTransactionOldDataT n)
    :> PostAccepted '[JSON] (ApiTransactionT n)

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/listTransactions
type ListTransactions n = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions"
    :> QueryParam "minWithdrawal" MinWithdrawal
    :> QueryParam "start" Iso8601Time
    :> QueryParam "end" Iso8601Time
    :> QueryParam "order" (ApiT SortOrder)
    :> QueryParam "max_count" ApiLimit
    :> QueryParam "address" (ApiAddressIdT n)
    :> QueryFlag "simple-metadata"
    :> Get '[JSON] [ApiTransactionT n]

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/getTransaction
type GetTransaction n = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions"
    :> Capture "transactionId" ApiTxId
    :> QueryFlag "simple-metadata"
    :> Get '[JSON] (ApiTransactionT n)

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/postTransactionFee
type PostTransactionFeeOld n = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "payment-fees"
    :> ReqBody '[JSON] (PostTransactionFeeOldDataT n)
    :> PostAccepted '[JSON] ApiFee

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/deleteTransaction
type DeleteTransaction = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions"
    :> Capture "transactionId" ApiTxId
    :> DeleteNoContent

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/balanceTransaction
type BalanceTransaction n = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions-balance"
    :> ReqBody '[JSON] (ApiBalanceTransactionPostDataT n)
    :> PostAccepted '[JSON] ApiSerialisedTransaction

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/decodeTransaction
type DecodeTransaction n = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions-decode"
    :> ReqBody '[JSON] ApiDecodeTransactionPostData
    :> PostAccepted '[JSON] (ApiDecodedTransactionT n)

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/submitTransaction
type SubmitTransaction = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions-submit"
    :> ReqBody '[JSON] ApiSerialisedTransaction
    :> PostAccepted '[JSON] ApiTxId

{-------------------------------------------------------------------------------
                                 Shelley Migrations

See also:
https://cardano-foundation.github.io/cardano-wallet/api/#tag/Migrations
-------------------------------------------------------------------------------}

type ShelleyMigrations n =
         CreateShelleyWalletMigrationPlan n
    :<|> MigrateShelleyWallet n

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/migrateShelleyWallet
type MigrateShelleyWallet n = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "migrations"
    :> ReqBody '[JSON] (ApiWalletMigrationPostDataT n "user")
    :> PostAccepted '[JSON] (NonEmpty (ApiTransactionT n))

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/createShelleyWalletMigrationPlan
type CreateShelleyWalletMigrationPlan n = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "migrations"
    :> "plan"
    :> ReqBody '[JSON] (ApiWalletMigrationPlanPostDataT n)
    :> PostAccepted '[JSON] (ApiWalletMigrationPlan n)

{-------------------------------------------------------------------------------
                                  StakePools

  See also: https://cardano-foundation.github.io/cardano-wallet/api/edge/#tag/Stake-Pools
-------------------------------------------------------------------------------}

-- Factoring out common prefixes into nested APIs
-- doesn't work well with the functionality provided
-- by the Servant.Links module.

type StakePools n
    =    ListStakePools
    :<|> JoinStakePool n
    :<|> QuitStakePool n
    :<|> PoolMaintenance
    :<|> DelegationFee
    :<|> ListStakeKeys n

type ListStakePools =
    "stake-pools"
    :> QueryParam "stake" (ApiT Coin) :> Get '[JSON] [ApiT StakePool]

type JoinStakePool n =
    "stake-pools" :> Capture "stakePoolId" ApiPoolSpecifier
    :> "wallets" :> Capture "walletId" (ApiT WalletId)
    :> ReqBody '[JSON] ApiWalletPassphrase
    :> PutAccepted '[JSON] (ApiTransactionT n)

type QuitStakePool n =
    "stake-pools" :> "*"
    :> "wallets" :> Capture "walletId" (ApiT WalletId)
    :> ReqBody '[JSON] ApiWalletPassphrase
    :> DeleteAccepted '[JSON] (ApiTransactionT n)

type DelegationFee = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "delegation-fees"
    :> Get '[JSON] ApiFee

type ListStakeKeys n = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "stake-keys"
    :> Get '[JSON] (ApiStakeKeysT n)

type PoolMaintenance
    = PostPoolMaintenance :<|> GetPoolMaintenance

type PostPoolMaintenance =
    "stake-pools" :> "maintenance-actions"
    :> ReqBody '[JSON] ApiMaintenanceActionPostData
    :> PostNoContent

type GetPoolMaintenance =
    "stake-pools" :> "maintenance-actions"
    :> Get '[JSON] ApiMaintenanceAction

{-------------------------------------------------------------------------------
                                  DReps

  See also: https://cardano-foundation.github.io/cardano-wallet/api/edge/#tag/DReps
-------------------------------------------------------------------------------}

type DReps n
    =    JoinDRep n

type JoinDRep n =
    "dreps" :> Capture "drepId" ApiDRepSpecifier
    :> "wallets" :> Capture "walletId" (ApiT WalletId)
    :> ReqBody '[JSON] ApiWalletPassphrase
    :> PutAccepted '[JSON] (ApiTransactionT n)

{-------------------------------------------------------------------------------
                                  Settings
-------------------------------------------------------------------------------}

type Settings = PutSettings :<|> GetSettings

type PutSettings = "settings"
    :> ReqBody '[JSON] SettingsPutData
    :> PutNoContent

type GetSettings = "settings"
    :> Get '[JSON] (ApiT W.Settings)

{-------------------------------------------------------------------------------
                                 Byron Wallets

  See also: https://cardano-foundation.github.io/cardano-wallet/api/#tag/Byron-Wallets
-------------------------------------------------------------------------------}

type ByronWallets =
         PostByronWallet
    :<|> DeleteByronWallet
    :<|> GetByronWallet
    :<|> ListByronWallets
    :<|> PutByronWallet
    :<|> GetByronWalletUtxoSnapshot
    :<|> GetByronUTxOsStatistics
    :<|> PutByronWalletPassphrase

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/postByronWallet
type PostByronWallet = "byron-wallets"
    :> ReqBody '[JSON] (PostData ApiByronWallet)
    :> PostCreated '[JSON] ApiByronWallet

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/deleteByronWallet
type DeleteByronWallet = "byron-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> DeleteNoContent

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/getByronWallet
type GetByronWallet = "byron-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> Get '[JSON] ApiByronWallet

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/listByronWallets
type ListByronWallets = "byron-wallets"
    :> Get '[JSON] [ApiByronWallet]

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/putByronWallet
type PutByronWallet = "byron-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> ReqBody '[JSON] ApiWalletPutData
    :> Put '[JSON] ApiByronWallet

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/getByronWalletUtxoSnapshot
type GetByronWalletUtxoSnapshot = "byron-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "utxo"
    :> Get '[JSON] ApiWalletUtxoSnapshot

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/getByronUTxOsStatistics
type GetByronUTxOsStatistics = "byron-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "statistics"
    :> "utxos"
    :> Get '[JSON] ApiUtxoStatistics

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/putByronWalletPassphrase
type PutByronWalletPassphrase = "byron-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "passphrase"
    :> ReqBody '[JSON] ByronWalletPutPassphraseData
    :> PutNoContent

{-------------------------------------------------------------------------------
                                  Assets

  See also: https://cardano-foundation.github.io/cardano-wallet/api/#tag/ByronAssets
-------------------------------------------------------------------------------}

type ByronAssets =
    ListByronAssets
    :<|> GetByronAsset
    :<|> GetByronAssetDefault

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/listByronAssets
type ListByronAssets = "byron-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "assets"
    :> Get '[JSON] [ApiAsset]

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/getByronAsset
type GetByronAsset = "byron-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "assets"
    :> Capture "policyId" (ApiT TokenPolicyId)
    :> Capture "assetName" (ApiT AssetName)
    :> Get '[JSON] ApiAsset

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/getByronAssetDefault
type GetByronAssetDefault = "byron-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "assets"
    :> Capture "policyId" (ApiT TokenPolicyId)
    :> Get '[JSON] ApiAsset

{-------------------------------------------------------------------------------
                                  Addresses

  See also: https://cardano-foundation.github.io/cardano-wallet/api/#tag/Byron-Addresses
-------------------------------------------------------------------------------}

type ByronAddresses n =
    PostByronAddress n
    :<|> PutByronAddress n
    :<|> PutByronAddresses n
    :<|> ListByronAddresses n

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/createAddress
type PostByronAddress n = "byron-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "addresses"
    :> ReqBody '[JSON] ApiPostRandomAddressData
    :> PostCreated '[JSON] (ApiAddressT n)

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/restoreAddress
type PutByronAddress n = "byron-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "addresses"
    :> Capture "addressId" (ApiAddressIdT n)
    :> PutNoContent

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/importAddresses
type PutByronAddresses n = "byron-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "addresses"
    :> ReqBody '[JSON] (ApiPutAddressesDataT n)
    :> PutNoContent

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/listByronAddresses
type ListByronAddresses n = "byron-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "addresses"
    :> QueryParam "state" (ApiT AddressState)
    :> Get '[JSON] [ApiAddressT n]

{-------------------------------------------------------------------------------
                               Coin Selections

  See also:
  https://cardano-foundation.github.io/cardano-wallet/api/#tag/Byron-Coin-Selections
-------------------------------------------------------------------------------}

type ByronCoinSelections n =
    ByronSelectCoins n

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/byronSelectCoins
type ByronSelectCoins n = "byron-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "coin-selections"
    :> "random"
    :> ReqBody '[JSON] (ApiSelectCoinsDataT n)
    :> Post '[JSON] (ApiCoinSelectionT n)

{-------------------------------------------------------------------------------
                                 Byron Transactions

  See also: https://cardano-foundation.github.io/cardano-wallet/api/#tag/Byron-Transactions
-------------------------------------------------------------------------------}

type ByronTransactions n =
         ListByronTransactions n
    :<|> GetByronTransaction n
    :<|> DeleteByronTransaction
    :<|> CreateByronTransactionOld n
    :<|> PostByronTransactionFeeOld n

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/postByronTransaction
type CreateByronTransactionOld n = "byron-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions"
    :> ReqBody '[JSON] (PostTransactionOldDataT n)
    :> PostAccepted '[JSON] (ApiTransactionT n)

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/listByronTransactions
type ListByronTransactions n = "byron-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions"
    :> QueryParam "start" Iso8601Time
    :> QueryParam "end" Iso8601Time
    :> QueryParam "order" (ApiT SortOrder)
    :> QueryParam "max_count" ApiLimit
    :> QueryParam "address" (ApiAddressIdT n)
    :> Get '[JSON] [ApiTransactionT n]

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/getByronTransaction
type GetByronTransaction n = "byron-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions"
    :> Capture "transactionId" ApiTxId
    :> Get '[JSON] (ApiTransactionT n)

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/postByronTransactionFee
type PostByronTransactionFeeOld n = "byron-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "payment-fees"
    :> ReqBody '[JSON] (PostTransactionFeeOldDataT n)
    :> PostAccepted '[JSON] ApiFee

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/deleteByronTransaction
type DeleteByronTransaction = "byron-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions"
    :> Capture "transactionId" ApiTxId
    :> DeleteNoContent

{-------------------------------------------------------------------------------
                                 Byron Migrations

  See also: https://cardano-foundation.github.io/cardano-wallet/api/#tag/Byron-Migrations
-------------------------------------------------------------------------------}

type ByronMigrations n =
         CreateByronWalletMigrationPlan n
    :<|> MigrateByronWallet n

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/migrateByronWallet
type MigrateByronWallet n = "byron-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "migrations"
    :> ReqBody '[JSON] (ApiWalletMigrationPostDataT n "lenient")
    :> PostAccepted '[JSON] (NonEmpty (ApiTransactionT n))

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/createByronWalletMigrationPlan
type CreateByronWalletMigrationPlan n = "byron-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "migrations"
    :> "plan"
    :> ReqBody '[JSON] (ApiWalletMigrationPlanPostDataT n)
    :> PostAccepted '[JSON] (ApiWalletMigrationPlan n)

{-------------------------------------------------------------------------------
                                  Network

  See also: https://cardano-foundation.github.io/cardano-wallet/api/#tag/Network
-------------------------------------------------------------------------------}

type Network =
         GetNetworkInformation
    :<|> GetNetworkParameters
    :<|> GetNetworkClock

type GetNetworkInformation = "network"
    :> "information"
    :> Get '[JSON] ApiNetworkInformation

type GetNetworkParameters = "network"
    :> "parameters"
    :> Get '[JSON] ApiNetworkParameters

type GetNetworkClock = "network"
    :> "clock"
    :> QueryFlag "forceNtpCheck"
    :> Get '[JSON] ApiNetworkClock

{-------------------------------------------------------------------------------
                                  Blocks

-------------------------------------------------------------------------------}

type GetBlocksLatestHeader = "blocks"
    :> "latest"
    :> "header"
    :> Get '[JSON] ApiBlockHeader
{-------------------------------------------------------------------------------
                                  SMASH

-------------------------------------------------------------------------------}

type SMASH = GetCurrentSMASHHealth

type GetCurrentSMASHHealth = "smash"
    :> "health"
    :> QueryParam "url" (ApiT SmashServer)
    :> Get '[JSON] ApiHealthCheck

{-------------------------------------------------------------------------------
                                  Shared Wallets

  See also: https://cardano-foundation.github.io/cardano-wallet/api/#tag/Shared-Wallets
-------------------------------------------------------------------------------}

type SharedWallets =
         PostSharedWallet
    :<|> GetSharedWallet
    :<|> PutSharedWallet
    :<|> ListSharedWallets
    :<|> PatchSharedWalletInPayment
    :<|> PatchSharedWalletInDelegation
    :<|> DeleteSharedWallet
    :<|> GetUTxOsStatisticsShared
    :<|> GetSharedWalletUtxoSnapshot

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/postSharedWallet
type PostSharedWallet = "shared-wallets"
    :> ReqBody '[JSON] ApiSharedWalletPostData
    :> PostCreated '[JSON] ApiSharedWallet

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/getSharedWallet
type GetSharedWallet = "shared-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> Get '[JSON] ApiSharedWallet

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/putSharedWallet
type PutSharedWallet = "shared-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> ReqBody '[JSON] ApiWalletPutDataExtended
    :> Put '[JSON] ApiSharedWallet

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/listSharedWallets
type ListSharedWallets = "shared-wallets"
    :> Get '[JSON] [ApiSharedWallet]

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/patchSharedWalletInPayment
type PatchSharedWalletInPayment = "shared-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "payment-script-template"
    :> ReqBody '[JSON] ApiSharedWalletPatchData
    :> Patch '[JSON] ApiSharedWallet

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/patchSharedWalletInDelegation
type PatchSharedWalletInDelegation = "shared-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "delegation-script-template"
    :> ReqBody '[JSON] ApiSharedWalletPatchData
    :> Patch '[JSON] ApiSharedWallet

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/deleteSharedWallet
type DeleteSharedWallet = "shared-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> DeleteNoContent

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/getUTxOsStatisticsShared
type GetUTxOsStatisticsShared = "shared-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "statistics"
    :> "utxos"
    :> Get '[JSON] ApiUtxoStatistics

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/getSharedWalletUtxoSnapshot
type GetSharedWalletUtxoSnapshot = "shared-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "utxo"
    :> Get '[JSON] ApiWalletUtxoSnapshot

{-------------------------------------------------------------------------------
                                  Shared Wallet Keys
  See also: https://cardano-foundation.github.io/cardano-wallet/api/#tag/Keys
-------------------------------------------------------------------------------}

type SharedWalletKeys =
         GetSharedWalletKey
    :<|> PostAccountKeyShared
    :<|> GetAccountKeyShared

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/getSharedWalletKey
type GetSharedWalletKey = "shared-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "keys"
    :> Capture "role" (ApiT Role)
    :> Capture "index" (ApiT DerivationIndex)
    :> QueryParam "hash" Bool
    :> Get '[JSON] ApiVerificationKeyShared

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/postAccountKeyShared
type PostAccountKeyShared = "shared-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "keys"
    :> Capture "index" (ApiT DerivationIndex)
    :> ReqBody '[JSON] ApiPostAccountKeyData
    :> PostAccepted '[JSON] ApiAccountKeyShared

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/getAccountKeyShared
type GetAccountKeyShared = "shared-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "keys"
    :> QueryParam "format" KeyFormat
    :> Get '[JSON] ApiAccountKeyShared

{-------------------------------------------------------------------------------
                                 Shared Addresses

  See also: https://cardano-foundation.github.io/cardano-wallet/api/#tag/Shared-Addresses
-------------------------------------------------------------------------------}

type SharedAddresses n =
    ListSharedAddresses n

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/listSharedAddresses
type ListSharedAddresses n = "shared-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "addresses"
    :> QueryParam "state" (ApiT AddressState)
    :> Get '[JSON] [ApiAddressT n]

{-------------------------------------------------------------------------------
                                  Shared Transactions

  See also: https://cardano-foundation.github.io/cardano-wallet/api/#tag/Shared-Transactions
-------------------------------------------------------------------------------}

type SharedTransactions n =
         ConstructSharedTransaction n
    :<|> SignSharedTransaction n
    :<|> DecodeSharedTransaction n
    :<|> SubmitSharedTransaction
    :<|> GetSharedTransaction n
    :<|> ListSharedTransactions n

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/constructSharedTransaction
type ConstructSharedTransaction n = "shared-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions-construct"
    :> ReqBody '[JSON] (ApiConstructTransactionDataT n)
    :> PostAccepted '[JSON] (ApiConstructTransactionT n)

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/signSharedTransaction
type SignSharedTransaction n = "shared-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions-sign"
    :> ReqBody '[JSON] ApiSignTransactionPostData
    :> PostAccepted '[JSON] ApiSerialisedTransaction

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/decodeSharedTransaction
type DecodeSharedTransaction n = "shared-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions-decode"
    :> ReqBody '[JSON] ApiDecodeTransactionPostData
    :> PostAccepted '[JSON] (ApiDecodedTransactionT n)

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/submitSharedTransaction
type SubmitSharedTransaction = "shared-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions-submit"
    :> ReqBody '[JSON] ApiSerialisedTransaction
    :> PostAccepted '[JSON] ApiTxId

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/getSharedTransaction
type GetSharedTransaction n = "shared-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions"
    :> Capture "transactionId" ApiTxId
    :> QueryFlag "simple-metadata"
    :> Get '[JSON] (ApiTransactionT n)

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/listSharedTransactions
type ListSharedTransactions n = "shared-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions"
    :> QueryParam "minWithdrawal" MinWithdrawal
    :> QueryParam "start" Iso8601Time
    :> QueryParam "end" Iso8601Time
    :> QueryParam "order" (ApiT SortOrder)
    :> QueryParam "max_count" ApiLimit
    :> QueryParam "address" (ApiAddressIdT n)
    :> QueryFlag "simple-metadata"
    :> Get '[JSON] [ApiTransactionT n]

{-------------------------------------------------------------------------------
                                   Proxy_

  See also: https://cardano-foundation.github.io/cardano-wallet/api/edge/#tag/Proxy
-------------------------------------------------------------------------------}

type Proxy_ =
    PostExternalTransaction

-- | https://cardano-foundation.github.io/cardano-wallet/api/#operation/postExternalTransaction
type PostExternalTransaction = "proxy"
    :> "transactions"
    :> ReqBody '[OctetStream] (ApiT SealedTx)
    :> PostAccepted '[JSON] ApiTxId

{-------------------------------------------------------------------------------
                               Api Layer
-------------------------------------------------------------------------------}

data ApiLayer s
    = ApiLayer
        { tracerTxSubmit :: Tracer IO TxSubmitLog
        , tracerWalletWorker :: Tracer IO (WorkerLog WalletId WalletWorkerLog)
        , netParams :: (Block, NetworkParameters)
        , netLayer :: NetworkLayer IO Read.ConsensusBlock
        , txLayer :: TransactionLayer (KeyOf s) (CredFromOf s) SealedTx
        , _dbFactory :: DBFactory IO s
        , _workerRegistry :: WorkerRegistry WalletId (DBLayer IO s)
        , concierge :: Concierge IO WalletLock
        , _tokenMetadataClient :: TokenMetadataClient IO
        }
    deriving (Generic)

-- | Locks that are held by the wallet in order to enforce
-- sequential execution of some API actions.
-- Used with "Control.Concurrent.Concierge".
data WalletLock = PostTransactionOld WalletId
    deriving (Eq, Ord, Show)

instance HasWorkerCtx (DBLayer IO s) (ApiLayer s) where
    type WorkerCtx (ApiLayer s) = WalletLayer IO s
    type WorkerMsg (ApiLayer s) = WalletWorkerLog
    type WorkerKey (ApiLayer s) = WalletId
    hoistResource db transform (ApiLayer _ tr gp nw tl _ _ _ _) =
        WalletLayer (contramap transform tr) gp nw tl db

{-------------------------------------------------------------------------------
                               Capabilities
-------------------------------------------------------------------------------}

type HasWorkerRegistry s ctx =
    ( HasType (WorkerRegistry WalletId (DBLayer IO s)) ctx
    , HasWorkerCtx (DBLayer IO s) ctx
    , WorkerKey ctx ~ WalletId
    , WorkerMsg ctx ~ WalletWorkerLog
    )

workerRegistry
    :: forall s ctx. (HasWorkerRegistry s ctx)
    => Lens' ctx (WorkerRegistry WalletId (DBLayer IO s))
workerRegistry =
    typed @(WorkerRegistry WalletId (DBLayer IO s))

type HasDBFactory s = HasType (DBFactory IO s)
type HasTokenMetadataClient = HasType (TokenMetadataClient IO)

dbFactory
    :: forall s ctx
     . HasDBFactory s ctx
    => Lens' ctx (DBFactory IO s)
dbFactory =
    typed @(DBFactory IO s)

tokenMetadataClient
    :: forall ctx. (HasTokenMetadataClient ctx)
    => Lens' ctx (TokenMetadataClient IO)
tokenMetadataClient =
    typed @(TokenMetadataClient IO)

walletLocks
    :: forall ctx. (HasType (Concierge IO WalletLock) ctx)
    => Lens' ctx (Concierge IO WalletLock)
walletLocks =
    typed @(Concierge IO WalletLock)

{-------------------------------------------------------------------------------
                              Type Families
-------------------------------------------------------------------------------}

type family PostData wallet :: Type where
    PostData ApiWallet = WalletOrAccountPostData
    PostData ApiByronWallet = SomeByronWalletPostData
