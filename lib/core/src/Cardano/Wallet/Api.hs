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

    , Assets
        , ListAssets
        , GetAsset
        , GetAssetDefault
        , MintBurnAssets

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
        , PostPoolMaintenance
        , GetPoolMaintenance

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
        , ConstructByronTransaction
        , SignByronTransaction
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
        , ListSharedWallets
        , PatchSharedWalletInPayment
        , PatchSharedWalletInDelegation
        , DeleteSharedWallet

    , SharedWalletKeys
        , GetSharedWalletKey
        , PostAccountKeyShared
        , GetAccountKeyShared

    , SharedAddresses
        , ListSharedAddresses

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
    ( TxSubmitLog, WalletLayer (..), WalletWorkerLog )
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
    , ApiDecodedTransactionT
    , ApiFee
    , ApiHealthCheck
    , ApiMaintenanceAction
    , ApiMaintenanceActionPostData
    , ApiMintedBurnedTransactionT
    , ApiNetworkClock
    , ApiNetworkInformation
    , ApiNetworkParameters
    , ApiPolicyKey
    , ApiPoolId
    , ApiPostAccountKeyData
    , ApiPostAccountKeyDataWithPurpose
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
    , ApiWalletSignData
    , ApiWalletUtxoSnapshot
    , ByronWalletPutPassphraseData
    , Iso8601Time
    , KeyFormat
    , MinWithdrawal
    , PostMintBurnAssetDataT
    , PostTransactionFeeOldDataT
    , PostTransactionOldDataT
    , SettingsPutData
    , SomeByronWalletPostData
    , WalletOrAccountPostData
    , WalletPutData
    , WalletPutPassphraseData
    )
import Cardano.Wallet.DB
    ( DBFactory, DBLayer )
import Cardano.Wallet.Network
    ( NetworkLayer )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth, DerivationIndex, Role )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncTolerance )
import Cardano.Wallet.Primitive.Types
    ( Block
    , NetworkParameters
    , SmashServer (..)
    , SortOrder (..)
    , WalletId (..)
    )
import Cardano.Wallet.Primitive.Types.Address
    ( AddressState )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName, TokenPolicyId )
import Cardano.Wallet.Primitive.Types.Tx
    ( SealedTx )
import Cardano.Wallet.Registry
    ( HasWorkerCtx (..), WorkerLog, WorkerRegistry )
import Cardano.Wallet.TokenMetadata
    ( TokenMetadataClient )
import Cardano.Wallet.Transaction
    ( TransactionLayer )
import Control.Concurrent.Concierge
    ( Concierge )
import Control.Tracer
    ( Tracer, contramap )
import Data.ByteString
    ( ByteString )
import Data.Generics.Internal.VL.Lens
    ( Lens' )
import Data.Generics.Labels
    ()
import Data.Generics.Product.Typed
    ( HasType, typed )
import Data.Kind
    ( Type )
import Data.List.NonEmpty
    ( NonEmpty )
import GHC.Generics
    ( Generic )
import Servant.API
    ( (:<|>)
    , (:>)
    , Capture
    , JSON
    , OctetStream
    , QueryFlag
    , QueryParam
    , ReqBody
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

type ApiV2 n apiPool = "v2" :> Api n apiPool

-- | The full cardano-wallet API.
type Api n apiPool =
         Wallets
    :<|> WalletKeys
    :<|> Assets n
    :<|> Addresses n
    :<|> CoinSelections n
    :<|> ShelleyTransactions n
    :<|> ShelleyMigrations n
    :<|> StakePools n apiPool
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

{-------------------------------------------------------------------------------
                                  Wallets

  See also: https://input-output-hk.github.io/cardano-wallet/api/#tag/Wallets
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

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/deleteWallet
type DeleteWallet = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> DeleteNoContent

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/getWallet
type GetWallet = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> Get '[JSON] ApiWallet

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/listWallets
type ListWallets = "wallets"
    :> Get '[JSON] [ApiWallet]

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/postWallet
type PostWallet = "wallets"
    :> ReqBody '[JSON] (PostData ApiWallet)
    :> PostCreated '[JSON] ApiWallet

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/putWallet
type PutWallet = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> ReqBody '[JSON] WalletPutData
    :> Put '[JSON] ApiWallet

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/putWalletPassphrase
type PutWalletPassphrase = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "passphrase"
    :> ReqBody '[JSON] WalletPutPassphraseData
    :> PutNoContent

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/getWalletUtxoSnapshot
type GetWalletUtxoSnapshot = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "utxo"
    :> Get '[JSON] ApiWalletUtxoSnapshot

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/getUTxOsStatistics
type GetUTxOsStatistics = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "statistics"
    :> "utxos"
    :> Get '[JSON] ApiUtxoStatistics

{-------------------------------------------------------------------------------
                                  Wallet Keys
  See also: https://input-output-hk.github.io/cardano-wallet/api/#tag/Keys
-------------------------------------------------------------------------------}

type WalletKeys =
    GetWalletKey
    :<|> SignMetadata
    :<|> PostAccountKey
    :<|> GetAccountKey
    :<|> GetPolicyKey

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/getWalletKey
type GetWalletKey = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "keys"
    :> Capture "role" (ApiT Role)
    :> Capture "index" (ApiT DerivationIndex)
    :> QueryParam "hash" Bool
    :> Get '[JSON] ApiVerificationKeyShelley

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/signMetadata
type SignMetadata = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "signatures"
    :> Capture "role" (ApiT Role)
    :> Capture "index" (ApiT DerivationIndex)
    :> ReqBody '[JSON] ApiWalletSignData
    :> Post '[OctetStream] ByteString

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/postAccountKey
type PostAccountKey = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "keys"
    :> Capture "index" (ApiT DerivationIndex)
    :> ReqBody '[JSON] ApiPostAccountKeyDataWithPurpose
    :> PostAccepted '[JSON] ApiAccountKey

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/getAccountKey
type GetAccountKey = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "keys"
    :> QueryParam "format" KeyFormat
    :> Get '[JSON] ApiAccountKey

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/getPolicyKey
type GetPolicyKey = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "policy-key"
    :> QueryParam "hash" Bool
    :> Get '[JSON] ApiPolicyKey

{-------------------------------------------------------------------------------
                                  Assets

  See also: https://input-output-hk.github.io/cardano-wallet/api/#tag/Assets
-------------------------------------------------------------------------------}

type Assets n =
    MintBurnAssets n
    :<|> ListAssets
    :<|> GetAsset
    :<|> GetAssetDefault

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/listAssets
type ListAssets = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "assets"
    :> Get '[JSON] [ApiAsset]

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/getAsset
type GetAsset = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "assets"
    :> Capture "policyId" (ApiT TokenPolicyId)
    :> Capture "assetName" (ApiT TokenName)
    :> Get '[JSON] ApiAsset

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/getAssetDefault
type GetAssetDefault = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "assets"
    :> Capture "policyId" (ApiT TokenPolicyId)
    :> Get '[JSON] ApiAsset

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/mintBurnAssets
type MintBurnAssets n = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "assets"
    :> ReqBody '[JSON] (PostMintBurnAssetDataT n)
    :> PostAccepted '[JSON] (ApiMintedBurnedTransactionT n)

{-------------------------------------------------------------------------------
                                  Addresses

  See also: https://input-output-hk.github.io/cardano-wallet/api/#tag/Addresses
-------------------------------------------------------------------------------}

type Addresses n =
    ListAddresses n
    :<|> InspectAddress
    :<|> PostAnyAddress n

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/listAddresses
type ListAddresses n = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "addresses"
    :> QueryParam "state" (ApiT AddressState)
    :> Get '[JSON] [ApiAddressT n]

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/inspectAddress
type InspectAddress = "addresses"
    :> Capture "addressId" ApiAddressInspectData
    :> Get '[JSON] ApiAddressInspect

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/postAnyAddress
type PostAnyAddress n = "addresses"
    :> ReqBody '[JSON] ApiAddressData
    :> PostAccepted '[JSON] AnyAddress

{-------------------------------------------------------------------------------
                               Coin Selections

  See also:
  https://input-output-hk.github.io/cardano-wallet/api/#tag/Coin-Selections
-------------------------------------------------------------------------------}

type CoinSelections n =
    SelectCoins n

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/selectCoins
type SelectCoins n = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "coin-selections"
    :> "random"
    :> ReqBody '[JSON] (ApiSelectCoinsDataT n)
    :> Post '[JSON] (ApiCoinSelectionT n)

{-------------------------------------------------------------------------------
                                  ShelleyTransactions

  See also: https://input-output-hk.github.io/cardano-wallet/api/#tag/Transactions
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

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/constructTransaction
type ConstructTransaction n = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions-construct"
    :> ReqBody '[JSON] (ApiConstructTransactionDataT n)
    :> PostAccepted '[JSON] (ApiConstructTransactionT n)

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/signTransaction
type SignTransaction n = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions-sign"
    :> ReqBody '[JSON] ApiSignTransactionPostData
    :> PostAccepted '[JSON] ApiSerialisedTransaction

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/postTransaction
type CreateTransactionOld n = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions"
    :> ReqBody '[JSON] (PostTransactionOldDataT n)
    :> PostAccepted '[JSON] (ApiTransactionT n)

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/listTransactions
type ListTransactions n = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions"
    :> QueryParam "minWithdrawal" MinWithdrawal
    :> QueryParam "start" Iso8601Time
    :> QueryParam "end" Iso8601Time
    :> QueryParam "order" (ApiT SortOrder)
    :> Get '[JSON] [ApiTransactionT n]

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/getTransaction
type GetTransaction n = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions"
    :> Capture "transactionId" ApiTxId
    :> Get '[JSON] (ApiTransactionT n)

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/postTransactionFee
type PostTransactionFeeOld n = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "payment-fees"
    :> ReqBody '[JSON] (PostTransactionFeeOldDataT n)
    :> PostAccepted '[JSON] ApiFee

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/deleteTransaction
type DeleteTransaction = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions"
    :> Capture "transactionId" ApiTxId
    :> DeleteNoContent

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/balanceTransaction
type BalanceTransaction n = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions-balance"
    :> ReqBody '[JSON] (ApiBalanceTransactionPostDataT n)
    :> PostAccepted '[JSON] ApiSerialisedTransaction

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/decodeTransaction
type DecodeTransaction n = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions-decode"
    :> ReqBody '[JSON] ApiSerialisedTransaction
    :> PostAccepted '[JSON] (ApiDecodedTransactionT n)

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/submitTransaction
type SubmitTransaction = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions-submit"
    :> ReqBody '[JSON] ApiSerialisedTransaction
    :> PostAccepted '[JSON] ApiTxId

{-------------------------------------------------------------------------------
                                 Shelley Migrations

See also:
https://input-output-hk.github.io/cardano-wallet/api/#tag/Migrations
-------------------------------------------------------------------------------}

type ShelleyMigrations n =
         CreateShelleyWalletMigrationPlan n
    :<|> MigrateShelleyWallet n

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/migrateShelleyWallet
type MigrateShelleyWallet n = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "migrations"
    :> ReqBody '[JSON] (ApiWalletMigrationPostDataT n "raw")
    :> PostAccepted '[JSON] (NonEmpty (ApiTransactionT n))

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/createShelleyWalletMigrationPlan
type CreateShelleyWalletMigrationPlan n = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "migrations"
    :> "plan"
    :> ReqBody '[JSON] (ApiWalletMigrationPlanPostDataT n)
    :> PostAccepted '[JSON] (ApiWalletMigrationPlan n)

{-------------------------------------------------------------------------------
                                  StakePools

  See also: https://input-output-hk.github.io/cardano-wallet/api/edge/#tag/Stake-Pools
-------------------------------------------------------------------------------}

type StakePools n apiPool =
    ListStakePools apiPool
    :<|> JoinStakePool n
    :<|> QuitStakePool n
    :<|> DelegationFee
    :<|> ListStakeKeys n
    :<|> PostPoolMaintenance
    :<|> GetPoolMaintenance

-- | https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/listStakePools
type ListStakePools apiPool = "stake-pools"
    :> QueryParam "stake" (ApiT Coin)
    :> Get '[JSON] [apiPool]

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/joinStakePool
type JoinStakePool n = "stake-pools"
    :> Capture "stakePoolId" ApiPoolId
    :> "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> ReqBody '[JSON] ApiWalletPassphrase
    :> PutAccepted '[JSON] (ApiTransactionT n)

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/quitStakePool
type QuitStakePool n = "stake-pools"
    :> "*"
    :> "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> ReqBody '[JSON] ApiWalletPassphrase
    :> DeleteAccepted '[JSON] (ApiTransactionT n)

type ListStakeKeys n = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "stake-keys"
    :> Get '[JSON] (ApiStakeKeysT n)

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/getDelegationFee
type DelegationFee = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "delegation-fees"
    :> Get '[JSON] ApiFee

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/postPoolMaintenance
type PostPoolMaintenance = "stake-pools"
    :> "maintenance-actions"
    :> ReqBody '[JSON] ApiMaintenanceActionPostData
    :> PostNoContent

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/getPoolMaintenance
type GetPoolMaintenance = "stake-pools"
    :> "maintenance-actions"
    :> Get '[JSON] ApiMaintenanceAction

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

  See also: https://input-output-hk.github.io/cardano-wallet/api/#tag/Byron-Wallets
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

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/postByronWallet
type PostByronWallet = "byron-wallets"
    :> ReqBody '[JSON] (PostData ApiByronWallet)
    :> PostCreated '[JSON] ApiByronWallet

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/deleteByronWallet
type DeleteByronWallet = "byron-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> DeleteNoContent

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/getByronWallet
type GetByronWallet = "byron-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> Get '[JSON] ApiByronWallet

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/listByronWallets
type ListByronWallets = "byron-wallets"
    :> Get '[JSON] [ApiByronWallet]

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/putByronWallet
type PutByronWallet = "byron-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> ReqBody '[JSON] WalletPutData
    :> Put '[JSON] ApiByronWallet

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/getByronWalletUtxoSnapshot
type GetByronWalletUtxoSnapshot = "byron-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "utxo"
    :> Get '[JSON] ApiWalletUtxoSnapshot

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/getByronUTxOsStatistics
type GetByronUTxOsStatistics = "byron-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "statistics"
    :> "utxos"
    :> Get '[JSON] ApiUtxoStatistics

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/putByronWalletPassphrase
type PutByronWalletPassphrase = "byron-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "passphrase"
    :> ReqBody '[JSON] ByronWalletPutPassphraseData
    :> PutNoContent

{-------------------------------------------------------------------------------
                                  Assets

  See also: https://input-output-hk.github.io/cardano-wallet/api/#tag/ByronAssets
-------------------------------------------------------------------------------}

type ByronAssets =
    ListByronAssets
    :<|> GetByronAsset
    :<|> GetByronAssetDefault

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/listByronAssets
type ListByronAssets = "byron-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "assets"
    :> Get '[JSON] [ApiAsset]

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/getByronAsset
type GetByronAsset = "byron-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "assets"
    :> Capture "policyId" (ApiT TokenPolicyId)
    :> Capture "assetName" (ApiT TokenName)
    :> Get '[JSON] ApiAsset

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/getByronAssetDefault
type GetByronAssetDefault = "byron-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "assets"
    :> Capture "policyId" (ApiT TokenPolicyId)
    :> Get '[JSON] ApiAsset

{-------------------------------------------------------------------------------
                                  Addresses

  See also: https://input-output-hk.github.io/cardano-wallet/api/#tag/Byron-Addresses
-------------------------------------------------------------------------------}

type ByronAddresses n =
    PostByronAddress n
    :<|> PutByronAddress n
    :<|> PutByronAddresses n
    :<|> ListByronAddresses n

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/createAddress
type PostByronAddress n = "byron-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "addresses"
    :> ReqBody '[JSON] ApiPostRandomAddressData
    :> PostCreated '[JSON] (ApiAddressT n)

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/restoreAddress
type PutByronAddress n = "byron-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "addresses"
    :> Capture "addressId" (ApiAddressIdT n)
    :> PutNoContent

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/importAddresses
type PutByronAddresses n = "byron-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "addresses"
    :> ReqBody '[JSON] (ApiPutAddressesDataT n)
    :> PutNoContent

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/listByronAddresses
type ListByronAddresses n = "byron-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "addresses"
    :> QueryParam "state" (ApiT AddressState)
    :> Get '[JSON] [ApiAddressT n]

{-------------------------------------------------------------------------------
                               Coin Selections

  See also:
  https://input-output-hk.github.io/cardano-wallet/api/#tag/Byron-Coin-Selections
-------------------------------------------------------------------------------}

type ByronCoinSelections n =
    ByronSelectCoins n

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/byronSelectCoins
type ByronSelectCoins n = "byron-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "coin-selections"
    :> "random"
    :> ReqBody '[JSON] (ApiSelectCoinsDataT n)
    :> Post '[JSON] (ApiCoinSelectionT n)

{-------------------------------------------------------------------------------
                                 Byron Transactions

  See also: https://input-output-hk.github.io/cardano-wallet/api/#tag/Byron-Transactions
-------------------------------------------------------------------------------}

type ByronTransactions n =
         ConstructByronTransaction n
    :<|> SignByronTransaction n
    :<|> ListByronTransactions n
    :<|> GetByronTransaction n
    :<|> DeleteByronTransaction
    :<|> CreateByronTransactionOld n
    :<|> PostByronTransactionFeeOld n

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/constructByronTransaction
type ConstructByronTransaction n = "byron-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions-construct"
    :> ReqBody '[JSON] (ApiConstructTransactionDataT n)
    :> PostAccepted '[JSON] (ApiConstructTransactionT n)

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/signByronTransaction
type SignByronTransaction n = "byron-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions-sign"
    :> ReqBody '[JSON] ApiSignTransactionPostData
    :> PostAccepted '[JSON] ApiSerialisedTransaction

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/postByronTransaction
type CreateByronTransactionOld n = "byron-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions"
    :> ReqBody '[JSON] (PostTransactionOldDataT n)
    :> PostAccepted '[JSON] (ApiTransactionT n)

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/listByronTransactions
type ListByronTransactions n = "byron-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions"
    :> QueryParam "start" Iso8601Time
    :> QueryParam "end" Iso8601Time
    :> QueryParam "order" (ApiT SortOrder)
    :> Get '[JSON] [ApiTransactionT n]

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/getByronTransaction
type GetByronTransaction n = "byron-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions"
    :> Capture "transactionId" ApiTxId
    :> Get '[JSON] (ApiTransactionT n)

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/postByronTransactionFee
type PostByronTransactionFeeOld n = "byron-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "payment-fees"
    :> ReqBody '[JSON] (PostTransactionFeeOldDataT n)
    :> PostAccepted '[JSON] ApiFee

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/deleteByronTransaction
type DeleteByronTransaction = "byron-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions"
    :> Capture "transactionId" ApiTxId
    :> DeleteNoContent

{-------------------------------------------------------------------------------
                                 Byron Migrations

  See also: https://input-output-hk.github.io/cardano-wallet/api/#tag/Byron-Migrations
-------------------------------------------------------------------------------}

type ByronMigrations n =
         CreateByronWalletMigrationPlan n
    :<|> MigrateByronWallet n

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/migrateByronWallet
type MigrateByronWallet n = "byron-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "migrations"
    :> ReqBody '[JSON] (ApiWalletMigrationPostDataT n "lenient")
    :> PostAccepted '[JSON] (NonEmpty (ApiTransactionT n))

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/createByronWalletMigrationPlan
type CreateByronWalletMigrationPlan n = "byron-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "migrations"
    :> "plan"
    :> ReqBody '[JSON] (ApiWalletMigrationPlanPostDataT n)
    :> PostAccepted '[JSON] (ApiWalletMigrationPlan n)

{-------------------------------------------------------------------------------
                                  Network

  See also: https://input-output-hk.github.io/cardano-wallet/api/#tag/Network
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
                                  SMASH

-------------------------------------------------------------------------------}

type SMASH = GetCurrentSMASHHealth

type GetCurrentSMASHHealth = "smash"
    :> "health"
    :> QueryParam "url" (ApiT SmashServer)
    :> Get '[JSON] ApiHealthCheck

{-------------------------------------------------------------------------------
                                  Shared Wallets

  See also: https://input-output-hk.github.io/cardano-wallet/api/#tag/Shared-Wallets
-------------------------------------------------------------------------------}

type SharedWallets =
         PostSharedWallet
    :<|> GetSharedWallet
    :<|> ListSharedWallets
    :<|> PatchSharedWalletInPayment
    :<|> PatchSharedWalletInDelegation
    :<|> DeleteSharedWallet

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/postSharedWallet
type PostSharedWallet = "shared-wallets"
    :> ReqBody '[JSON] ApiSharedWalletPostData
    :> PostCreated '[JSON] ApiSharedWallet

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/getSharedWallet
type GetSharedWallet = "shared-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> Get '[JSON] ApiSharedWallet

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/listSharedWallets
type ListSharedWallets = "shared-wallets"
    :> Get '[JSON] [ApiSharedWallet]

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/patchSharedWalletInPayment
type PatchSharedWalletInPayment = "shared-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "payment-script-template"
    :> ReqBody '[JSON] ApiSharedWalletPatchData
    :> Patch '[JSON] ApiSharedWallet

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/patchSharedWalletInDelegation
type PatchSharedWalletInDelegation = "shared-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "delegation-script-template"
    :> ReqBody '[JSON] ApiSharedWalletPatchData
    :> Patch '[JSON] ApiSharedWallet

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/deleteSharedWallet
type DeleteSharedWallet = "shared-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> DeleteNoContent

{-------------------------------------------------------------------------------
                                  Shared Wallet Keys
  See also: https://input-output-hk.github.io/cardano-wallet/api/#tag/Keys
-------------------------------------------------------------------------------}

type SharedWalletKeys =
         GetSharedWalletKey
    :<|> PostAccountKeyShared
    :<|> GetAccountKeyShared

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/getSharedWalletKey
type GetSharedWalletKey = "shared-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "keys"
    :> Capture "role" (ApiT Role)
    :> Capture "index" (ApiT DerivationIndex)
    :> QueryParam "hash" Bool
    :> Get '[JSON] ApiVerificationKeyShared

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/postAccountKeyShared
type PostAccountKeyShared = "shared-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "keys"
    :> Capture "index" (ApiT DerivationIndex)
    :> ReqBody '[JSON] ApiPostAccountKeyData
    :> PostAccepted '[JSON] ApiAccountKeyShared

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/getAccountKeyShared
type GetAccountKeyShared = "shared-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "keys"
    :> QueryParam "format" KeyFormat
    :> Get '[JSON] ApiAccountKeyShared

{-------------------------------------------------------------------------------
                                 Shared Addresses

  See also: https://input-output-hk.github.io/cardano-wallet/api/#tag/Shared-Addresses
-------------------------------------------------------------------------------}

type SharedAddresses n =
    ListSharedAddresses n

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/listSharedAddresses
type ListSharedAddresses n = "shared-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "addresses"
    :> QueryParam "state" (ApiT AddressState)
    :> Get '[JSON] [ApiAddressT n]

{-------------------------------------------------------------------------------
                                   Proxy_

  See also: https://input-output-hk.github.io/cardano-wallet/api/edge/#tag/Proxy
-------------------------------------------------------------------------------}

type Proxy_ =
    PostExternalTransaction

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/postExternalTransaction
type PostExternalTransaction = "proxy"
    :> "transactions"
    :> ReqBody '[OctetStream] (ApiT SealedTx)
    :> PostAccepted '[JSON] ApiTxId

{-------------------------------------------------------------------------------
                               Api Layer
-------------------------------------------------------------------------------}

data ApiLayer s (k :: Depth -> Type -> Type)
    = ApiLayer
        (Tracer IO TxSubmitLog)
        (Tracer IO (WorkerLog WalletId WalletWorkerLog))
        (Block, NetworkParameters, SyncTolerance)
        (NetworkLayer IO Block)
        (TransactionLayer k SealedTx)
        (DBFactory IO s k)
        (WorkerRegistry WalletId (DBLayer IO s k))
        (Concierge IO WalletLock)
        (TokenMetadataClient IO)
    deriving (Generic)

-- | Locks that are held by the wallet in order to enforce
-- sequential execution of some API actions.
-- Used with "Control.Concurrent.Concierge".
data WalletLock = PostTransactionOld WalletId
    deriving (Eq, Ord, Show)

instance HasWorkerCtx (DBLayer IO s k) (ApiLayer s k) where
    type WorkerCtx (ApiLayer s k) = WalletLayer IO s k
    type WorkerMsg (ApiLayer s k) = WalletWorkerLog
    type WorkerKey (ApiLayer s k) = WalletId
    hoistResource db transform (ApiLayer _ tr gp nw tl _ _ _ _) =
        WalletLayer (contramap transform tr) gp nw tl db

{-------------------------------------------------------------------------------
                               Capabilities
-------------------------------------------------------------------------------}

type HasWorkerRegistry s k ctx =
    ( HasType (WorkerRegistry WalletId (DBLayer IO s k)) ctx
    , HasWorkerCtx (DBLayer IO s k) ctx
    , WorkerKey ctx ~ WalletId
    , WorkerMsg ctx ~ WalletWorkerLog
    )

workerRegistry
    :: forall s k ctx. (HasWorkerRegistry s k ctx)
    => Lens' ctx (WorkerRegistry WalletId (DBLayer IO s k))
workerRegistry =
    typed @(WorkerRegistry WalletId (DBLayer IO s k))

type HasDBFactory s k = HasType (DBFactory IO s k)
type HasTokenMetadataClient = HasType (TokenMetadataClient IO)

dbFactory
    :: forall s k ctx. (HasDBFactory s k ctx)
    => Lens' ctx (DBFactory IO s k)
dbFactory =
    typed @(DBFactory IO s k)

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
