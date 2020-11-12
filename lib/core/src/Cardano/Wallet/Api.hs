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

    , WalletKeys
        , GetWalletKey
        , SignMetadata

    , Addresses
        , ListAddresses
        , InspectAddress
        , PostAnyAddress

    , CoinSelections
        , SelectCoins

    , Transactions
        , CreateTransaction
        , PostTransactionFee
        , ListTransactions
        , DeleteTransaction
        , GetTransaction

    , StakePools
        , ListStakePools
        , JoinStakePool
        , QuitStakePool
        , DelegationFee
        , PostPoolMaintenance
        , GetPoolMaintenance

    , ShelleyMigrations
        , MigrateShelleyWallet
        , GetShelleyWalletMigrationInfo

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
        , PutByronWalletPassphrase

    , ByronAddresses
        , PostByronAddress
        , PutByronAddress
        , PutByronAddresses
        , ListByronAddresses

    , ByronCoinSelections
        , ByronSelectCoins

    , ByronTransactions
        , CreateByronTransaction
        , ListByronTransactions
        , PostByronTransactionFee
        , DeleteByronTransaction
        , GetByronTransaction

    , ByronMigrations
        , MigrateByronWallet
        , GetByronWalletMigrationInfo

    -- * Miscellaneous
    , Network
        , GetNetworkInformation
        , GetNetworkParameters
        , GetNetworkClock

    , Proxy_
        , PostExternalTransaction

      -- * Api Layer
    , ApiLayer (..)
    , HasWorkerRegistry
    , workerRegistry
    , HasDBFactory
    , dbFactory
    ) where

import Prelude

import Cardano.Wallet
    ( WalletLayer (..), WalletLog )
import Cardano.Wallet.Api.Types
    ( AnyAddress
    , ApiAddressData
    , ApiAddressIdT
    , ApiAddressInspect
    , ApiAddressInspectData
    , ApiAddressT
    , ApiByronWallet
    , ApiCoinSelectionT
    , ApiFee
    , ApiMaintenanceAction
    , ApiMaintenanceActionPostData
    , ApiNetworkClock
    , ApiNetworkInformation
    , ApiNetworkParameters
    , ApiPoolId
    , ApiPostRandomAddressData
    , ApiPutAddressesDataT
    , ApiSelectCoinsDataT
    , ApiT
    , ApiTransactionT
    , ApiTxId
    , ApiUtxoStatistics
    , ApiVerificationKey
    , ApiWallet
    , ApiWalletMigrationInfo
    , ApiWalletMigrationPostDataT
    , ApiWalletPassphrase
    , ApiWalletSignData
    , ByronWalletPutPassphraseData
    , Iso8601Time
    , MinWithdrawal
    , PostExternalTransactionData
    , PostTransactionDataT
    , PostTransactionFeeDataT
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
    ( AccountingStyle, Depth, DerivationIndex )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncTolerance )
import Cardano.Wallet.Primitive.Types
    ( AddressState
    , Block
    , Coin (..)
    , NetworkParameters
    , SortOrder (..)
    , WalletId (..)
    )
import Cardano.Wallet.Registry
    ( HasWorkerCtx (..), WorkerLog, WorkerRegistry )
import Cardano.Wallet.Transaction
    ( TransactionLayer )
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
--
-- The API used in cardano-wallet-jormungandr may differ from this one.
type Api n apiPool =
         Wallets
    :<|> WalletKeys
    :<|> Addresses n
    :<|> CoinSelections n
    :<|> Transactions n
    :<|> ShelleyMigrations n
    :<|> StakePools n apiPool
    :<|> ByronWallets
    :<|> ByronAddresses n
    :<|> ByronCoinSelections n
    :<|> ByronTransactions n
    :<|> ByronMigrations n
    :<|> Network
    :<|> Proxy_
    :<|> Settings

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

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/getUTxOsStatistics
type GetUTxOsStatistics = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "statistics"
    :> "utxos"
    :> Get '[JSON] ApiUtxoStatistics

{-------------------------------------------------------------------------------
                                  Wallet Keys
  See also: https://input-output-hk.github.io/cardano-wallet/api/#tag/WalletKeys
-------------------------------------------------------------------------------}

type WalletKeys =
    GetWalletKey
    :<|> SignMetadata

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/getWalletKey
type GetWalletKey = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "keys"
    :> Capture "role" (ApiT AccountingStyle)
    :> Capture "index" (ApiT DerivationIndex)
    :> Get '[JSON] ApiVerificationKey

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/signMetadata
type SignMetadata = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "signatures"
    :> Capture "role" (ApiT AccountingStyle)
    :> Capture "index" (ApiT DerivationIndex)
    :> ReqBody '[JSON] ApiWalletSignData
    :> Post '[OctetStream] ByteString

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
                                  Transactions

  See also: https://input-output-hk.github.io/cardano-wallet/api/#tag/Transactions
-------------------------------------------------------------------------------}

type Transactions n =
    CreateTransaction n
    :<|> ListTransactions n
    :<|> PostTransactionFee n
    :<|> DeleteTransaction
    :<|> GetTransaction n

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/postTransaction
type CreateTransaction n = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions"
    :> ReqBody '[JSON] (PostTransactionDataT n)
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
type PostTransactionFee n = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "payment-fees"
    :> ReqBody '[JSON] (PostTransactionFeeDataT n)
    :> PostAccepted '[JSON] ApiFee

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/deleteTransaction
type DeleteTransaction = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions"
    :> Capture "transactionId" ApiTxId
    :> DeleteNoContent

{-------------------------------------------------------------------------------
                                 Shelley Migrations

See also:
https://input-output-hk.github.io/cardano-wallet/api/#tag/Migrations
-------------------------------------------------------------------------------}

type ShelleyMigrations n =
         GetShelleyWalletMigrationInfo
    :<|> MigrateShelleyWallet n

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/migrateShelleyWallet
type MigrateShelleyWallet n = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "migrations"
    :> ReqBody '[JSON] (ApiWalletMigrationPostDataT n "raw")
    :> PostAccepted '[JSON] [ApiTransactionT n]

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/getShelleyWalletMigrationInfo
type GetShelleyWalletMigrationInfo = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "migrations"
    :> Get '[JSON] ApiWalletMigrationInfo

{-------------------------------------------------------------------------------
                                  StakePools

  See also: https://input-output-hk.github.io/cardano-wallet/api/edge/#tag/Stake-Pools
-------------------------------------------------------------------------------}

type StakePools n apiPool =
    ListStakePools apiPool
    :<|> JoinStakePool n
    :<|> QuitStakePool n
    :<|> DelegationFee
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
    CreateByronTransaction n
    :<|> ListByronTransactions n
    :<|> PostByronTransactionFee n
    :<|> DeleteByronTransaction
    :<|> GetByronTransaction n

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/postByronTransaction
type CreateByronTransaction n = "byron-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions"
    :> ReqBody '[JSON] (PostTransactionDataT n)
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
type PostByronTransactionFee n = "byron-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "payment-fees"
    :> ReqBody '[JSON] (PostTransactionFeeDataT n)
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
         GetByronWalletMigrationInfo
    :<|> MigrateByronWallet n

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/migrateByronWallet
type MigrateByronWallet n = "byron-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "migrations"
    :> ReqBody '[JSON] (ApiWalletMigrationPostDataT n "lenient")
    :> PostAccepted '[JSON] [ApiTransactionT n]

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/getByronWalletMigrationInfo
type GetByronWalletMigrationInfo = "byron-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "migrations"
    :> Get '[JSON] ApiWalletMigrationInfo

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
                                   Proxy_

  See also: https://input-output-hk.github.io/cardano-wallet/api/edge/#tag/Proxy
-------------------------------------------------------------------------------}

type Proxy_ =
    PostExternalTransaction

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/postExternalTransaction
type PostExternalTransaction = "proxy"
    :> "transactions"
    :> ReqBody '[OctetStream] PostExternalTransactionData
    :> PostAccepted '[JSON] ApiTxId

{-------------------------------------------------------------------------------
                               Api Layer
-------------------------------------------------------------------------------}

data ApiLayer s t (k :: Depth -> * -> *)
    = ApiLayer
        (Tracer IO (WorkerLog WalletId WalletLog))
        (Block, NetworkParameters, SyncTolerance)
        (NetworkLayer IO t (Block))
        (TransactionLayer t k)
        (DBFactory IO s k)
        (WorkerRegistry WalletId (DBLayer IO s k))
    deriving (Generic)

instance HasWorkerCtx (DBLayer IO s k) (ApiLayer s t k) where
    type WorkerCtx (ApiLayer s t k) = WalletLayer s t k
    type WorkerMsg (ApiLayer s t k) = WalletLog
    type WorkerKey (ApiLayer s t k) = WalletId
    hoistResource db transform (ApiLayer tr gp nw tl _ _) =
        WalletLayer (contramap transform tr) gp nw tl db

{-------------------------------------------------------------------------------
                               Capabilities
-------------------------------------------------------------------------------}

type HasWorkerRegistry s k ctx =
    ( HasType (WorkerRegistry WalletId (DBLayer IO s k)) ctx
    , HasWorkerCtx (DBLayer IO s k) ctx
    , WorkerKey ctx ~ WalletId
    , WorkerMsg ctx ~ WalletLog
    )

workerRegistry
    :: forall s k ctx. (HasWorkerRegistry s k ctx)
    => Lens' ctx (WorkerRegistry WalletId (DBLayer IO s k))
workerRegistry =
    typed @(WorkerRegistry WalletId (DBLayer IO s k))

type HasDBFactory s k = HasType (DBFactory IO s k)

dbFactory
    :: forall s k ctx. (HasDBFactory s k ctx)
    => Lens' ctx (DBFactory IO s k)
dbFactory =
    typed @(DBFactory IO s k)

{-------------------------------------------------------------------------------
                              Type Families
-------------------------------------------------------------------------------}

type family PostData wallet :: * where
    PostData ApiWallet = WalletOrAccountPostData
    PostData ApiByronWallet = SomeByronWalletPostData
