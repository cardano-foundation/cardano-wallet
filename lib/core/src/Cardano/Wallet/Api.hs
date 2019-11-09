{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Wallet.Api
    ( -- * API
      Api

      -- * Core API
    , CoreApi
    , Addresses
    , Wallets
    , Transactions

      -- * Stake Pool API
    , StakePoolApi

      -- * Compatibility API
    , CompatibilityApi

      -- * Api Layer
    , ApiLayer (..)
    , HasWorkerRegistry
    , workerRegistry
    , HasDBFactory
    , dbFactory

      -- * Miscellaneous Types
    , Any

    ) where

import Prelude

import Cardano.BM.Trace
    ( Trace )
import Cardano.Wallet
    ( WalletLayer (..) )
import Cardano.Wallet.Api.Types
    ( ApiAddress
    , ApiByronWallet
    , ApiByronWalletMigrationInfo
    , ApiFee
    , ApiNetworkInformation
    , ApiStakePool
    , ApiT
    , ApiTransaction
    , ApiTxId
    , ApiUtxoStatistics
    , ApiWallet
    , ApiWalletPassphrase
    , ByronWalletPostData
    , Iso8601Time
    , PostExternalTransactionData
    , PostTransactionData
    , PostTransactionFeeData
    , WalletPostData
    , WalletPutData
    , WalletPutPassphraseData
    )
import Cardano.Wallet.DB
    ( DBFactory, DBLayer )
import Cardano.Wallet.Network
    ( NetworkLayer )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth )
import Cardano.Wallet.Primitive.Model
    ( BlockchainParameters )
import Cardano.Wallet.Primitive.Types
    ( AddressState
    , Block
    , PoolId
    , SortOrder (..)
    , SyncTolerance
    , WalletId (..)
    )
import Cardano.Wallet.Registry
    ( HasWorkerCtx (..), WorkerRegistry )
import Cardano.Wallet.Transaction
    ( TransactionLayer )
import Data.Generics.Internal.VL.Lens
    ( Lens' )
import Data.Generics.Labels
    ()
import Data.Generics.Product.Typed
    ( HasType, typed )
import Data.List.NonEmpty
    ( NonEmpty ((:|)) )
import Data.Text
    ( Text )
import GHC.Generics
    ( Generic )
import Network.HTTP.Media
    ( (//), (/:) )
import Servant.API
    ( (:<|>)
    , (:>)
    , Accept (..)
    , Capture
    , DeleteAccepted
    , DeleteNoContent
    , Get
    , JSON
    , NoContent
    , OctetStream
    , PostAccepted
    , Put
    , PutNoContent
    , QueryParam
    , ReqBody
    )

type Api t = CoreApi t :<|> CompatibilityApi t :<|> StakePoolApi t

type CoreApi t =
    Addresses t
    :<|> Wallets
    :<|> Transactions t
    :<|> Network

type StakePoolApi t =
    ListStakePools
    :<|> JoinStakePool t
    :<|> QuitStakePool t

type CompatibilityApi n =
    DeleteByronWallet
    :<|> GetByronWallet
    :<|> GetByronWalletMigrationInfo
    :<|> ListByronWallets
    :<|> ListByronTransactions n
    :<|> MigrateByronWallet n
    :<|> PostByronWallet
    :<|> DeleteByronTransaction

{-------------------------------------------------------------------------------
                                  Addresses

  See also: https://input-output-hk.github.io/cardano-wallet/api/#tag/Addresses
-------------------------------------------------------------------------------}

type Addresses t =
    ListAddresses t

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/listAddresses
type ListAddresses t = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "addresses"
    :> QueryParam "state" (ApiT AddressState)
    :> Get '[JSON] [ApiAddress t]

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
    :> DeleteNoContent '[Any] NoContent

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/getWallet
type GetWallet = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> Get '[JSON] ApiWallet

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/listWallets
type ListWallets = "wallets"
    :> Get '[JSON] [ApiWallet]

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/postWallet
type PostWallet = "wallets"
    :> ReqBody '[JSON] WalletPostData
    :> PostAccepted '[JSON] ApiWallet

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
    :> PutNoContent '[Any] NoContent

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/getUTxOsStatistics
type GetUTxOsStatistics = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "statistics"
    :> "utxos"
    :> Get '[JSON] ApiUtxoStatistics

{-------------------------------------------------------------------------------
                                  Transactions

  See also: https://input-output-hk.github.io/cardano-wallet/api/#tag/Transactions
-------------------------------------------------------------------------------}

type Transactions n =
    CreateTransaction n
    :<|> ListTransactions n
    :<|> PostTransactionFee n
    :<|> PostExternalTransaction
    :<|> DeleteTransaction

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/postTransaction
type CreateTransaction n = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions"
    :> ReqBody '[JSON] (PostTransactionData n)
    :> PostAccepted '[JSON] (ApiTransaction n)

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/postTransactionFee
type PostTransactionFee n = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions"
    :> "fees"
    :> ReqBody '[JSON] (PostTransactionFeeData n)
    :> PostAccepted '[JSON] ApiFee

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/listTransaction
type ListTransactions n = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions"
    :> QueryParam "start" Iso8601Time
    :> QueryParam "end" Iso8601Time
    :> QueryParam "order" (ApiT SortOrder)
    :> Get '[JSON] [ApiTransaction n]

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/deleteTransaction
type DeleteTransaction = "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions"
    :> Capture "transactionId" ApiTxId
    :> DeleteNoContent '[Any] NoContent

{-------------------------------------------------------------------------------
                                  StakePools

  See also: https://input-output-hk.github.io/cardano-wallet/api/edge/#tag/Stake-Pools
-------------------------------------------------------------------------------}

-- | https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/listStakePools
type ListStakePools = "stake-pools"
    :> Get '[JSON] [ApiStakePool]

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/joinStakePool
type JoinStakePool t = "stake-pools"
    :> Capture "stakePoolId" (ApiT PoolId)
    :> "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> ReqBody '[JSON] ApiWalletPassphrase
    :> Put '[JSON] (ApiTransaction t)

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/quitStakePool
type QuitStakePool t = "stake-pools"
    :> Capture "stakePoolId" (ApiT PoolId)
    :> "wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> ReqBody '[JSON] ApiWalletPassphrase
    :> DeleteAccepted '[JSON] (ApiTransaction t)

{-------------------------------------------------------------------------------
                                  Network

  See also: https://input-output-hk.github.io/cardano-wallet/api/#tag/Network
-------------------------------------------------------------------------------}

type Network =
    "network"
    :> "information"
    :> Get '[JSON] ApiNetworkInformation

{-------------------------------------------------------------------------------
                                  Proxy

  See also: https://input-output-hk.github.io/cardano-wallet/api/edge/#tag/Proxy
-------------------------------------------------------------------------------}

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/postExternalTransaction
type PostExternalTransaction = "proxy"
    :> "transactions"
    :> ReqBody '[OctetStream] PostExternalTransactionData
    :> PostAccepted '[JSON] ApiTxId

{-------------------------------------------------------------------------------
                              Compatibility API

  See also: https://input-output-hk.github.io/cardano-wallet/api/#tag/Byron-Wallets
-------------------------------------------------------------------------------}

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/postByronWallet
type PostByronWallet = "byron-wallets"
    :> ReqBody '[JSON] ByronWalletPostData
    :> PostAccepted '[JSON] ApiByronWallet

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/deleteByronWallet
type DeleteByronWallet = "byron-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> DeleteNoContent '[Any] NoContent

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/getByronWallet
type GetByronWallet = "byron-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> Get '[JSON] ApiByronWallet

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/getByronWalletMigrationInfo
type GetByronWalletMigrationInfo = "byron-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "migrations"
    :> Get '[JSON] ApiByronWalletMigrationInfo

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/listByronWallets
type ListByronWallets = "byron-wallets"
    :> Get '[JSON] [ApiByronWallet]

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/listByronTransactions
type ListByronTransactions n = "byron-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions"
    :> QueryParam "start" Iso8601Time
    :> QueryParam "end" Iso8601Time
    :> QueryParam "order" (ApiT SortOrder)
    :> Get '[JSON] [ApiTransaction n]

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/migrateByronWallet
type MigrateByronWallet n = "byron-wallets"
    :> Capture "sourceWalletId" (ApiT WalletId)
    :> "migrations"
    :> Capture "targetWalletId" (ApiT WalletId)
    :> ReqBody '[JSON] ApiWalletPassphrase
    :> PostAccepted '[JSON] [ApiTransaction n]

-- | https://input-output-hk.github.io/cardano-wallet/api/#operation/deleteByronTransaction
type DeleteByronTransaction = "byron-wallets"
    :> Capture "walletId" (ApiT WalletId)
    :> "transactions"
    :> Capture "transactionId" ApiTxId
    :> DeleteNoContent '[Any] NoContent

{-------------------------------------------------------------------------------
                                   Internals
-------------------------------------------------------------------------------}

-- | Any media type
data Any

instance Accept Any where
    contentTypes _ = ("*" // "*") :|
        -- We also 'conveniently' accept JSON format
        [ "application" // "json"
        , "application" // "json" /: ("charset", "utf-8")
        ]

{-------------------------------------------------------------------------------
                               Api Layer
-------------------------------------------------------------------------------}

data ApiLayer s t (k :: Depth -> * -> *)
    = ApiLayer
        (Trace IO Text)
        (Block, BlockchainParameters, SyncTolerance)
        (NetworkLayer IO t (Block))
        (TransactionLayer t k)
        (DBFactory IO s k)
        (WorkerRegistry WalletId (DBLayer IO s k))
    deriving (Generic)

instance HasWorkerCtx (DBLayer IO s k) (ApiLayer s t k) where
    type WorkerCtx (ApiLayer s t k) = WalletLayer s t k
    hoistResource db (ApiLayer tr bp nw tl _ _) =
        WalletLayer tr bp nw tl db

{-------------------------------------------------------------------------------
                               Capabilities
-------------------------------------------------------------------------------}

type HasWorkerRegistry s k ctx =
    ( HasType (WorkerRegistry WalletId (DBLayer IO s k)) ctx
    , HasWorkerCtx (DBLayer IO s k) ctx
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
