{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Wallet.Api.Clients.Shelley
where

import Prelude

import Cardano.Wallet.Api
    ( Assets
    , BalanceTransaction
    , ConstructTransaction
    , CreateShelleyWalletMigrationPlan
    , CreateTransactionOld
    , DecodeTransaction
    , DeleteTransaction
    , GetTransaction
    , InspectAddress
    , JoinStakePool
    , ListAddresses
    , ListStakePools
    , ListTransactions
    , MigrateShelleyWallet
    , Network
    , PostAnyAddress
    , PostTransactionFeeOld
    , Proxy_
    , QuitStakePool
    , SignTransaction
    , SubmitTransaction
    , Wallets
    )

import Cardano.Wallet.Api.Types
    ( AnyAddress
    , ApiAddressData
    , ApiAddressInspect
    , ApiAddressInspectData
    , ApiAddressWithPath
    , ApiAsset
    , ApiBalanceTransactionPostData
    , ApiConstructTransaction
    , ApiConstructTransactionData
    , ApiDecodeTransactionPostData
    , ApiFee
    , ApiNetworkClock
    , ApiNetworkInformation
    , ApiNetworkParameters
    , ApiPoolSpecifier
    , ApiSerialisedTransaction
    , ApiSignTransactionPostData
    , ApiT
    , ApiTransaction
    , ApiTxId
    , ApiUtxoStatistics
    , ApiWallet
    , ApiWalletMigrationPlan
    , ApiWalletMigrationPlanPostData
    , ApiWalletMigrationPostData
    , ApiWalletPassphrase
    , ApiWalletPutDataExtended
    , ApiWalletUtxoSnapshot
    , Iso8601Time
    , MinWithdrawal
    , PostTransactionFeeOldData
    , PostTransactionOldData
    , WalletOrAccountPostData
    , WalletPutPassphraseData
    )
import Cardano.Wallet.Api.Types.Transaction
    ( ApiAddress
    , ApiDecodedTransaction
    , ApiLimit
    )
import Cardano.Wallet.Pools
    ( StakePool
    )
import Cardano.Wallet.Primitive.NetworkId
    ( HasSNetworkId
    , NetworkDiscriminant
    )
import Cardano.Wallet.Primitive.Types
    ( SortOrder
    , WalletId
    )
import Cardano.Wallet.Primitive.Types.Address
    ( AddressState
    )
import Cardano.Wallet.Primitive.Types.AssetName
    ( AssetName
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin
    )
import Cardano.Wallet.Primitive.Types.TokenPolicyId
    ( TokenPolicyId
    )
import Cardano.Wallet.Primitive.Types.Tx
    ( SealedTx
    )
import Data.Generics.Labels
    ()
import Data.List.NonEmpty
    ( NonEmpty
    )
import Data.Proxy
    ( Proxy (..)
    )
import Servant.API
    ( NoContent
    , type (:<|>) ((:<|>))
    , type (:>)
    )
import Servant.Client
    ( ClientM
    , client
    )

deleteWallet
    :: ApiT WalletId -> ClientM NoContent
getWallet
    :: ApiT WalletId -> ClientM ApiWallet
listWallets
    :: ClientM [ApiWallet]
postWallet
    :: WalletOrAccountPostData -> ClientM ApiWallet
putWallet
    :: ApiT WalletId -> ApiWalletPutDataExtended -> ClientM ApiWallet
putWalletPassphrase
    :: ApiT WalletId
    -> WalletPutPassphraseData
    -> ClientM NoContent
getWalletUtxoSnapshot
    :: ApiT WalletId -> ClientM ApiWalletUtxoSnapshot
getWalletUtxoStatistics
    :: ApiT WalletId -> ClientM ApiUtxoStatistics
deleteWallet
    :<|> getWallet
    :<|> listWallets
    :<|> postWallet
    :<|> putWallet
    :<|> putWalletPassphrase
    :<|> getWalletUtxoSnapshot
    :<|> getWalletUtxoStatistics =
        client (Proxy @("v2" :> Wallets))

constructTransaction
    :: forall network
     . HasSNetworkId network
    => ApiT WalletId
    -> ApiConstructTransactionData network
    -> ClientM (ApiConstructTransaction network)
constructTransaction =
    client (Proxy @("v2" :> ConstructTransaction network))

signTransaction
    :: forall network
     . Proxy (network :: NetworkDiscriminant)
    -> ApiT WalletId
    -> ApiSignTransactionPostData
    -> ClientM ApiSerialisedTransaction
signTransaction _ =
    client (Proxy @("v2" :> SignTransaction network))

listTransactions
    :: forall network
     . HasSNetworkId network
    => ApiT WalletId
    -> Maybe MinWithdrawal
    -> Maybe Iso8601Time
    -> Maybe Iso8601Time
    -> Maybe (ApiT SortOrder)
    -> Maybe ApiLimit
    -> Maybe (ApiAddress network)
    -> Bool
    -> ClientM [ApiTransaction network]
listTransactions =
    client (Proxy @("v2" :> ListTransactions network))

getTransaction
    :: forall network
     . HasSNetworkId network
    => ApiT WalletId
    -> ApiTxId
    -> Bool
    -> ClientM (ApiTransaction network)
getTransaction =
    client (Proxy @("v2" :> GetTransaction network))

deleteTransaction
    :: ApiT WalletId
    -> ApiTxId
    -> ClientM NoContent
deleteTransaction =
    client (Proxy @("v2" :> DeleteTransaction))

postTransaction
    :: forall network
     . HasSNetworkId network
    => ApiT WalletId
    -> PostTransactionOldData network
    -> ClientM (ApiTransaction network)
postTransaction =
    client (Proxy @("v2" :> CreateTransactionOld network))

postTransactionFee
    :: forall network
     . HasSNetworkId network
    => ApiT WalletId
    -> PostTransactionFeeOldData network
    -> ClientM ApiFee
postTransactionFee =
    client (Proxy @("v2" :> PostTransactionFeeOld network))

balanceTransaction
    :: forall network
     . HasSNetworkId network
    => ApiT WalletId
    -> ApiBalanceTransactionPostData network
    -> ClientM ApiSerialisedTransaction
balanceTransaction =
    client (Proxy @("v2" :> BalanceTransaction network))

decodeTransaction
    :: forall network
     . HasSNetworkId network
    => ApiT WalletId
    -> ApiDecodeTransactionPostData
    -> ClientM (ApiDecodedTransaction network)
decodeTransaction =
    client (Proxy @("v2" :> DecodeTransaction network))

submitTransaction
    :: ApiT WalletId
    -> ApiSerialisedTransaction
    -> ClientM ApiTxId
submitTransaction =
    client (Proxy @("v2" :> SubmitTransaction))

postExternalTransaction
    :: ApiT SealedTx -> ClientM ApiTxId
postExternalTransaction =
    client (Proxy @("v2" :> Proxy_))

listAddresses
    :: forall network
     . HasSNetworkId network
    => ApiT WalletId
    -> Maybe (ApiT AddressState)
    -> ClientM [ApiAddressWithPath network]
listAddresses = client (Proxy @("v2" :> ListAddresses network))

inspectAddress
    :: ApiAddressInspectData -> ClientM ApiAddressInspect
inspectAddress = client (Proxy @("v2" :> InspectAddress))

postScriptAddress
    :: forall (network :: NetworkDiscriminant)
    . Proxy network
    -> ApiAddressData -> ClientM AnyAddress
postScriptAddress _ = client (Proxy @("v2" :> PostAnyAddress network))

listPools
    :: Maybe (ApiT Coin) -> ClientM [ApiT StakePool]
listPools = client (Proxy @("v2" :> ListStakePools))

joinStakePool
    :: forall network
     . HasSNetworkId network
    => ApiPoolSpecifier
    -> ApiT WalletId
    -> ApiWalletPassphrase
    -> ClientM (ApiTransaction network)
joinStakePool = client (Proxy @("v2" :> JoinStakePool network))

quitStakePool
    :: forall network
     . HasSNetworkId network
    => ApiT WalletId
    -> ApiWalletPassphrase
    -> ClientM (ApiTransaction network)
quitStakePool = client (Proxy @("v2" :> QuitStakePool network))

networkInformation
    :: ClientM ApiNetworkInformation
networkParameters
    :: ClientM ApiNetworkParameters
networkClock
    :: Bool -> ClientM ApiNetworkClock
networkInformation :<|> networkParameters :<|> networkClock =
    client (Proxy @("v2" :> Network))

getAssets
    :: ApiT WalletId -> ClientM [ApiAsset]
getAsset
    :: ApiT WalletId -> ApiT TokenPolicyId -> ApiT AssetName -> ClientM ApiAsset
getAsset'
    :: ApiT WalletId -> ApiT TokenPolicyId -> ClientM ApiAsset
getAssets :<|> getAsset :<|> getAsset' =
    client (Proxy @("v2" :> Assets))

planMigration
    :: forall network
     . HasSNetworkId network
    => ApiT WalletId
    -> ApiWalletMigrationPlanPostData network
    -> ClientM (ApiWalletMigrationPlan network)
planMigration = client (Proxy @("v2" :> CreateShelleyWalletMigrationPlan network))

migrate
    :: forall network
     . HasSNetworkId network
    => ApiT WalletId
    -> ApiWalletMigrationPostData network "user"
    -> ClientM (NonEmpty (ApiTransaction network))
migrate = client (Proxy @("v2" :> MigrateShelleyWallet network))
