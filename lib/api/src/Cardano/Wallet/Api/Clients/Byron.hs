{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Wallet.Api.Clients.Byron
    ( deleteWallet
    , deleteTransaction
    , getTransaction
    , getWallet
    , getWalletUtxoSnapshot
    , getWalletUtxoStatistics
    , listAddresses
    , listTransactions
    , listWallets
    , postExternalTransaction
    , postRandomAddress
    , postTransaction
    , postTransactionFee
    , postWallet
    , putRandomAddress
    , putRandomAddresses
    , putWalletByron
    , putWalletPassphrase
    )
where

import Prelude

import Cardano.Wallet.Api
    ( ByronWallets
    , CreateByronTransactionOld
    , DeleteByronTransaction
    , GetByronTransaction
    , ListByronAddresses
    , ListByronTransactions
    , PostByronAddress
    , PostByronTransactionFeeOld
    , Proxy_
    , PutByronAddress
    , PutByronAddresses
    )
import Cardano.Wallet.Api.Types
    ( ApiAddressIdT
    , ApiAddressT
    , ApiByronWallet
    , ApiFee
    , ApiPostRandomAddressData
    , ApiPutAddressesDataT
    , ApiT (..)
    , ApiTransactionT
    , ApiTxId (..)
    , ApiUtxoStatistics
    , ApiWalletPutData (..)
    , ApiWalletUtxoSnapshot (..)
    , ByronWalletPutPassphraseData (..)
    , Iso8601Time (..)
    , PostTransactionFeeOldDataT
    , PostTransactionOldDataT
    , SomeByronWalletPostData
    )
import Cardano.Wallet.Api.Types.Transaction
    ( ApiLimit
    )
import Cardano.Wallet.Primitive.NetworkId
    ( HasSNetworkId
    )
import Cardano.Wallet.Primitive.Types
    ( SortOrder
    , WalletId
    )
import Cardano.Wallet.Primitive.Types.Address
    ( AddressState
    )
import Cardano.Wallet.Primitive.Types.Tx
    ( SealedTx
    )
import Data.Generics.Labels
    ()
import Data.Proxy
    ( Proxy (..)
    )
import Servant
    ( NoContent
    , (:<|>) (..)
    , (:>)
    )
import Servant.Client
    ( ClientM
    , client
    )

postWallet
    :: SomeByronWalletPostData
    -> ClientM ApiByronWallet
deleteWallet
    :: ApiT WalletId
    -> ClientM NoContent
getWallet
    :: ApiT WalletId
    -> ClientM ApiByronWallet
listWallets
    :: ClientM [ApiByronWallet]
putWalletByron
    :: ApiT WalletId
    -> ApiWalletPutData
    -> ClientM ApiByronWallet
getWalletUtxoSnapshot
    :: ApiT WalletId
    -> ClientM ApiWalletUtxoSnapshot
getWalletUtxoStatistics
    :: ApiT WalletId
    -> ClientM ApiUtxoStatistics
putWalletPassphrase
    :: ApiT WalletId
    -> ByronWalletPutPassphraseData
    -> ClientM NoContent
postWallet
    :<|> deleteWallet
    :<|> getWallet
    :<|> listWallets
    :<|> putWalletByron
    :<|> getWalletUtxoSnapshot
    :<|> getWalletUtxoStatistics
    :<|> putWalletPassphrase =
        client (Proxy @("v2" :> ByronWallets))

listTransactions
    :: forall network
     . HasSNetworkId network
    => ApiT WalletId
    -> Maybe Iso8601Time
    -> Maybe Iso8601Time
    -> Maybe (ApiT SortOrder)
    -> Maybe ApiLimit
    -> Maybe (ApiAddressIdT network)
    -> ClientM [ApiTransactionT network]
listTransactions = client (Proxy @("v2" :> ListByronTransactions network))

getTransaction
    :: forall network
     . HasSNetworkId network
    => ApiT WalletId
    -> ApiTxId
    -> ClientM (ApiTransactionT network)
getTransaction = client (Proxy @("v2" :> GetByronTransaction network))

deleteTransaction
    :: ApiT WalletId
    -> ApiTxId
    -> ClientM NoContent
deleteTransaction = client (Proxy @("v2" :> DeleteByronTransaction))

postTransaction
    :: forall network
     . HasSNetworkId network
    => ApiT WalletId
    -> PostTransactionOldDataT network
    -> ClientM (ApiTransactionT network)
postTransaction = client (Proxy @("v2" :> CreateByronTransactionOld network))

postTransactionFee
    :: forall network
     . HasSNetworkId network
    => ApiT WalletId
    -> PostTransactionFeeOldDataT network
    -> ClientM ApiFee
postTransactionFee = client (Proxy @("v2" :> PostByronTransactionFeeOld network))

postExternalTransaction :: ApiT SealedTx -> ClientM ApiTxId
postExternalTransaction =
    client (Proxy @("v2" :> Proxy_))

postRandomAddress
    :: forall network
     . HasSNetworkId network
    => ApiT WalletId
    -> ApiPostRandomAddressData
    -> ClientM (ApiAddressT network)
postRandomAddress = client (Proxy @("v2" :> PostByronAddress network))

putRandomAddress
    :: forall network
     . HasSNetworkId network
    => ApiT WalletId
    -> ApiAddressIdT network
    -> ClientM NoContent
putRandomAddress = client (Proxy @("v2" :> PutByronAddress network))

putRandomAddresses
    :: forall network
     . HasSNetworkId network
    => ApiT WalletId
    -> ApiPutAddressesDataT network
    -> ClientM NoContent
putRandomAddresses = client (Proxy @("v2" :> PutByronAddresses network))

listAddresses
    :: forall network
     . HasSNetworkId network
    => ApiT WalletId
    -> Maybe (ApiT AddressState)
    -> ClientM [ApiAddressT network]
listAddresses = client (Proxy @("v2" :> ListByronAddresses network))
