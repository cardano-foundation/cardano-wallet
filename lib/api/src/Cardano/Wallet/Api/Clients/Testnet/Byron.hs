{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Wallet.Api.Clients.Testnet.Byron
    ( A
    , deleteWallet
    , deleteTransaction
    , getTransaction
    , getWallet
    , getWalletUtxoSnapshot
    , getWalletUtxoStatistics
    , inspectAddress
    , listAddresses
    , listTransactions
    , listWallets
    , postExternalTransaction
    , postRandomAddress
    , postScriptAddress
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
    ( Addresses
    , ByronAddresses
    , ByronTransactions
    , ByronWallets
    , Proxy_
    )
import Cardano.Wallet.Api.Types
    ( AnyAddress
    , ApiAddressData
    , ApiAddressIdT
    , ApiAddressInspect (..)
    , ApiAddressInspectData (..)
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
    ( NetworkDiscriminant (..)
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

type A = Testnet 42

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
    :: ApiT WalletId
    -> Maybe Iso8601Time
    -> Maybe Iso8601Time
    -> Maybe (ApiT SortOrder)
    -> Maybe ApiLimit
    -> Maybe (ApiAddressIdT A)
    -> ClientM [ApiTransactionT A]
getTransaction
    :: ApiT WalletId
    -> ApiTxId
    -> ClientM (ApiTransactionT A)
deleteTransaction
    :: ApiT WalletId
    -> ApiTxId
    -> ClientM NoContent
postTransaction
    :: ApiT WalletId
    -> PostTransactionOldDataT A
    -> ClientM (ApiTransactionT A)
postTransactionFee
    :: ApiT WalletId
    -> PostTransactionFeeOldDataT A
    -> ClientM ApiFee
listTransactions
    :<|> getTransaction
    :<|> deleteTransaction
    :<|> postTransaction
    :<|> postTransactionFee =
        client (Proxy @("v2" :> (ByronTransactions A)))

postExternalTransaction :: ApiT SealedTx -> ClientM ApiTxId
postExternalTransaction =
    client (Proxy @("v2" :> Proxy_))

inspectAddress
    :: ApiAddressInspectData
    -> ClientM ApiAddressInspect
postScriptAddress
    :: ApiAddressData
    -> ClientM AnyAddress
_ :<|> inspectAddress
    :<|> postScriptAddress =
        client (Proxy @("v2" :> Addresses A))

postRandomAddress
    :: ApiT WalletId
    -> ApiPostRandomAddressData
    -> ClientM (ApiAddressT A)
putRandomAddress
    :: ApiT WalletId
    -> ApiAddressIdT A
    -> ClientM NoContent
putRandomAddresses
    :: ApiT WalletId
    -> ApiPutAddressesDataT A
    -> ClientM NoContent
listAddresses
    :: ApiT WalletId
    -> Maybe (ApiT AddressState)
    -> ClientM [ApiAddressT A]
postRandomAddress
    :<|> putRandomAddress
    :<|> putRandomAddresses
    :<|> listAddresses =
        client (Proxy @("v2" :> ByronAddresses A))
