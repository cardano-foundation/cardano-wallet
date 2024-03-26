{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Api.Clients.Testnet.Byron
    ( Testnet42
    , deleteWallet
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

import Cardano.Wallet.Api.Clients.Testnet.Id
    ( Testnet42
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
import Servant
    ( NoContent
    )
import Servant.Client
    ( ClientM
    )

import qualified Cardano.Wallet.Api.Clients.Byron as Byron

postWallet
    :: SomeByronWalletPostData
    -> ClientM ApiByronWallet
postWallet = Byron.postWallet

deleteWallet
    :: ApiT WalletId
    -> ClientM NoContent
deleteWallet = Byron.deleteWallet

getWallet
    :: ApiT WalletId
    -> ClientM ApiByronWallet
getWallet = Byron.getWallet

listWallets :: ClientM [ApiByronWallet]
listWallets = Byron.listWallets

putWalletByron
    :: ApiT WalletId
    -> ApiWalletPutData
    -> ClientM ApiByronWallet
putWalletByron = Byron.putWalletByron

getWalletUtxoSnapshot
    :: ApiT WalletId
    -> ClientM ApiWalletUtxoSnapshot
getWalletUtxoSnapshot = Byron.getWalletUtxoSnapshot

getWalletUtxoStatistics
    :: ApiT WalletId
    -> ClientM ApiUtxoStatistics
getWalletUtxoStatistics = Byron.getWalletUtxoStatistics

putWalletPassphrase
    :: ApiT WalletId
    -> ByronWalletPutPassphraseData
    -> ClientM NoContent
putWalletPassphrase = Byron.putWalletPassphrase

listTransactions
    :: ApiT WalletId
    -> Maybe Iso8601Time
    -> Maybe Iso8601Time
    -> Maybe (ApiT SortOrder)
    -> Maybe ApiLimit
    -> Maybe (ApiAddressIdT Testnet42)
    -> ClientM [ApiTransactionT Testnet42]
listTransactions = Byron.listTransactions

getTransaction
    :: ApiT WalletId
    -> ApiTxId
    -> ClientM (ApiTransactionT Testnet42)
getTransaction = Byron.getTransaction

deleteTransaction
    :: ApiT WalletId
    -> ApiTxId
    -> ClientM NoContent
deleteTransaction = Byron.deleteTransaction

postTransaction
    :: ApiT WalletId
    -> PostTransactionOldDataT Testnet42
    -> ClientM (ApiTransactionT Testnet42)
postTransaction = Byron.postTransaction

postTransactionFee
    :: ApiT WalletId
    -> PostTransactionFeeOldDataT Testnet42
    -> ClientM ApiFee
postTransactionFee = Byron.postTransactionFee

postExternalTransaction :: ApiT SealedTx -> ClientM ApiTxId
postExternalTransaction = Byron.postExternalTransaction

postRandomAddress
    :: ApiT WalletId
    -> ApiPostRandomAddressData
    -> ClientM (ApiAddressT Testnet42)
postRandomAddress = Byron.postRandomAddress

putRandomAddress
    :: ApiT WalletId
    -> ApiAddressIdT Testnet42
    -> ClientM NoContent
putRandomAddress = Byron.putRandomAddress

putRandomAddresses
    :: ApiT WalletId
    -> ApiPutAddressesDataT Testnet42
    -> ClientM NoContent
putRandomAddresses = Byron.putRandomAddresses

listAddresses
    :: ApiT WalletId
    -> Maybe (ApiT AddressState)
    -> ClientM [ApiAddressT Testnet42]
listAddresses = Byron.listAddresses
