{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Wallet.Api.Clients.Testnet.Shelley
    ( A
    , deleteWallet
    , getWallet
    , listWallets
    , postWallet
    , putWallet
    , putWalletPassphrase
    , getWalletUtxoSnapshot
    , getWalletUtxoStatistics
    , constructTransaction
    , signTransaction
    , listTransactions
    , getTransaction
    , deleteTransaction
    , postTransaction
    , postTransactionFee
    , balanceTransaction
    , decodeTransaction
    , submitTransaction
    , postExternalTransaction
    , listAddresses
    , inspectAddress
    , postScriptAddress
    , listPools
    , joinStakePool
    , quitStakePool
    , networkInformation
    , networkParameters
    , networkClock
    , getAssets
    , getAsset
    , getAsset'
    )
where

import Prelude

import Cardano.Wallet.Api
    ( Addresses
    , Assets
    , Network
    , Proxy_
    , ShelleyTransactions
    , StakePools
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
    ( NetworkDiscriminant (Testnet)
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

type A = Testnet 42

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
    :: ApiT WalletId
    -> ApiConstructTransactionData A
    -> ClientM (ApiConstructTransaction A)
signTransaction
    :: ApiT WalletId
    -> ApiSignTransactionPostData
    -> ClientM ApiSerialisedTransaction
listTransactions
    :: ApiT WalletId
    -> Maybe MinWithdrawal
    -> Maybe Iso8601Time
    -> Maybe Iso8601Time
    -> Maybe (ApiT SortOrder)
    -> Maybe ApiLimit
    -> Maybe (ApiAddress A)
    -> Bool
    -> ClientM [ApiTransaction A]
getTransaction
    :: ApiT WalletId
    -> ApiTxId
    -> Bool
    -> ClientM (ApiTransaction A)
deleteTransaction
    :: ApiT WalletId
    -> ApiTxId
    -> ClientM NoContent
postTransaction
    :: ApiT WalletId
    -> PostTransactionOldData A
    -> ClientM (ApiTransaction A)
postTransactionFee
    :: ApiT WalletId
    -> PostTransactionFeeOldData A
    -> ClientM ApiFee
balanceTransaction
    :: ApiT WalletId
    -> ApiBalanceTransactionPostData A
    -> ClientM ApiSerialisedTransaction
decodeTransaction
    :: ApiT WalletId
    -> ApiDecodeTransactionPostData
    -> ClientM (ApiDecodedTransaction A)
submitTransaction
    :: ApiT WalletId
    -> ApiSerialisedTransaction
    -> ClientM ApiTxId
constructTransaction
    :<|> signTransaction
    :<|> listTransactions
    :<|> getTransaction
    :<|> deleteTransaction
    :<|> postTransaction
    :<|> postTransactionFee
    :<|> balanceTransaction
    :<|> decodeTransaction
    :<|> submitTransaction =
        client (Proxy @("v2" :> (ShelleyTransactions A)))

postExternalTransaction
    :: ApiT SealedTx -> ClientM ApiTxId
postExternalTransaction =
    client (Proxy @("v2" :> Proxy_))

listAddresses
    :: ApiT WalletId
    -> Maybe (ApiT AddressState)
    -> ClientM [ApiAddressWithPath A]
inspectAddress
    :: ApiAddressInspectData -> ClientM ApiAddressInspect
postScriptAddress
    :: ApiAddressData -> ClientM AnyAddress
listAddresses
    :<|> inspectAddress
    :<|> postScriptAddress =
        client (Proxy @("v2" :> Addresses A))

listPools
    :: Maybe (ApiT Coin) -> ClientM [ApiT StakePool]
joinStakePool
    :: ApiPoolSpecifier
    -> ApiT WalletId
    -> ApiWalletPassphrase
    -> ClientM (ApiTransaction A)
quitStakePool
    :: ApiT WalletId
    -> ApiWalletPassphrase
    -> ClientM (ApiTransaction A)
listPools
    :<|> joinStakePool
    :<|> quitStakePool
    :<|> _
    :<|> _
    :<|> _ =
        client (Proxy @("v2" :> StakePools A))
networkInformation
    :: ClientM ApiNetworkInformation
networkParameters
    :: ClientM ApiNetworkParameters
networkClock
    :: Bool -> ClientM ApiNetworkClock
networkInformation :<|> networkParameters :<|> networkClock =
    client (Proxy @("v2" :> Network))

getAssets :: ApiT WalletId -> ClientM [ApiAsset]
getAsset :: ApiT WalletId -> ApiT TokenPolicyId -> ApiT AssetName -> ClientM ApiAsset
getAsset' :: ApiT WalletId -> ApiT TokenPolicyId -> ClientM ApiAsset
getAssets  :<|> getAsset :<|> getAsset'
    = client (Proxy @("v2" :> Assets))
