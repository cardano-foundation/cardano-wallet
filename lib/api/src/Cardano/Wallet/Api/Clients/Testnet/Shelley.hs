{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Api.Clients.Testnet.Shelley
where

import Prelude

import Cardano.Wallet.Api.Clients.Testnet.Id
    ( Testnet42
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
    )
import Servant.Client
    ( ClientM
    )

import qualified Cardano.Wallet.Api.Clients.Shelley as Shelley

deleteWallet
    :: ApiT WalletId -> ClientM NoContent
deleteWallet = Shelley.deleteWallet

getWallet
    :: ApiT WalletId -> ClientM ApiWallet
getWallet = Shelley.getWallet

listWallets
    :: ClientM [ApiWallet]
listWallets = Shelley.listWallets

postWallet
    :: WalletOrAccountPostData -> ClientM ApiWallet
postWallet = Shelley.postWallet

putWallet
    :: ApiT WalletId -> ApiWalletPutDataExtended -> ClientM ApiWallet
putWallet = Shelley.putWallet

putWalletPassphrase
    :: ApiT WalletId
    -> WalletPutPassphraseData
    -> ClientM NoContent
putWalletPassphrase = Shelley.putWalletPassphrase

getWalletUtxoSnapshot
    :: ApiT WalletId -> ClientM ApiWalletUtxoSnapshot
getWalletUtxoSnapshot = Shelley.getWalletUtxoSnapshot

getWalletUtxoStatistics
    :: ApiT WalletId -> ClientM ApiUtxoStatistics
getWalletUtxoStatistics = Shelley.getWalletUtxoStatistics

constructTransaction
    :: ApiT WalletId
    -> ApiConstructTransactionData Testnet42
    -> ClientM (ApiConstructTransaction Testnet42)
constructTransaction = Shelley.constructTransaction

signTransaction
    :: ApiT WalletId
    -> ApiSignTransactionPostData
    -> ClientM ApiSerialisedTransaction
signTransaction = Shelley.signTransaction (Proxy @Testnet42)

listTransactions
    :: ApiT WalletId
    -> Maybe MinWithdrawal
    -> Maybe Iso8601Time
    -> Maybe Iso8601Time
    -> Maybe (ApiT SortOrder)
    -> Maybe ApiLimit
    -> Maybe (ApiAddress Testnet42)
    -> Bool
    -> ClientM [ApiTransaction Testnet42]
listTransactions = Shelley.listTransactions

getTransaction
    :: ApiT WalletId
    -> ApiTxId
    -> Bool
    -> ClientM (ApiTransaction Testnet42)
getTransaction = Shelley.getTransaction

deleteTransaction
    :: ApiT WalletId
    -> ApiTxId
    -> ClientM NoContent
deleteTransaction = Shelley.deleteTransaction

postTransaction
    :: ApiT WalletId
    -> PostTransactionOldData Testnet42
    -> ClientM (ApiTransaction Testnet42)
postTransaction = Shelley.postTransaction

postTransactionFee
    :: ApiT WalletId
    -> PostTransactionFeeOldData Testnet42
    -> ClientM ApiFee
postTransactionFee = Shelley.postTransactionFee

balanceTransaction
    :: ApiT WalletId
    -> ApiBalanceTransactionPostData Testnet42
    -> ClientM ApiSerialisedTransaction
balanceTransaction = Shelley.balanceTransaction

decodeTransaction
    :: ApiT WalletId
    -> ApiDecodeTransactionPostData
    -> ClientM (ApiDecodedTransaction Testnet42)
decodeTransaction = Shelley.decodeTransaction

submitTransaction
    :: ApiT WalletId
    -> ApiSerialisedTransaction
    -> ClientM ApiTxId
submitTransaction = Shelley.submitTransaction

postExternalTransaction
    :: ApiT SealedTx -> ClientM ApiTxId
postExternalTransaction = Shelley.postExternalTransaction

listAddresses
    :: ApiT WalletId
    -> Maybe (ApiT AddressState)
    -> ClientM [ApiAddressWithPath Testnet42]
listAddresses = Shelley.listAddresses

inspectAddress
    :: ApiAddressInspectData -> ClientM ApiAddressInspect
inspectAddress = Shelley.inspectAddress

postScriptAddress
    :: ApiAddressData -> ClientM AnyAddress
postScriptAddress = Shelley.postScriptAddress (Proxy @Testnet42)

listPools
    :: Maybe (ApiT Coin) -> ClientM [ApiT StakePool]
listPools = Shelley.listPools

joinStakePool
    :: ApiPoolSpecifier
    -> ApiT WalletId
    -> ApiWalletPassphrase
    -> ClientM (ApiTransaction Testnet42)
joinStakePool = Shelley.joinStakePool

quitStakePool
    :: ApiT WalletId
    -> ApiWalletPassphrase
    -> ClientM (ApiTransaction Testnet42)
quitStakePool = Shelley.quitStakePool

getAssets
    :: ApiT WalletId -> ClientM [ApiAsset]
getAssets = Shelley.getAssets

getAsset
    :: ApiT WalletId -> ApiT TokenPolicyId -> ApiT AssetName -> ClientM ApiAsset
getAsset = Shelley.getAsset

getAsset'
    :: ApiT WalletId -> ApiT TokenPolicyId -> ClientM ApiAsset
getAsset' = Shelley.getAsset'

planMigration
    :: ApiT WalletId
    -> ApiWalletMigrationPlanPostData Testnet42
    -> ClientM (ApiWalletMigrationPlan Testnet42)
planMigration = Shelley.planMigration

migrate
    :: ApiT WalletId
    -> ApiWalletMigrationPostData Testnet42 "user"
    -> ClientM (NonEmpty (ApiTransaction Testnet42))
migrate = Shelley.migrate
