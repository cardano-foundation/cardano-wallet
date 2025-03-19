{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- This module provides a half-typed Servant client for the cardano-wallet V2 API.
--
-- The functions in this module can be run with "Servant.Client.runClientM".

module Cardano.Wallet.Api.Client
    ( -- * API Clients
      WalletClient (..)
    , walletClient
    , byronWalletClient

    , TransactionClient (..)
    , transactionClient
    , byronTransactionClient

    , AddressClient (..)
    , addressClient
    , byronAddressClient

    , StakePoolClient (..)
    , stakePoolClient

    , NetworkClient (..)
    , networkClient
    ) where

import Prelude

import Cardano.Wallet.Api
    ( Addresses
    , ByronAddresses
    , ByronTransactions
    , ByronWallets
    , Network
    , PostData
    , Proxy_
    , ShelleyTransactions
    , StakePools
    , Wallets
    )
import Cardano.Wallet.Api.Types
    ( ApiAddressIdT
    , ApiAddressInspect (..)
    , ApiAddressInspectData (..)
    , ApiAddressT
    , ApiBalanceTransactionPostDataT
    , ApiByronWallet
    , ApiBytesT (..)
    , ApiCoinSelectionT
    , ApiConstructTransactionDataT
    , ApiConstructTransactionT
    , ApiDecodeTransactionPostData
    , ApiDecodedTransactionT
    , ApiFee
    , ApiNetworkClock
    , ApiNetworkInformation (..)
    , ApiNetworkParameters
    , ApiPoolSpecifier
    , ApiPostRandomAddressData
    , ApiPutAddressesDataT
    , ApiSelectCoinsDataT
    , ApiSerialisedTransaction (..)
    , ApiSignTransactionPostData
    , ApiStakeKeysT
    , ApiT (..)
    , ApiTransactionT
    , ApiTxId (..)
    , ApiUtxoStatistics
    , ApiWallet (..)
    , ApiWalletPassphrase
    , ApiWalletPutData (..)
    , ApiWalletPutDataExtended (..)
    , ApiWalletUtxoSnapshot (..)
    , Base (Base64)
    , ByronWalletPutPassphraseData (..)
    , Iso8601Time (..)
    , PostTransactionFeeOldDataT
    , PostTransactionOldDataT
    , WalletPutPassphraseData (..)
    )
import Cardano.Wallet.Api.Types.SchemaMetadata
    ( TxMetadataSchema
    , toSimpleMetadataFlag
    )
import Cardano.Wallet.Api.Types.Transaction
    ( ApiLimit
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
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..)
    )
import Cardano.Wallet.Primitive.Types.Tx
    ( SealedTx
    , SerialisedTx (..)
    , unsafeSealedTxFromBytes
    )
import Control.Monad
    ( void
    )
import Data.Generics.Internal.VL.Lens
    ( view
    )
import Data.Generics.Labels
    ()
import Data.Proxy
    ( Proxy (..)
    )
import Data.Text
    ( Text
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
import UnliftIO.Exception
    ( throwString
    )

import qualified Data.Aeson as Aeson

{-------------------------------------------------------------------------------
                              Server Interaction
-------------------------------------------------------------------------------}
type family  WalletPutPassphraseFormat wallet where
    WalletPutPassphraseFormat ApiWallet = WalletPutPassphraseData
    WalletPutPassphraseFormat ApiByronWallet = ByronWalletPutPassphraseData

-- | This data type encapsulates the client functions for all endpoints of the
-- cardano-wallet V2 API.
data WalletClient wallet = WalletClient
    { deleteWallet
        :: ApiT WalletId
        -> ClientM ()
    , getWallet
        :: ApiT WalletId
        -> ClientM wallet
    , getWalletUtxoStatistics
        :: ApiT WalletId
        -> ClientM ApiUtxoStatistics
    , getWalletUtxoSnapshot
        :: ApiT WalletId
        -> ClientM ApiWalletUtxoSnapshot
    , listWallets
        :: ClientM [wallet]
    , postWallet
        :: PostData wallet
        -> ClientM wallet
    , putWallet
        :: ApiT WalletId
        -> ApiWalletPutDataExtended
        -> ClientM wallet
    , putWalletByron
        :: ApiT WalletId
        -> ApiWalletPutData
        -> ClientM wallet
    , putWalletPassphrase
        :: ApiT WalletId
        -> WalletPutPassphraseFormat wallet
        -> ClientM NoContent
    }

data TransactionClient = TransactionClient
    { listTransactions
        :: ApiT WalletId
        -> Maybe Iso8601Time
        -> Maybe Iso8601Time
        -> Maybe (ApiT SortOrder)
        -> Maybe ApiLimit
        -> Maybe (ApiAddressIdT Aeson.Value)
        -> Bool
        -> ClientM [ApiTransactionT Aeson.Value]
    , signTransaction
        :: ApiT WalletId
        -> ApiSignTransactionPostData
        -> ClientM ApiSerialisedTransaction
    , postTransaction
        :: ApiT WalletId
        -> PostTransactionOldDataT Aeson.Value
        -> ClientM (ApiTransactionT Aeson.Value)
    , postTransactionFee
        :: ApiT WalletId
        -> PostTransactionFeeOldDataT Aeson.Value
        -> ClientM ApiFee
    , postExternalTransaction
        :: ApiBytesT 'Base64 SerialisedTx
        -> ClientM ApiTxId
    , deleteTransaction
        :: ApiT WalletId
        -> ApiTxId
        -> ClientM NoContent
    , getTransaction
        :: ApiT WalletId
        -> ApiTxId
        -> TxMetadataSchema
        -> ClientM (ApiTransactionT Aeson.Value)
    , constructTransaction
        :: ApiT WalletId
        -> ApiConstructTransactionDataT Aeson.Value
        -> ClientM (ApiConstructTransactionT Aeson.Value)
    , balanceTransaction
        :: ApiT WalletId
        -> ApiBalanceTransactionPostDataT Aeson.Value
        -> ClientM ApiSerialisedTransaction
    , decodeTransaction
        :: ApiT WalletId
        -> ApiDecodeTransactionPostData
        -> ClientM (ApiDecodedTransactionT Aeson.Value)
    , submitTransaction
        :: ApiT WalletId
        -> ApiSerialisedTransaction
        -> ClientM ApiTxId
    }

data AddressClient = AddressClient
    { listAddresses
        :: ApiT WalletId
        -> Maybe (ApiT AddressState)
        -> ClientM [Aeson.Value]
    , inspectAddress
        :: Text
        -> ClientM Aeson.Value
    , postRandomAddress
        :: ApiT WalletId
        -> ApiPostRandomAddressData
        -> ClientM (ApiAddressT Aeson.Value)
    , putRandomAddress
        :: ApiT WalletId
        -> ApiAddressIdT Aeson.Value
        -> ClientM NoContent
    , putRandomAddresses
        :: ApiT WalletId
        -> ApiPutAddressesDataT Aeson.Value
        -> ClientM NoContent
    }

data StakePoolClient = StakePoolClient
    { listPools
        :: Maybe (ApiT Coin) -> ClientM [ApiT StakePool]
    , joinStakePool
        :: ApiPoolSpecifier
        -> ApiT WalletId
        -> ApiWalletPassphrase
        -> ClientM (ApiTransactionT Aeson.Value)
    , quitStakePool
        :: ApiT WalletId
        -> ApiWalletPassphrase
        -> ClientM (ApiTransactionT Aeson.Value)
    }

data NetworkClient = NetworkClient
    { networkInformation
        :: ClientM ApiNetworkInformation
    , networkParameters
        :: ClientM ApiNetworkParameters
    , networkClock
        :: Bool -- When 'True', block and force NTP check
        -> ClientM ApiNetworkClock
    }

-- | Produces a 'WalletClient' working against the /wallets API.
walletClient :: WalletClient ApiWallet
walletClient =
    let
        _deleteWallet
            :<|> _getWallet
            :<|> _listWallets
            :<|> _postWallet
            :<|> _putWallet
            :<|> _putWalletPassphrase
            :<|> _getWalletUtxoSnapshot
            :<|> _getWalletUtxoStatistics
            = client (Proxy @("v2" :> Wallets))
    in
        WalletClient
            { deleteWallet = void . _deleteWallet
            , getWallet = _getWallet
            , listWallets = _listWallets
            , postWallet = _postWallet
            , putWallet = _putWallet
            , putWalletByron = error "putWalletByron not available for shelley"
            , putWalletPassphrase = _putWalletPassphrase
            , getWalletUtxoSnapshot = _getWalletUtxoSnapshot
            , getWalletUtxoStatistics = _getWalletUtxoStatistics
            }

-- | Produces a 'WalletClient' working against the /wallets API.
byronWalletClient :: WalletClient ApiByronWallet
byronWalletClient =
    let
        _postWallet
            :<|> _deleteWallet
            :<|> _getWallet
            :<|> _listWallets
            :<|> _putWalletByron
            :<|> _getWalletUtxoSnapshot
            :<|> _getWalletUtxoStatistics
            :<|> _putWalletPassphrase
            = client (Proxy @("v2" :> ByronWallets))
    in
        WalletClient
            { deleteWallet = void . _deleteWallet
            , getWallet = _getWallet
            , listWallets = _listWallets
            , postWallet = _postWallet
            , putWallet = error "putWallet not available for byron"
            , putWalletByron = _putWalletByron
            , putWalletPassphrase = _putWalletPassphrase
            , getWalletUtxoSnapshot = _getWalletUtxoSnapshot
            , getWalletUtxoStatistics = _getWalletUtxoStatistics
            }

-- | Produces a 'TransactionClient t' working against the /wallets API.
transactionClient :: TransactionClient
transactionClient =
    let
        _constructTransaction
            :<|> _signTransaction
            :<|> _listTransactions
            :<|> _getTransaction
            :<|> _deleteTransaction
            :<|> _postTransaction
            :<|> _postTransactionFee
            :<|> _balanceTransaction
            :<|> _decodeTransaction
            :<|> _submitTransaction
            = client (Proxy @("v2" :> (ShelleyTransactions Aeson.Value)))

        _postExternalTransaction
            = client (Proxy @("v2" :> Proxy_))
    in
        TransactionClient
            { listTransactions = (`_listTransactions` Nothing)
            , signTransaction = _signTransaction
            , postTransaction = _postTransaction
            , postTransactionFee = _postTransactionFee
            , postExternalTransaction = _postExternalTransaction . fromSerialisedTx
            , deleteTransaction = _deleteTransaction
            , getTransaction =
                \wid txid metadataSchema ->
                    _getTransaction wid txid (toSimpleMetadataFlag metadataSchema)
            , constructTransaction = _constructTransaction
            , balanceTransaction = _balanceTransaction
            , decodeTransaction = _decodeTransaction
            , submitTransaction = _submitTransaction
            }

fromSerialisedTx :: ApiBytesT base SerialisedTx -> ApiT SealedTx
fromSerialisedTx = ApiT . unsafeSealedTxFromBytes . view (#getApiBytesT . #payload)

-- | Produces a 'TransactionClient n' working against the /byron-wallets API.
byronTransactionClient :: TransactionClient
byronTransactionClient =
    let
        _listTransactions
            :<|> _getTransaction
            :<|> _deleteTransaction
            :<|> _postTransaction
            :<|> _postTransactionFee
            = client (Proxy @("v2" :> (ByronTransactions Aeson.Value)))

        _postExternalTransaction
            = client (Proxy @("v2" :> Proxy_))

    in TransactionClient
        { listTransactions = \wid start end order limit addr _ ->
            _listTransactions wid start end order limit addr
        , postTransaction = _postTransaction
        , postTransactionFee = _postTransactionFee
        , postExternalTransaction = _postExternalTransaction . fromSerialisedTx
        , deleteTransaction = _deleteTransaction
        , getTransaction = \wid txid _  -> _getTransaction wid txid
        , balanceTransaction = error "balance transaction endpoint not supported for byron"
        , decodeTransaction = error "decode transaction endpoint not supported for byron"
        , submitTransaction = error "submit transaction endpoint not supported for byron"
        , signTransaction = error "sign transaction endpoint not supported for byron"
        , constructTransaction = error "construct transaction endpoint not supported for byron"
        }

-- | Produces an 'AddressClient n' working against the /wallets API
addressClient :: AddressClient
addressClient =
    let
        _listAddresses
            :<|> _inspectAddress
            :<|> _postScriptAddress
            = client (Proxy @("v2" :> Addresses Aeson.Value))
    in
        AddressClient
            { listAddresses = _listAddresses
            , inspectAddress =
                fmap unApiAddressInspect
                . _inspectAddress
                . ApiAddressInspectData
            , postRandomAddress = \_ _ -> throwString "feature unavailable."
            , putRandomAddress  = \_ _ -> throwString "feature unavailable."
            , putRandomAddresses = \_ _ -> throwString "feature unavailable."
            }

-- | Produces an 'AddressClient n' working against the /wallets API
byronAddressClient :: AddressClient
byronAddressClient =
    let
        _ :<|> _inspectAddress
          :<|> _postScriptAddress
            = client (Proxy @("v2" :> Addresses Aeson.Value))

        _postRandomAddress
            :<|> _putRandomAddress
            :<|> _putRandomAddresses
            :<|> _listAddresses
            = client (Proxy @("v2" :> ByronAddresses Aeson.Value))
    in
        AddressClient
            { listAddresses = _listAddresses
            , inspectAddress =
                fmap unApiAddressInspect
                . _inspectAddress
                . ApiAddressInspectData
            , postRandomAddress = _postRandomAddress
            , putRandomAddress = _putRandomAddress
            , putRandomAddresses = _putRandomAddresses
            }

-- | Produces an 'StakePoolsClient n' working against the /stake-pools API
stakePoolClient :: StakePoolClient
stakePoolClient =
    let listPools
            :<|> joinStakePool
            :<|> quitStakePool
            :<|> _
            :<|> _
            :<|> _
            = client (Proxy @("v2" :> StakePools Aeson.Value))
    in
        StakePoolClient
            { listPools
            , joinStakePool
            , quitStakePool
            }

-- | Produces a 'NetworkClient'
networkClient :: NetworkClient
networkClient =
    let
        _networkInformation :<|> _networkParameters :<|> _networkClock
            = client (Proxy @("v2" :> Network))
    in
        NetworkClient
            { networkInformation = _networkInformation
            , networkParameters = _networkParameters
            , networkClock = _networkClock
            }

--
-- Type families
--

type instance ApiAddressT Aeson.Value = Aeson.Value
type instance ApiStakeKeysT Aeson.Value = Aeson.Value
type instance ApiAddressIdT Aeson.Value = Text
type instance ApiCoinSelectionT Aeson.Value = Aeson.Value
type instance ApiSelectCoinsDataT Aeson.Value = Aeson.Value
type instance ApiTransactionT Aeson.Value = Aeson.Value
type instance ApiConstructTransactionT Aeson.Value = Aeson.Value
type instance ApiConstructTransactionDataT Aeson.Value = Aeson.Value
type instance PostTransactionOldDataT Aeson.Value = Aeson.Value
type instance PostTransactionFeeOldDataT Aeson.Value = Aeson.Value
type instance ApiPutAddressesDataT Aeson.Value = Aeson.Value
type instance ApiBalanceTransactionPostDataT Aeson.Value = Aeson.Value
type instance ApiDecodedTransactionT Aeson.Value = Aeson.Value
