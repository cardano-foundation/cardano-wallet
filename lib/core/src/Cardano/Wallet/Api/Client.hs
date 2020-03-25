{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

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

    , StakePoolClient (..)
    , stakePoolClient

    , NetworkClient (..)
    , networkClient
    ) where

import Prelude

import Cardano.Wallet.Api
    ( Addresses
    , ByronTransactions
    , ByronWallets
    , Network
    , PostData
    , Proxy_
    , StakePools
    , Transactions
    , Wallets
    )
import Cardano.Wallet.Api.Types
    ( ApiAddressT
    , ApiByronWallet
    , ApiCoinSelectionT
    , ApiEpochNumber
    , ApiFee
    , ApiNetworkClock
    , ApiNetworkInformation (..)
    , ApiNetworkParameters
    , ApiNetworkTip
    , ApiPoolId
    , ApiSelectCoinsDataT
    , ApiStakePool
    , ApiT (..)
    , ApiTransactionT
    , ApiTxId (..)
    , ApiUtxoStatistics
    , ApiWallet (..)
    , ApiWalletPassphrase
    , Iso8601Time (..)
    , PostExternalTransactionData (..)
    , PostTransactionDataT
    , PostTransactionFeeDataT
    , WalletPutData (..)
    , WalletPutPassphraseData (..)
    )
import Cardano.Wallet.Primitive.Types
    ( AddressState, SortOrder, WalletId )
import Control.Monad
    ( void )
import Data.Proxy
    ( Proxy (..) )
import Servant
    ( (:<|>) (..), (:>), NoContent )
import Servant.Client
    ( ClientM, client )

import qualified Data.Aeson as Json

{-------------------------------------------------------------------------------
                              Server Interaction
-------------------------------------------------------------------------------}

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
    , listWallets
        :: ClientM [wallet]
    , postWallet
        :: PostData wallet
        -> ClientM wallet
    , putWallet
        :: ApiT WalletId
        -> WalletPutData
        -> ClientM wallet
    , putWalletPassphrase
        :: ApiT WalletId
        -> WalletPutPassphraseData
        -> ClientM NoContent
    , forceResyncWallet
        :: ApiT WalletId
        -> ApiNetworkTip
        -> ClientM NoContent
    }

data TransactionClient = TransactionClient
    { listTransactions
        :: ApiT WalletId
        -> Maybe Iso8601Time
        -> Maybe Iso8601Time
        -> Maybe (ApiT SortOrder)
        -> ClientM [ApiTransactionT Json.Value]
    , postTransaction
        :: ApiT WalletId
        -> PostTransactionDataT Json.Value
        -> ClientM (ApiTransactionT Json.Value)
    , postTransactionFee
        :: ApiT WalletId
        -> PostTransactionFeeDataT Json.Value
        -> ClientM ApiFee
    , postExternalTransaction
        :: PostExternalTransactionData
        -> ClientM ApiTxId
    , deleteTransaction
        :: ApiT WalletId
        -> ApiTxId
        -> ClientM NoContent
    }

newtype AddressClient = AddressClient
    { listAddresses
        :: ApiT WalletId
        -> Maybe (ApiT AddressState)
        -> ClientM [Json.Value]
    }

data StakePoolClient = StakePoolClient
    { listPools
        :: ClientM [ApiStakePool]
    , joinStakePool
        :: ApiPoolId
        -> ApiT WalletId
        -> ApiWalletPassphrase
        -> ClientM (ApiTransactionT Json.Value)
    , quitStakePool
        :: ApiT WalletId
        -> ApiWalletPassphrase
        -> ClientM (ApiTransactionT Json.Value)
    }


data NetworkClient = NetworkClient
    { networkInformation
        :: ClientM ApiNetworkInformation
    , networkParameters
        :: ApiEpochNumber
        -> ClientM ApiNetworkParameters
    , networkClock
        :: ClientM ApiNetworkClock
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
            :<|> _getWalletUtxoStatistics
            :<|> _forceResyncWallet
            = client (Proxy @("v2" :> Wallets))
    in
        WalletClient
            { deleteWallet = void . _deleteWallet
            , getWallet = _getWallet
            , listWallets = _listWallets
            , postWallet = _postWallet
            , putWallet = _putWallet
            , putWalletPassphrase = _putWalletPassphrase
            , forceResyncWallet = _forceResyncWallet
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
            :<|> _forceResyncWallet
            :<|> _putWallet
            :<|> _getWalletUtxoStatistics
            = client (Proxy @("v2" :> ByronWallets))
    in
        WalletClient
            { deleteWallet = void . _deleteWallet
            , getWallet = _getWallet
            , listWallets = _listWallets
            , postWallet = _postWallet
            , putWallet = _putWallet
            , putWalletPassphrase = error "TODO: putWalletPassphrase"
            , forceResyncWallet = _forceResyncWallet
            , getWalletUtxoStatistics = _getWalletUtxoStatistics
            }

-- | Produces a 'TransactionClient t' working against the /wallets API.
transactionClient
    :: TransactionClient
transactionClient =
    let
        _postTransaction
            :<|> _listTransactions
            :<|> _postTransactionFee
            :<|> _deleteTransaction
            = client (Proxy @("v2" :> (Transactions Json.Value)))

        _postExternalTransaction
            = client (Proxy @("v2" :> Proxy_))
    in
        TransactionClient
            { listTransactions = _listTransactions
            , postTransaction = _postTransaction
            , postTransactionFee = _postTransactionFee
            , postExternalTransaction = _postExternalTransaction
            , deleteTransaction = _deleteTransaction
            }

-- | Produces a 'TransactionClient n' working against the /byron-wallets API.
byronTransactionClient
    :: TransactionClient
byronTransactionClient =
    let
        _postTransaction
            :<|> _listTransactions
            :<|> _postTransactionFee
            :<|> _deleteTransaction
            = client (Proxy @("v2" :> (ByronTransactions Json.Value)))

        _postExternalTransaction
            = client (Proxy @("v2" :> Proxy_))

    in TransactionClient
        { listTransactions = _listTransactions
        , postTransaction = _postTransaction
        , postTransactionFee = _postTransactionFee
        , postExternalTransaction = _postExternalTransaction
        , deleteTransaction = _deleteTransaction
        }

-- | Produces an 'AddressClient n' working against the /wallets API
addressClient
    :: AddressClient
addressClient =
    let
        _listAddresses
            = client (Proxy @("v2" :> Addresses Json.Value))
    in
        AddressClient
            { listAddresses = _listAddresses
            }

-- | Produces an 'StakePoolsClient n' working against the /stake-pools API
stakePoolClient
    :: StakePoolClient
stakePoolClient =
    let
        _listPools
            :<|> _joinStakePool
            :<|> _quitStakePool
            :<|> _delegationFee
            = client (Proxy @("v2" :> StakePools Json.Value))
    in
        StakePoolClient
            { listPools = _listPools
            , joinStakePool = _joinStakePool
            , quitStakePool = _quitStakePool
            }

-- | Produces a 'NetworkClient'
networkClient
    :: NetworkClient
networkClient =
    let
        _networkInformation
            :<|> _networkParameters
            :<|> _networkClock
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

type instance ApiAddressT Json.Value = Json.Value
type instance ApiCoinSelectionT Json.Value = Json.Value
type instance ApiSelectCoinsDataT Json.Value = Json.Value
type instance ApiTransactionT Json.Value = Json.Value
type instance PostTransactionDataT Json.Value = Json.Value
type instance PostTransactionFeeDataT Json.Value = Json.Value
