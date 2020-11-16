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
    , StakePools
    , Transactions
    , Wallets
    )
import Cardano.Wallet.Api.Types
    ( AccountPutPassphraseData (..)
    , ApiAccount (..)
    , ApiAccountPutData (..)
    , ApiAddressIdT
    , ApiAddressInspect (..)
    , ApiAddressInspectData (..)
    , ApiAddressT
    , ApiByronAccount
    , ApiByronAccountPutPassphraseData (..)
    , ApiCoinSelectionT
    , ApiEncryptionPassphrase
    , ApiFee
    , ApiNetworkClock
    , ApiNetworkInformation (..)
    , ApiNetworkParameters
    , ApiPoolId
    , ApiPostRandomAddressData
    , ApiPutAddressesDataT
    , ApiSelectCoinsDataT
    , ApiT (..)
    , ApiTransactionT
    , ApiTxId (..)
    , ApiUtxoStatistics
    , Iso8601Time (..)
    , PostExternalTransactionData (..)
    , PostTransactionDataT
    , PostTransactionFeeDataT
    )
import Cardano.Wallet.Primitive.Types
    ( AccountId, AddressState, Coin (..), SortOrder )
import Control.Monad
    ( void )
import Data.Coerce
    ( coerce )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Generics.Labels
    ()
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Servant
    ( (:<|>) (..), (:>), NoContent )
import Servant.Client
    ( ClientM, client )

import qualified Data.Aeson as Aeson

{-------------------------------------------------------------------------------
                              Server Interaction
-------------------------------------------------------------------------------}

-- | This data type encapsulates the client functions for all endpoints of the
-- cardano-wallet V2 API.
data WalletClient wallet = WalletClient
    { deleteWallet
        :: ApiT AccountId
        -> ClientM ()
    , getWallet
        :: ApiT AccountId
        -> ClientM wallet
    , getWalletUtxoStatistics
        :: ApiT AccountId
        -> ClientM ApiUtxoStatistics
    , listWallets
        :: ClientM [wallet]
    , postWallet
        :: PostData wallet
        -> ClientM wallet
    , putWallet
        :: ApiT AccountId
        -> ApiAccountPutData
        -> ClientM wallet
    , putWalletPassphrase
        :: ApiT AccountId
        -> AccountPutPassphraseData
        -> ClientM NoContent
    }

data TransactionClient = TransactionClient
    { listTransactions
        :: ApiT AccountId
        -> Maybe Iso8601Time
        -> Maybe Iso8601Time
        -> Maybe (ApiT SortOrder)
        -> ClientM [ApiTransactionT Aeson.Value]
    , postTransaction
        :: ApiT AccountId
        -> PostTransactionDataT Aeson.Value
        -> ClientM (ApiTransactionT Aeson.Value)
    , postTransactionFee
        :: ApiT AccountId
        -> PostTransactionFeeDataT Aeson.Value
        -> ClientM ApiFee
    , postExternalTransaction
        :: PostExternalTransactionData
        -> ClientM ApiTxId
    , deleteTransaction
        :: ApiT AccountId
        -> ApiTxId
        -> ClientM NoContent
    , getTransaction
        :: ApiT AccountId
        -> ApiTxId
        -> ClientM (ApiTransactionT Aeson.Value)
    }

data AddressClient = AddressClient
    { listAddresses
        :: ApiT AccountId
        -> Maybe (ApiT AddressState)
        -> ClientM [Aeson.Value]
    , inspectAddress
        :: Text
        -> ClientM Aeson.Value
    , postRandomAddress
        :: ApiT AccountId
        -> ApiPostRandomAddressData
        -> ClientM (ApiAddressT Aeson.Value)
    , putRandomAddress
        :: ApiT AccountId
        -> ApiAddressIdT Aeson.Value
        -> ClientM NoContent
    , putRandomAddresses
        :: ApiT AccountId
        -> ApiPutAddressesDataT Aeson.Value
        -> ClientM NoContent
    }

data StakePoolClient apiPool = StakePoolClient
    { listPools
        :: Maybe (ApiT Coin) -> ClientM [apiPool]
    , joinStakePool
        :: ApiPoolId
        -> ApiT AccountId
        -> ApiEncryptionPassphrase
        -> ClientM (ApiTransactionT Aeson.Value)
    , quitStakePool
        :: ApiT AccountId
        -> ApiEncryptionPassphrase
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
walletClient :: WalletClient ApiAccount
walletClient =
    let
        _deleteWallet
            :<|> _getWallet
            :<|> _listWallets
            :<|> _postWallet
            :<|> _putWallet
            :<|> _putWalletPassphrase
            :<|> _getWalletUtxoStatistics
            = client (Proxy @("v2" :> Wallets))
    in
        WalletClient
            { deleteWallet = void . _deleteWallet
            , getWallet = _getWallet
            , listWallets = _listWallets
            , postWallet = _postWallet
            , putWallet = _putWallet
            , putWalletPassphrase = _putWalletPassphrase
            , getWalletUtxoStatistics = _getWalletUtxoStatistics
            }

-- | Produces a 'WalletClient' working against the /wallets API.
byronWalletClient :: WalletClient ApiByronAccount
byronWalletClient =
    let
        _postWallet
            :<|> _deleteWallet
            :<|> _getWallet
            :<|> _listWallets
            :<|> _putWallet
            :<|> _getWalletUtxoStatistics
            :<|> _putWalletPassphrase
            = client (Proxy @("v2" :> ByronWallets))
    in
        WalletClient
            { deleteWallet = void . _deleteWallet
            , getWallet = _getWallet
            , listWallets = _listWallets
            , postWallet = _postWallet
            , putWallet = _putWallet
            , putWalletPassphrase = \wid body ->
                _putWalletPassphrase wid $ ApiByronAccountPutPassphraseData
                    { oldPassphrase = Just $ coerce <$> body ^. #oldPassphrase
                    , newPassphrase = body ^. #newPassphrase
                    }
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
            :<|> _getTransaction
            = client (Proxy @("v2" :> (Transactions Aeson.Value)))

        _postExternalTransaction
            = client (Proxy @("v2" :> Proxy_))
    in
        TransactionClient
            { listTransactions = (`_listTransactions` Nothing)
            , postTransaction = _postTransaction
            , postTransactionFee = _postTransactionFee
            , postExternalTransaction = _postExternalTransaction
            , deleteTransaction = _deleteTransaction
            , getTransaction = _getTransaction
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
            :<|> _getTransaction
            = client (Proxy @("v2" :> (ByronTransactions Aeson.Value)))

        _postExternalTransaction
            = client (Proxy @("v2" :> Proxy_))

    in TransactionClient
        { listTransactions = _listTransactions
        , postTransaction = _postTransaction
        , postTransactionFee = _postTransactionFee
        , postExternalTransaction = _postExternalTransaction
        , deleteTransaction = _deleteTransaction
        , getTransaction = _getTransaction
        }

-- | Produces an 'AddressClient n' working against the /wallets API
addressClient
    :: AddressClient
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
            , postRandomAddress = \_ _ -> fail "feature unavailable."
            , putRandomAddress  = \_ _ -> fail "feature unavailable."
            , putRandomAddresses = \_ _ -> fail "feature unavailable."
            }

-- | Produces an 'AddressClient n' working against the /wallets API
byronAddressClient
    :: AddressClient
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
stakePoolClient
    :: forall apiPool. (Aeson.FromJSON apiPool) => StakePoolClient apiPool
stakePoolClient =
    let
        _listPools
            :<|> _joinStakePool
            :<|> _quitStakePool
            :<|> _delegationFee
            :<|> _postPoolMaintenance
            :<|> _getPoolMaintenance
            = client (Proxy @("v2" :> StakePools Aeson.Value apiPool))
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

type instance ApiAddressT Aeson.Value = Aeson.Value
type instance ApiAddressIdT Aeson.Value = Text
type instance ApiCoinSelectionT Aeson.Value = Aeson.Value
type instance ApiSelectCoinsDataT Aeson.Value = Aeson.Value
type instance ApiTransactionT Aeson.Value = Aeson.Value
type instance PostTransactionDataT Aeson.Value = Aeson.Value
type instance PostTransactionFeeDataT Aeson.Value = Aeson.Value
type instance ApiPutAddressesDataT Aeson.Value = Aeson.Value
