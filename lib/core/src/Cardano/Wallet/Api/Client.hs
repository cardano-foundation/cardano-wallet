{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
--
-- This module provides a Servant client for the cardano-wallet V2 API.
--
-- The functions in this module can be run with "Servant.Client.runClientM".

module Cardano.Wallet.Api.Client
    ( -- * API endpoints
      WalletClient (..)
    , walletClient
      -- * Helper functions
    , waitForSync
    , waitForRestore
    ) where

import Prelude

import Cardano.Wallet.Api
    ( Api )
import Cardano.Wallet.Api.Types
    ( ApiAddress
    , ApiFee
    , ApiNetworkInformation (..)
    , ApiStakePool
    , ApiT (..)
    , ApiTransaction
    , ApiTxId (..)
    , ApiUtxoStatistics
    , ApiWallet (..)
    , ApiWalletPassphrase
    , DecodeAddress
    , EncodeAddress
    , Iso8601Time (..)
    , PostExternalTransactionData (..)
    , PostTransactionData (..)
    , PostTransactionFeeData (..)
    , WalletPostData (..)
    , WalletPutData (..)
    , WalletPutPassphraseData (..)
    )
import Cardano.Wallet.Primitive.Types
    ( AddressState, PoolId, SortOrder, SyncProgress (..), WalletId )
import Control.Monad
    ( void )
import Control.Retry
    ( RetryPolicy, constantDelay, limitRetriesByCumulativeDelay, retrying )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Proxy
    ( Proxy (..) )
import Servant
    ( (:<|>) (..), (:>), NoContent )
import Servant.Client
    ( ClientM, client )

{-------------------------------------------------------------------------------
                              Server Interaction
-------------------------------------------------------------------------------}

-- | This data type encapsulates the client functions for all endpoints of the
-- cardano-wallet V2 API.
data WalletClient t = WalletClient
    { listAddresses
        :: ApiT WalletId
        -> Maybe (ApiT AddressState)
        -> ClientM [ApiAddress t]
    , deleteWallet
        :: ApiT WalletId
        -> ClientM ()
    , getWallet
        :: ApiT WalletId
        -> ClientM ApiWallet
    , getWalletUtxoStatistics
        :: ApiT WalletId
        -> ClientM ApiUtxoStatistics
    , listWallets
        :: ClientM [ApiWallet]
    , postWallet
        :: WalletPostData
        -> ClientM ApiWallet
    , putWallet
        :: ApiT WalletId
        -> WalletPutData
        -> ClientM ApiWallet
    , putWalletPassphrase
        :: ApiT WalletId
        -> WalletPutPassphraseData
        -> ClientM NoContent
    , listTransactions
        :: ApiT WalletId
        -> Maybe Iso8601Time
        -> Maybe Iso8601Time
        -> Maybe (ApiT SortOrder)
        -> ClientM [ApiTransaction t]
    , postTransaction
        :: ApiT WalletId
        -> PostTransactionData t
        -> ClientM (ApiTransaction t)
    , postTransactionFee
        :: ApiT WalletId
        -> PostTransactionFeeData t
        -> ClientM ApiFee
    , postExternalTransaction
        :: PostExternalTransactionData
        -> ClientM ApiTxId
    , deleteTransaction
        :: ApiT WalletId
        -> ApiTxId
        -> ClientM NoContent
    , listPools
        :: ClientM [ApiStakePool]
    , joinStakePool
        :: ApiT PoolId
        -> ApiT WalletId
        -> ApiWalletPassphrase
        -> ClientM (ApiTransaction t)
    , quitStakePool
        :: ApiT PoolId
        -> ApiT WalletId
        -> ApiWalletPassphrase
        -> ClientM (ApiTransaction t)
    , networkInformation
        :: ClientM ApiNetworkInformation
    }

-- | Produces a 'WalletClient' for the cardano-wallet V2 API.
--
-- You should apply a
-- "Cardano.Wallet.Primitive.AddressDerivation.NetworkDiscriminant" type
-- parameter to this to get a concrete client.
walletClient :: forall t. (DecodeAddress t, EncodeAddress t) => WalletClient t
walletClient =
    let
        (wallets
            :<|> addresses
            :<|> coinSelections
            :<|> transactions
            :<|> stakePools
            :<|> _byronWallets
            :<|> _byronTransactions
            :<|> _byronMigrations
            :<|> network
            :<|> proxy_) =
            client (Proxy @("v2" :> (Api t)))

        _deleteWallet
            :<|> _getWallet
            :<|> _listWallets
            :<|> _postWallet
            :<|> _putWallet
            :<|> _putWalletPassphrase
            :<|> _getWalletUtxoStatistics
            = wallets

        _listAddresses =
            addresses

        _selectCoins
            = coinSelections

        _postTransaction
            :<|> _listTransactions
            :<|> _postTransactionFee
            :<|> _deleteTransaction
            = transactions

        _listPools
            :<|> _joinStakePool
            :<|> _quitStakePool
            :<|> _delegationFee
            = stakePools

        _networkInformation = network

        _postExternalTransaction
            = proxy_
    in
        WalletClient
            { listAddresses = _listAddresses
            , deleteWallet = void . _deleteWallet
            , getWallet = _getWallet
            , listWallets = _listWallets
            , postWallet = _postWallet
            , putWallet = _putWallet
            , putWalletPassphrase = _putWalletPassphrase
            , listTransactions = _listTransactions
            , postTransaction = _postTransaction
            , postExternalTransaction = _postExternalTransaction
            , deleteTransaction = _deleteTransaction
            , postTransactionFee = _postTransactionFee
            , getWalletUtxoStatistics = _getWalletUtxoStatistics
            , listPools = _listPools
            , joinStakePool = _joinStakePool
            , quitStakePool = _quitStakePool
            , networkInformation = _networkInformation
            }

{-------------------------------------------------------------------------------
                                    Helpers
-------------------------------------------------------------------------------}

-- | Poll a wallet by ID until it has restored.
waitForRestore :: WalletClient t -> ApiT WalletId -> ClientM ApiWallet
waitForRestore wc wid = retrying retryPolicy (const shouldRetry) (const action)
  where
    shouldRetry res = pure (res ^. #state /= ApiT Ready)
    action = getWallet wc wid

-- | Poll the wallet server until it reports that it has synced with the
-- network.
waitForSync :: WalletClient t -> ClientM ()
waitForSync wc = void $ retrying retryPolicy (const shouldRetry) (const action)
  where
    shouldRetry res = pure (res ^. #syncProgress /= ApiT Ready)
    action = networkInformation wc

retryPolicy :: RetryPolicy
retryPolicy = limitRetriesByCumulativeDelay (3600 * second) (constantDelay second)
  where
    second = 1000*1000
