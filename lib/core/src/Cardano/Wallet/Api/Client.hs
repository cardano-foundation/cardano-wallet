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
    , ApiDecodedTransactionT
    , ApiFee
    , ApiNetworkClock
    , ApiNetworkInformation (..)
    , ApiNetworkParameters
    , ApiPoolId
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
    , ApiWalletUtxoSnapshot (..)
    , Base (Base64)
    , ByronWalletPutPassphraseData (..)
    , Iso8601Time (..)
    , PostTransactionFeeOldDataT
    , PostTransactionOldDataT
    , WalletPutData (..)
    , WalletPutPassphraseData (..)
    )
import Cardano.Wallet.Api.Types.SchemaMetadata
    ( TxMetadataSchema, toSimpleMetadataFlag )
import Cardano.Wallet.Primitive.Types
    ( SortOrder, WalletId )
import Cardano.Wallet.Primitive.Types.Address
    ( AddressState )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( SealedTx, SerialisedTx (..), unsafeSealedTxFromBytes )
import Control.Monad
    ( void )
import Data.Coerce
    ( coerce )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
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
import UnliftIO.Exception
    ( throwString )

import qualified Data.Aeson as Aeson

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
        -> WalletPutData
        -> ClientM wallet
    , putWalletPassphrase
        :: ApiT WalletId
        -> WalletPutPassphraseData
        -> ClientM NoContent
    }

data TransactionClient = TransactionClient
    { listTransactions
        :: ApiT WalletId
        -> Maybe Iso8601Time
        -> Maybe Iso8601Time
        -> Maybe (ApiT SortOrder)
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
        -> ApiSerialisedTransaction
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

data StakePoolClient apiPool = StakePoolClient
    { listPools
        :: Maybe (ApiT Coin) -> ClientM [apiPool]
    , joinStakePool
        :: ApiPoolId
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
            :<|> _putWallet
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
            , putWallet = _putWallet
            , putWalletPassphrase = \wid (WalletPutPassphraseData (Left req)) ->
                _putWalletPassphrase wid $ ByronWalletPutPassphraseData
                    { oldPassphrase = Just $ coerce <$> req ^. #oldPassphrase
                    , newPassphrase = req ^. #newPassphrase
                    }
            , getWalletUtxoSnapshot = _getWalletUtxoSnapshot
            , getWalletUtxoStatistics = _getWalletUtxoStatistics
            }

-- | Produces a 'TransactionClient t' working against the /wallets API.
transactionClient
    :: TransactionClient
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
byronTransactionClient
    :: TransactionClient
byronTransactionClient =
    let
        _constructTransaction
            :<|> _signTransaction
            :<|> _listTransactions
            :<|> _getTransaction
            :<|> _deleteTransaction
            :<|> _postTransaction
            :<|> _postTransactionFee
            = client (Proxy @("v2" :> (ByronTransactions Aeson.Value)))

        _postExternalTransaction
            = client (Proxy @("v2" :> Proxy_))

    in TransactionClient
        { listTransactions = \wid start end order _ ->
            _listTransactions wid start end order
        , signTransaction = _signTransaction
        , postTransaction = _postTransaction
        , postTransactionFee = _postTransactionFee
        , postExternalTransaction = _postExternalTransaction . fromSerialisedTx
        , deleteTransaction = _deleteTransaction
        , getTransaction = \wid txid _  -> _getTransaction wid txid
        , constructTransaction = _constructTransaction
        , balanceTransaction = error "balance transaction endpoint not supported for byron"
        , decodeTransaction = error "decode transaction endpoint not supported for byron"
        , submitTransaction = error "submit transaction endpoint not supported for byron"
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
            , postRandomAddress = \_ _ -> throwString "feature unavailable."
            , putRandomAddress  = \_ _ -> throwString "feature unavailable."
            , putRandomAddresses = \_ _ -> throwString "feature unavailable."
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
    :: forall apiPool. Aeson.FromJSON apiPool => StakePoolClient apiPool
stakePoolClient =
    let
        _listPools
            :<|> _joinStakePool
            :<|> _quitStakePool
            :<|> _delegationFee
            :<|> _listStakeKeys
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
