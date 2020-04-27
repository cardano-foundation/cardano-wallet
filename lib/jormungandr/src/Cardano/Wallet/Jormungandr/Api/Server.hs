{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- API handlers and server using the underlying wallet layer to provide
-- endpoints reachable through HTTP.

module Cardano.Wallet.Jormungandr.Api.Server
    ( server
    ) where

import Prelude

import Cardano.Pool
    ( StakePoolLayer )
import Cardano.Pool.Jormungandr.Metrics
    ( ErrListStakePools (..) )
import Cardano.Wallet
    ( ErrValidateSelection
    , genesisData
    , networkLayer
    , normalizeDelegationAddress
    )
import Cardano.Wallet.Api
    ( Addresses
    , Api
    , ApiLayer (..)
    , ByronAddresses
    , ByronMigrations
    , ByronTransactions
    , ByronWallets
    , CoinSelections
    , Network
    , Proxy_
    , StakePools
    , Transactions
    , Wallets
    )
import Cardano.Wallet.Api.Server
    ( LiftHandler (..)
    , apiError
    , delegationFee
    , deleteTransaction
    , deleteWallet
    , forceResyncWallet
    , getMigrationInfo
    , getNetworkClock
    , getNetworkInformation
    , getNetworkParameters
    , getUTxOsStatistics
    , getWallet
    , joinStakePool
    , listAddresses
    , listPools
    , listTransactions
    , listWallets
    , migrateWallet
    , mkLegacyWallet
    , mkShelleyWallet
    , postExternalTransaction
    , postIcarusWallet
    , postLedgerWallet
    , postRandomWallet
    , postRandomWalletFromXPrv
    , postTransaction
    , postTransactionFee
    , postTrezorWallet
    , postWallet
    , putByronWalletPassphrase
    , putWallet
    , putWalletPassphrase
    , quitStakePool
    , selectCoins
    , withLegacyLayer
    , withLegacyLayer'
    )
import Cardano.Wallet.Api.Types
    ( ApiErrorCode (..), SomeByronWalletPostData (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( DelegationAddress (..), NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey )
import Cardano.Wallet.Primitive.AddressDiscovery.Random
    ( RndState )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( SeqState )
import Control.Applicative
    ( liftA2 )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.List
    ( sortOn )
import Data.Text.Class
    ( ToText (..) )
import Fmt
    ( Buildable )
import Network.Ntp
    ( NtpClient )
import Servant
    ( (:<|>) (..), Server, err501, err503, throwError )

-- | A Servant server for our wallet API
server
    :: forall t n byron icarus shelley.
        ( byron ~ ApiLayer (RndState 'Mainnet) t ByronKey
        , icarus ~ ApiLayer (SeqState 'Mainnet IcarusKey) t IcarusKey
        , shelley ~ ApiLayer (SeqState n ShelleyKey) t ShelleyKey
        , DelegationAddress n ShelleyKey
        , Buildable (ErrValidateSelection t)
        )
    => byron
    -> icarus
    -> shelley
    -> StakePoolLayer ErrListStakePools IO
    -> NtpClient
    -> Server (Api n)
server byron icarus shelley spl ntp =
         wallets
    :<|> addresses
    :<|> coinSelections
    :<|> transactions
    :<|> stakePools
    :<|> byronWallets
    :<|> byronAddresses
    :<|> byronTransactions
    :<|> byronMigrations
    :<|> network
    :<|> proxy
  where
    wallets :: Server Wallets
    wallets = deleteWallet shelley
        :<|> (fmap fst . getWallet shelley mkShelleyWallet)
        :<|> (fmap fst <$> listWallets shelley mkShelleyWallet)
        :<|> postWallet shelley
        :<|> putWallet shelley mkShelleyWallet
        :<|> putWalletPassphrase shelley
        :<|> getUTxOsStatistics shelley
        :<|> forceResyncWallet shelley

    addresses :: Server (Addresses n)
    addresses = listAddresses shelley (normalizeDelegationAddress @_ @_ @n)

    coinSelections :: Server (CoinSelections n)
    coinSelections = selectCoins shelley

    transactions :: Server (Transactions n)
    transactions =
        postTransaction shelley (delegationAddress @n)
        :<|> listTransactions shelley
        :<|> postTransactionFee shelley
        :<|> deleteTransaction shelley

    stakePools :: Server (StakePools n)
    stakePools = listPools spl
        :<|> joinStakePool shelley spl
        :<|> quitStakePool shelley
        :<|> delegationFee shelley

    byronWallets :: Server ByronWallets
    byronWallets =
        (\case
            RandomWalletFromMnemonic x -> postRandomWallet byron x
            RandomWalletFromXPrv x -> postRandomWalletFromXPrv byron x
            SomeIcarusWallet x -> postIcarusWallet icarus x
            SomeTrezorWallet x -> postTrezorWallet icarus x
            SomeLedgerWallet x -> postLedgerWallet icarus x
        )
        :<|> (\wid -> withLegacyLayer wid
                (byron , deleteWallet byron wid)
                (icarus, deleteWallet icarus wid)
             )
        :<|> (\wid -> withLegacyLayer' wid
                ( byron
                , fst <$> getWallet byron mkLegacyWallet wid
                , const (fst <$> getWallet byron mkLegacyWallet wid)
                )
                ( icarus
                , fst <$> getWallet icarus mkLegacyWallet wid
                , const (fst <$> getWallet icarus mkLegacyWallet wid)
                )
             )
        :<|> liftA2 (\xs ys -> fmap fst $ sortOn snd $ xs ++ ys)
            (listWallets byron  mkLegacyWallet)
            (listWallets icarus mkLegacyWallet)
        :<|> (\wid tip -> withLegacyLayer wid
                (byron , forceResyncWallet byron  wid tip)
                (icarus, forceResyncWallet icarus wid tip)
             )
        :<|> (\wid name -> withLegacyLayer wid
                (byron , putWallet byron mkLegacyWallet wid name)
                (icarus, putWallet icarus mkLegacyWallet wid name)
             )
        :<|> (\wid -> withLegacyLayer wid
                (byron , getUTxOsStatistics byron wid)
                (icarus, getUTxOsStatistics icarus wid)
             )
        :<|> (\wid pwd -> withLegacyLayer wid
                (byron , putByronWalletPassphrase byron wid pwd)
                (icarus, putByronWalletPassphrase icarus wid pwd)
             )

    byronAddresses :: Server (ByronAddresses n)
    byronAddresses =
             (\_ _ -> throwError err501)
        :<|> (\_ _ -> throwError err501)

    byronTransactions :: Server (ByronTransactions n)
    byronTransactions =
             (\_ _ -> throwError err501)
        :<|>
             (\wid r0 r1 s -> withLegacyLayer wid
                (byron , listTransactions byron  wid r0 r1 s)
                (icarus, listTransactions icarus wid r0 r1 s)
             )
        :<|>
             (\_ _ -> throwError err501)
        :<|> (\wid txid -> withLegacyLayer wid
                (byron , deleteTransaction byron  wid txid)
                (icarus, deleteTransaction icarus wid txid)
             )

    byronMigrations :: Server (ByronMigrations n)
    byronMigrations =
             (\wid -> withLegacyLayer wid
                (byron , getMigrationInfo byron  wid)
                (icarus, getMigrationInfo icarus wid)
             )
        :<|> (\from to pwd -> withLegacyLayer from
                (byron , migrateWallet byron  shelley from to pwd)
                (icarus, migrateWallet icarus shelley from to pwd)
             )

    network :: Server Network
    network =
        getNetworkInformation genesis nl
        :<|> (getNetworkParameters genesis)
        :<|> (getNetworkClock ntp)
      where
        nl = shelley ^. networkLayer @t
        genesis = shelley ^. genesisData

    proxy :: Server Proxy_
    proxy = postExternalTransaction shelley

instance LiftHandler ErrListStakePools where
     handler = \case
         ErrListStakePoolsCurrentNodeTip e -> handler e
         ErrMetricsIsUnsynced p ->
             apiError err503 NotSynced $ mconcat
                 [ "I can't list stake pools yet because I need to scan the "
                 , "blockchain for metrics first. I'm at "
                 , toText p
                 ]
