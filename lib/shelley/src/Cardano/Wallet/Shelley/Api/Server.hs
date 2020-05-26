{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- API handlers and server using the underlying wallet layer to provide
-- endpoints reachable through HTTP.

module Cardano.Wallet.Shelley.Api.Server
    ( server
    ) where

import Prelude

import Cardano.Wallet
    ( ErrCreateRandomAddress (..)
    , ErrValidateSelection
    , genesisData
    , networkLayer
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
    , ShelleyMigrations
    , StakePools
    , Transactions
    , Wallets
    )
import Cardano.Wallet.Api.Server
    ( deleteTransaction
    , deleteWallet
    , getMigrationInfo
    , getNetworkClock
    , getNetworkInformation
    , getNetworkParameters
    , getUTxOsStatistics
    , getWallet
    , liftHandler
    , listAddresses
    , listTransactions
    , listWallets
    , migrateWallet
    , mkLegacyWallet
    , mkShelleyWallet
    , postExternalTransaction
    , postIcarusWallet
    , postLedgerWallet
    , postRandomAddress
    , postRandomWallet
    , postRandomWalletFromXPrv
    , postTransaction
    , postTransactionFee
    , postTrezorWallet
    , postWallet
    , putByronWalletPassphrase
    , putRandomAddress
    , putWallet
    , putWalletPassphrase
    , rndStateChange
    , withLegacyLayer
    , withLegacyLayer'
    )
import Cardano.Wallet.Api.Types
    ( ApiT (..), SomeByronWalletPostData (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( PaymentAddress (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey (..), generateKeyFromSeed )
import Cardano.Wallet.Primitive.AddressDiscovery.Random
    ( RndState )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( SeqState )
import Control.Applicative
    ( liftA2 )
import Control.Monad.Trans.Except
    ( throwE )
import Data.Coerce
    ( coerce )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Generics.Labels
    ()
import Data.List
    ( sortOn )
import Fmt
    ( Buildable )
import Network.Ntp
    ( NtpClient )
import Servant
    ( (:<|>) (..), Server, err501, throwError )

-- | A diminished servant server to serve Byron wallets only.
server
    :: forall t n.
        ( Buildable (ErrValidateSelection t)
        , PaymentAddress n IcarusKey
        , PaymentAddress n ByronKey
        )
    => ApiLayer (RndState n) t ByronKey
    -> ApiLayer (SeqState n IcarusKey) t IcarusKey
    -> ApiLayer (SeqState n ShelleyKey) t ShelleyKey
    -> NtpClient
    -> Server (Api n)
server byron icarus shelley ntp =
         wallets
    :<|> addresses
    :<|> coinSelections
    :<|> transactions
    :<|> shelleyMigrations
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
        :<|> postWallet shelley generateKeyFromSeed ShelleyKey
        :<|> putWallet shelley mkShelleyWallet
        :<|> putWalletPassphrase shelley
        :<|> getUTxOsStatistics shelley

    addresses :: Server (Addresses n)
    addresses _ _ = throwError err501

    coinSelections :: Server (CoinSelections n)
    coinSelections _ _ = throwError err501

    transactions :: Server (Transactions n)
    transactions =
             (\_ _ -> throwError err501)
        :<|> (\_ _ _ _ -> throwError err501)
        :<|> (\_ _ -> throwError err501)
        :<|> (\_ _ -> throwError err501)

    shelleyMigrations :: Server (ShelleyMigrations n)
    shelleyMigrations =
             getMigrationInfo shelley
        :<|> migrateWallet shelley

    stakePools :: Server (StakePools n)
    stakePools =
             throwError err501
        :<|> (\_ _ _ -> throwError err501)
        :<|> (\_ _ -> throwError err501)
        :<|> (\_ -> throwError err501)

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
                , fst <$> getWallet byron  mkLegacyWallet wid
                , const (fst <$> getWallet byron  mkLegacyWallet wid)
                )
                ( icarus
                , fst <$> getWallet icarus mkLegacyWallet wid
                , const (fst <$> getWallet icarus mkLegacyWallet wid)
                )
             )
        :<|> liftA2 (\xs ys -> fmap fst $ sortOn snd $ xs ++ ys)
            (listWallets byron  mkLegacyWallet)
            (listWallets icarus mkLegacyWallet)
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
             (\wid s -> withLegacyLayer wid
                (byron, postRandomAddress byron wid s)
                (icarus, liftHandler $ throwE ErrCreateAddressNotAByronWallet)
             )
        :<|> (\wid addr -> withLegacyLayer wid
                (byron, putRandomAddress byron wid addr)
                (icarus, liftHandler $ throwE ErrCreateAddressNotAByronWallet)
             )
        :<|> (\wid s -> withLegacyLayer wid
                (byron , listAddresses byron (const pure) wid s)
                (icarus, listAddresses icarus (const pure) wid s)
             )

    byronTransactions :: Server (ByronTransactions n)
    byronTransactions =
             (\wid tx -> withLegacyLayer wid
                 (byron , do
                    let pwd = coerce (getApiT $ tx ^. #passphrase)
                    genChange <- rndStateChange byron wid pwd
                    postTransaction byron genChange wid tx
                 )
                 (icarus, do
                    let genChange k _ = paymentAddress @n k
                    postTransaction icarus genChange wid tx
                 )
             )
        :<|>
             (\wid r0 r1 s -> withLegacyLayer wid
                (byron , listTransactions byron wid r0 r1 s)
                (icarus, listTransactions icarus wid r0 r1 s)
             )
        :<|>
            (\wid tx -> withLegacyLayer wid
                (byron , postTransactionFee byron wid tx)
                (icarus, postTransactionFee icarus wid tx)
            )
        :<|> (\wid txid -> withLegacyLayer wid
                (byron , deleteTransaction byron wid txid)
                (icarus, deleteTransaction icarus wid txid)
             )

    byronMigrations :: Server (ByronMigrations n)
    byronMigrations =
             (\wid -> withLegacyLayer wid
                (byron , getMigrationInfo byron wid)
                (icarus, getMigrationInfo icarus wid)
             )
        :<|> \_ _ -> throwError err501

    network :: Server Network
    network =
        getNetworkInformation genesis nl
        :<|> (getNetworkParameters genesis)
        :<|> (getNetworkClock ntp)
      where
        nl = icarus ^. networkLayer @t
        genesis = icarus ^. genesisData

    proxy :: Server Proxy_
    proxy = postExternalTransaction icarus
