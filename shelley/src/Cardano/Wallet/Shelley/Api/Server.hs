{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

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
    , ErrNotASequentialWallet (..)
    , ErrValidateSelection
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
    , ShelleyMigrations
    , StakePools
    , Transactions
    , Wallets
    )
import Cardano.Wallet.Api.Server
    ( apiError
    , delegationFee
    , deleteTransaction
    , deleteWallet
    , getCurrentEpoch
    , getMigrationInfo
    , getNetworkClock
    , getNetworkInformation
    , getNetworkParameters
    , getTransaction
    , getUTxOsStatistics
    , getWallet
    , idleWorker
    , joinStakePool
    , liftHandler
    , listAddresses
    , listTransactions
    , listWallets
    , migrateWallet
    , mkLegacyWallet
    , mkShelleyWallet
    , postAccountWallet
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
    , putRandomAddresses
    , putWallet
    , putWalletPassphrase
    , quitStakePool
    , rndStateChange
    , selectCoins
    , withLegacyLayer
    , withLegacyLayer'
    )
import Cardano.Wallet.Api.Types
    ( ApiErrorCode (..)
    , ApiStakePool
    , ApiT (..)
    , SomeByronWalletPostData (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( DelegationAddress (..), PaymentAddress (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey (..), generateKeyFromSeed )
import Cardano.Wallet.Primitive.AddressDiscovery.Random
    ( RndState )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( SeqState )
import Cardano.Wallet.Shelley.Pools
    ( StakePoolLayer (..) )
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
    ( (:<|>) (..), Handler (..), Server, err400 )
import Type.Reflection
    ( Typeable )

server
    :: forall t n.
        ( Buildable (ErrValidateSelection t)
        , PaymentAddress n IcarusKey
        , PaymentAddress n ByronKey
        , DelegationAddress n ShelleyKey
        , Typeable n
        )
    => ApiLayer (RndState n) t ByronKey
    -> ApiLayer (SeqState n IcarusKey) t IcarusKey
    -> ApiLayer (SeqState n ShelleyKey) t ShelleyKey
    -> StakePoolLayer
    -> NtpClient
    -> Server (Api n ApiStakePool)
server byron icarus shelley spl ntp =
         wallets
    :<|> addresses
    :<|> coinSelections
    :<|> transactions
    :<|> shelleyMigrations
    :<|> stakePools
    :<|> byronWallets
    :<|> byronAddresses
    :<|> byronCoinSelections
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
    addresses = listAddresses shelley
        (normalizeDelegationAddress @_ @ShelleyKey @n)

    coinSelections :: Server (CoinSelections n)
    coinSelections = selectCoins shelley (delegationAddress @n)

    transactions :: Server (Transactions n)
    transactions =
        postTransaction shelley (delegationAddress @n)
        :<|> listTransactions shelley
        :<|> postTransactionFee shelley
        :<|> deleteTransaction shelley
        :<|> getTransaction shelley

    shelleyMigrations :: Server (ShelleyMigrations n)
    shelleyMigrations =
             getMigrationInfo @_ @_ @_ @n shelley
        :<|> migrateWallet shelley

    stakePools :: Server (StakePools n ApiStakePool)
    stakePools =
        listStakePools_
        :<|> joinStakePool shelley (knownPools spl) (getPoolLifeCycleStatus spl)
        :<|> quitStakePool shelley
        :<|> delegationFee shelley
      where
        listStakePools_ = \case
            Just (ApiT stake) -> do
                currentEpoch <- getCurrentEpoch shelley
                liftHandler $ listStakePools spl currentEpoch stake
            Nothing -> Handler $ throwE $ apiError err400 QueryParamMissing $
                mconcat
                [ "The stake intended to delegate must be provided as a query "
                , "parameter as it affects the rewards and ranking."
                ]

    byronWallets :: Server ByronWallets
    byronWallets =
        (\case
            RandomWalletFromMnemonic x -> postRandomWallet byron x
            RandomWalletFromXPrv x -> postRandomWalletFromXPrv byron x
            SomeIcarusWallet x -> postIcarusWallet icarus x
            SomeTrezorWallet x -> postTrezorWallet icarus x
            SomeLedgerWallet x -> postLedgerWallet icarus x
            SomeAccount x ->
                postAccountWallet icarus (mkLegacyWallet @_ @_ @_ @t) IcarusKey idleWorker x
        )
        :<|> (\wid -> withLegacyLayer wid
                (byron , deleteWallet byron wid)
                (icarus, deleteWallet icarus wid)
             )
        :<|> (\wid -> withLegacyLayer' wid
                ( byron
                , fst <$> getWallet byron (mkLegacyWallet @_ @_ @_ @t) wid
                , const (fst <$> getWallet byron (mkLegacyWallet @_ @_ @_ @t) wid)
                )
                ( icarus
                , fst <$> getWallet icarus (mkLegacyWallet @_ @_ @_ @t) wid
                , const (fst <$> getWallet icarus (mkLegacyWallet @_ @_ @_ @t) wid)
                )
             )
        :<|> liftA2 (\xs ys -> fmap fst $ sortOn snd $ xs ++ ys)
            (listWallets byron  (mkLegacyWallet @_ @_ @_ @t))
            (listWallets icarus (mkLegacyWallet @_ @_ @_ @t))
        :<|> (\wid name -> withLegacyLayer wid
                (byron , putWallet byron (mkLegacyWallet @_ @_ @_ @t) wid name)
                (icarus, putWallet icarus (mkLegacyWallet @_ @_ @_ @t) wid name)
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
                (byron, putRandomAddresses byron wid s)
                (icarus, liftHandler $ throwE ErrCreateAddressNotAByronWallet)
             )
        :<|> (\wid s -> withLegacyLayer wid
                (byron , listAddresses byron (const pure) wid s)
                (icarus, listAddresses icarus (const pure) wid s)
             )

    byronCoinSelections :: Server (CoinSelections n)
    byronCoinSelections wid x = withLegacyLayer wid
        (byron, liftHandler $ throwE ErrNotASequentialWallet)
        (icarus, selectCoins icarus (const $ paymentAddress @n) wid x)

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
                (byron , listTransactions byron wid Nothing r0 r1 s)
                (icarus, listTransactions icarus wid Nothing r0 r1 s)
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
        :<|> (\wid txid -> withLegacyLayer wid
                (byron , getTransaction byron wid txid)
                (icarus, getTransaction icarus wid txid)
             )

    byronMigrations :: Server (ByronMigrations n)
    byronMigrations =
             (\wid -> withLegacyLayer wid
                (byron , getMigrationInfo @_ @_ @_ @n byron wid)
                (icarus, getMigrationInfo @_ @_ @_ @n icarus wid)
             )
        :<|> (\wid m -> withLegacyLayer wid
                (byron , migrateWallet byron wid m)
                (icarus, migrateWallet icarus wid m)
             )

    network :: Server Network
    network =
        getNetworkInformation syncTolerance nl
        :<|> getNetworkParameters genesis nl
        :<|> getNetworkClock ntp
      where
        nl = icarus ^. networkLayer @t
        genesis@(_,_,syncTolerance) = icarus ^. genesisData

    proxy :: Server Proxy_
    proxy = postExternalTransaction icarus
