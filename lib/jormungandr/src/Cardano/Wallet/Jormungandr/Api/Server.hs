{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

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

import Cardano.Pool.Jormungandr.Metadata
    ( ApiStakePool (..), ApiStakePoolMetrics (..), StakePoolMetadata )
import Cardano.Pool.Jormungandr.Metrics
    ( ErrListStakePools (..), StakePoolLayer (..) )
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
    , ByronCoinSelections
    , ByronMigrations
    , ByronTransactions
    , ByronWallets
    , CoinSelections
    , Network
    , Proxy_
    , Settings
    , ShelleyMigrations
    , StakePools
    , Transactions
    , WalletKeys
    , Wallets
    )
import Cardano.Wallet.Api.Server
    ( LiftHandler (..)
    , apiError
    , delegationFee
    , deleteTransaction
    , deleteWallet
    , derivePublicKey
    , getMigrationInfo
    , getNetworkClock
    , getNetworkInformation
    , getNetworkParameters
    , getTransaction
    , getUTxOsStatistics
    , getWallet
    , idleWorker
    , joinStakePool
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
    , signMetadata
    , withLegacyLayer
    , withLegacyLayer'
    )
import Cardano.Wallet.Api.Types
    ( ApiErrorCode (..)
    , ApiListStakePools (..)
    , ApiSelectCoinsData (..)
    , ApiT (..)
    , SomeByronWalletPostData (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( DelegationAddress (..), NetworkDiscriminant (..), PaymentAddress )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Jormungandr
    ( JormungandrKey (..), generateKeyFromSeed )
import Cardano.Wallet.Primitive.AddressDiscovery.Random
    ( RndState )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( SeqState )
import Cardano.Wallet.Primitive.Types
    ( Coin, StakePool (..) )
import Control.Applicative
    ( liftA2 )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.List
    ( sortOn )
import Data.Quantity
    ( Quantity (..) )
import Data.Text.Class
    ( ToText (..) )
import Fmt
    ( Buildable )
import Network.Ntp
    ( NtpClient )
import Servant
    ( (:<|>) (..), Handler (..), Server, err501, err503, throwError )
import Type.Reflection
    ( Typeable )

-- | A Servant server for our wallet API
server
    :: forall t n byron icarus jormungandr.
        ( byron ~ ApiLayer (RndState 'Mainnet) t ByronKey
        , icarus ~ ApiLayer (SeqState 'Mainnet IcarusKey) t IcarusKey
        , jormungandr ~ ApiLayer (SeqState n JormungandrKey) t JormungandrKey
        , DelegationAddress n JormungandrKey
        , Buildable (ErrValidateSelection t)
        , PaymentAddress n ByronKey
        , Typeable n
        )
    => byron
    -> icarus
    -> jormungandr
    -> StakePoolLayer ErrListStakePools IO
    -> NtpClient
    -> Server (Api n ApiStakePool)
server byron icarus jormungandr spl ntp =
         wallets
    :<|> walletKeys
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
    :<|> settingS
  where
    wallets :: Server Wallets
    wallets = deleteWallet jormungandr
        :<|> (fmap fst . getWallet jormungandr mkShelleyWallet)
        :<|> (fmap fst <$> listWallets jormungandr mkShelleyWallet)
        :<|> postWallet jormungandr generateKeyFromSeed JormungandrKey
        :<|> putWallet jormungandr mkShelleyWallet
        :<|> putWalletPassphrase jormungandr
        :<|> getUTxOsStatistics jormungandr

    walletKeys :: Server WalletKeys
    walletKeys = derivePublicKey jormungandr
        :<|> signMetadata jormungandr

    addresses :: Server (Addresses n)
    addresses = listAddresses jormungandr (normalizeDelegationAddress @_ @JormungandrKey @n)
        :<|> (\_ -> throwError err501)

    -- Hlint doesn't seem to care about inlining properties:
    --   https://github.com/quchen/articles/blob/master/fbut.md#f-x---is-not-f--x---
    {-# HLINT ignore "Redundant lambda" #-}
    coinSelections :: Server (CoinSelections n)
    coinSelections =
        \wid ascd -> case ascd of
                (ApiSelectForPayment ascp) ->
                    selectCoins jormungandr (delegationAddress @n) wid ascp
                (ApiSelectForDelegation _) ->
                    throwError err501

    transactions :: Server (Transactions n)
    transactions =
        postTransaction jormungandr (delegationAddress @n)
        :<|> listTransactions jormungandr
        :<|> postTransactionFee jormungandr
        :<|> deleteTransaction jormungandr
        :<|> getTransaction jormungandr

    shelleyMigrations :: Server (ShelleyMigrations n)
    shelleyMigrations =
             (\_ -> throwError err501)
        :<|> (\_ _ -> throwError err501)

    stakePools :: Server (StakePools n ApiStakePool)
    stakePools = (listPools spl)
        :<|>
            joinStakePool jormungandr
                (knownStakePools spl)
                (getPoolLifeCycleStatus spl)
        :<|> quitStakePool jormungandr
        :<|> delegationFee jormungandr
        :<|> (\_ -> throwError err501)

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
            (listWallets byron (mkLegacyWallet @_ @_ @_ @t))
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
             (\_ _ -> throwError err501)
        :<|> (\_ _ -> throwError err501)
        :<|> (\_ _ -> throwError err501)
        :<|> (\_ _ -> throwError err501)

    byronCoinSelections :: Server (ByronCoinSelections n)
    byronCoinSelections _ _ = throwError err501

    byronTransactions :: Server (ByronTransactions n)
    byronTransactions =
             (\_ _ -> throwError err501)
        :<|>
             (\wid r0 r1 s -> withLegacyLayer wid
                (byron , listTransactions byron wid Nothing r0 r1 s)
                (icarus, listTransactions icarus wid Nothing r0 r1 s)
             )
        :<|>
             (\_ _ -> throwError err501)
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
                (byron , migrateWallet @_ @_ @_ @n byron wid m)
                (icarus, migrateWallet @_ @_ @_ @n icarus wid m)
             )

    network :: Server Network
    network =
        getNetworkInformation syncTolerance nl
        :<|> getNetworkParameters genesis nl
        :<|> getNetworkClock ntp
      where
        nl = jormungandr ^. networkLayer @t
        genesis@(_,_,syncTolerance) = jormungandr ^. genesisData

    proxy :: Server Proxy_
    proxy = postExternalTransaction jormungandr

    settingS :: Server Settings
    settingS = (\_ -> throwError err501) :<|> throwError err501


--------------------------------------------------------------------------------
-- List stake pools API handler
--------------------------------------------------------------------------------

listPools
    :: LiftHandler e
    => StakePoolLayer e IO
    -> Maybe (ApiT Coin)
    -- ^ Not needed, but there for consistency with haskell node.
    -> Handler (ApiListStakePools ApiStakePool)
listPools spl _walletId = do
    ps <- liftHandler $ map (uncurry mkApiStakePool) <$> listStakePools spl
    pure (ApiListStakePools ps Nothing)
  where
    mkApiStakePool
        :: StakePool
        -> Maybe StakePoolMetadata
        -> ApiStakePool
    mkApiStakePool sp meta =
        ApiStakePool
            (ApiT $ view #poolId sp)
            (ApiStakePoolMetrics
                (Quantity $ fromIntegral $ getQuantity $ stake sp)
                (Quantity $ fromIntegral $ getQuantity $ production sp))
            (sp ^. #performance)
            (ApiT <$> meta)
            (fromIntegral <$> sp ^. #cost)
            (Quantity $ sp ^. #margin)
            (sp ^. #desirability)
            (sp ^. #saturation)

instance LiftHandler ErrListStakePools where
     handler = \case
         ErrListStakePoolsCurrentNodeTip e -> handler e
         ErrMetricsIsUnsynced p ->
             apiError err503 NotSynced $ mconcat
                 [ "I can't list stake pools yet because I need to scan the "
                 , "blockchain for metrics first. I'm at "
                 , toText p
                 ]
