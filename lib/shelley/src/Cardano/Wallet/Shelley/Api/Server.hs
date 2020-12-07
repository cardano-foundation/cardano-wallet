{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

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

import Cardano.Address
    ( unAddress )
import Cardano.Pool.Metadata
    ( defaultManagerSettings, healthCheck, newManager, toHealthCheckSMASH )
import Cardano.Wallet
    ( ErrCreateRandomAddress (..)
    , ErrNotASequentialWallet (..)
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
    , SMASH
    , Settings
    , ShelleyMigrations
    , StakePools
    , Transactions
    , WalletKeys
    , Wallets
    )
import Cardano.Wallet.Api.Server
    ( apiError
    , delegationFee
    , deleteTransaction
    , deleteWallet
    , derivePublicKey
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
    , selectCoinsForJoin
    , selectCoinsForQuit
    , signMetadata
    , withLegacyLayer
    , withLegacyLayer'
    )
import Cardano.Wallet.Api.Types
    ( AnyAddress (..)
    , AnyAddressType (..)
    , ApiAddressData (..)
    , ApiAddressInspect (..)
    , ApiAddressInspectData (..)
    , ApiCredential (..)
    , ApiErrorCode (..)
    , ApiHealthCheck (..)
    , ApiMaintenanceAction (..)
    , ApiMaintenanceActionPostData (..)
    , ApiSelectCoinsAction (..)
    , ApiSelectCoinsData (..)
    , ApiStakePool
    , ApiT (..)
    , HealthCheckSMASH (..)
    , MaintenanceAction (..)
    , SettingsPutData (..)
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
import Cardano.Wallet.Primitive.Types
    ( PoolMetadataSource (..), SmashServer (..), poolMetadataSource )
import Cardano.Wallet.Shelley.Compatibility
    ( HasNetworkId (..), NetworkId, inspectAddress )
import Cardano.Wallet.Shelley.Pools
    ( StakePoolLayer (..) )
import Cardano.Wallet.Transaction
    ( DelegationAction (..) )
import Control.Applicative
    ( liftA2 )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Except
    ( except, throwE, withExceptT )
import Data.Coerce
    ( coerce )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Generics.Labels
    ()
import Data.List
    ( sortOn )
import Data.Maybe
    ( fromJust )
import Data.Proxy
    ( Proxy (..) )
import Data.Text.Class
    ( TextDecodingError (..) )
import Network.Ntp
    ( NtpClient )
import Servant
    ( (:<|>) (..), Handler (..), NoContent (..), Server, err400, throwError )
import Servant.Server
    ( ServerError (..) )
import Type.Reflection
    ( Typeable )

import qualified Cardano.Address.Derivation as CA
import qualified Cardano.Address.Script as CA
import qualified Cardano.Address.Style.Shelley as CA
import qualified Cardano.Api.Typed as Cardano
import qualified Data.ByteString as BS
import qualified Data.Text as T

server
    :: forall n.
        ( PaymentAddress n IcarusKey
        , PaymentAddress n ByronKey
        , DelegationAddress n ShelleyKey
        , Typeable n
        , HasNetworkId n
        )
    => ApiLayer (RndState n) ByronKey
    -> ApiLayer (SeqState n IcarusKey) IcarusKey
    -> ApiLayer (SeqState n ShelleyKey) ShelleyKey
    -> StakePoolLayer
    -> NtpClient
    -> Server (Api n ApiStakePool)
server byron icarus shelley spl ntp =
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
    :<|> network'
    :<|> proxy
    :<|> settingS
    :<|> smash
  where
    wallets :: Server Wallets
    wallets = deleteWallet shelley
        :<|> (fmap fst . getWallet shelley mkShelleyWallet)
        :<|> (fmap fst <$> listWallets shelley mkShelleyWallet)
        :<|> postWallet shelley generateKeyFromSeed ShelleyKey
        :<|> putWallet shelley mkShelleyWallet
        :<|> putWalletPassphrase shelley
        :<|> getUTxOsStatistics shelley

    walletKeys :: Server WalletKeys
    walletKeys = derivePublicKey shelley
        :<|> signMetadata shelley

    addresses :: Server (Addresses n)
    addresses = listAddresses shelley (normalizeDelegationAddress @_ @ShelleyKey @n)
        :<|> (handler ApiAddressInspect . inspectAddress . unApiAddressInspectData)
        :<|> postAnyAddress (networkIdVal (Proxy @n))
      where
        toServerError :: TextDecodingError -> ServerError
        toServerError = apiError err400 BadRequest . T.pack . getTextDecodingError

        handler :: (a -> result) -> Either TextDecodingError a -> Handler result
        handler transform =
            Handler . withExceptT toServerError . except . fmap transform

    -- Hlint doesn't seem to care about inlining properties:
    --   https://github.com/quchen/articles/blob/master/fbut.md#f-x---is-not-f--x---
    {-# HLINT ignore "Redundant lambda" #-}
    coinSelections :: Server (CoinSelections n)
    coinSelections = (\wid ascd -> case ascd of
        (ApiSelectForPayment ascp) ->
            selectCoins shelley (delegationAddress @n) wid ascp
        (ApiSelectForDelegation (ApiSelectCoinsAction (ApiT action))) ->
            case action of
                Join pid -> selectCoinsForJoin
                    shelley
                    (knownPools spl)
                    (getPoolLifeCycleStatus spl)
                    pid
                    (getApiT wid)
                RegisterKeyAndJoin _ -> throwError err400
                Quit -> selectCoinsForQuit shelley wid
        )

    transactions :: Server (Transactions n)
    transactions =
        postTransaction shelley (delegationAddress @n)
        :<|> listTransactions shelley
        :<|> postTransactionFee shelley
        :<|> deleteTransaction shelley
        :<|> getTransaction shelley

    shelleyMigrations :: Server (ShelleyMigrations n)
    shelleyMigrations =
             getMigrationInfo @_ @_ @n shelley
        :<|> migrateWallet shelley

    stakePools :: Server (StakePools n ApiStakePool)
    stakePools =
        listStakePools_
        :<|> joinStakePool shelley (knownPools spl) (getPoolLifeCycleStatus spl)
        :<|> quitStakePool shelley
        :<|> delegationFee shelley
        :<|> postPoolMaintenance
        :<|> getPoolMaintenance
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

        postPoolMaintenance action = do
            case action of
                ApiMaintenanceActionPostData GcStakePools ->
                    liftIO $ forceMetadataGC spl
            pure NoContent

        getPoolMaintenance =
            liftIO (ApiMaintenanceAction . ApiT <$> getGCMetadataStatus spl)

    byronWallets :: Server ByronWallets
    byronWallets =
        (\case
            RandomWalletFromMnemonic x -> postRandomWallet byron x
            RandomWalletFromXPrv x -> postRandomWalletFromXPrv byron x
            SomeIcarusWallet x -> postIcarusWallet icarus x
            SomeTrezorWallet x -> postTrezorWallet icarus x
            SomeLedgerWallet x -> postLedgerWallet icarus x
            SomeAccount x ->
                postAccountWallet icarus mkLegacyWallet IcarusKey idleWorker x
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
                (byron, putRandomAddresses byron wid s)
                (icarus, liftHandler $ throwE ErrCreateAddressNotAByronWallet)
             )
        :<|> (\wid s -> withLegacyLayer wid
                (byron , listAddresses byron (const pure) wid s)
                (icarus, listAddresses icarus (const pure) wid s)
             )

    byronCoinSelections :: Server (ByronCoinSelections n)
    byronCoinSelections wid (ApiSelectForPayment x) =
        withLegacyLayer wid (byron, handleRandom) (icarus, handleSequential)
      where
        handleRandom = liftHandler $ throwE ErrNotASequentialWallet
        handleSequential = selectCoins icarus genChangeSequential wid x
        genChangeSequential paymentK _ = paymentAddress @n paymentK
    byronCoinSelections _ _ = Handler
        $ throwE
        $ apiError err400 InvalidWalletType
        "Byron wallets don't have delegation capabilities."

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
                (byron , getMigrationInfo @_ @_ @n byron wid)
                (icarus, getMigrationInfo @_ @_ @n icarus wid)
             )
        :<|> (\wid m -> withLegacyLayer wid
                (byron , migrateWallet byron wid m)
                (icarus, migrateWallet icarus wid m)
             )

    network' :: Server Network
    network' =
        getNetworkInformation syncTolerance nl
        :<|> getNetworkParameters genesis nl
        :<|> getNetworkClock ntp
      where
        nl = icarus ^. networkLayer
        genesis@(_,_,syncTolerance) = icarus ^. genesisData

    proxy :: Server Proxy_
    proxy = postExternalTransaction icarus

    settingS :: Server Settings
    settingS = putSettings' :<|> getSettings'
      where
        putSettings' (SettingsPutData (ApiT settings'))
            = Handler $ do
                liftIO $ putSettings spl settings'
                pure NoContent
        getSettings'
            = Handler $ fmap ApiT $ liftIO $ getSettings spl

    smash :: Server SMASH
    smash = getCurrentSmashHealth
      where
        getHealth smashServer = liftIO $ do
            manager <- newManager defaultManagerSettings
            health' <- healthCheck mempty (unSmashServer smashServer) manager
            pure $ ApiHealthCheck $ toHealthCheckSMASH health'

        getCurrentSmashHealth (Just (ApiT smashServer)) = Handler $ getHealth smashServer
        getCurrentSmashHealth Nothing = Handler $ do
            settings' <- liftIO $ getSettings spl
            case poolMetadataSource settings' of
                FetchSMASH smashServer -> getHealth smashServer
                _ -> pure (ApiHealthCheck NoSmashConfigured)

postAnyAddress
    :: NetworkId
    -> ApiAddressData
    -> Handler AnyAddress
postAnyAddress net addrData = do
    (addr, addrType) <- case addrData of
        AddrEnterprise spendingCred ->
            pure ( unAddress $
                     CA.paymentAddress discriminant (spendingFrom spendingCred)
                 , EnterpriseDelegating )
        AddrRewardAccount stakingCred -> do
            let (Right stakeAddr) =
                    CA.stakeAddress discriminant (stakingFrom stakingCred)
            pure ( unAddress stakeAddr, RewardAccount )
        AddrBase spendingCred stakingCred ->
            pure ( unAddress $ CA.delegationAddress discriminant
                     (spendingFrom spendingCred) (stakingFrom stakingCred)
                 , EnterpriseDelegating )
    pure $ AnyAddress addr addrType (fromInteger netTag)
  where
      toXPub = fromJust . CA.xpubFromBytes . pubToXPub
      pubToXPub bytes = BS.append bytes bytes
      netTag = case net of
          Cardano.Mainnet -> 1
          _ -> 0
      spendingFrom cred = case cred of
          CredentialPubKey  bytes ->
              CA.PaymentFromKey $ CA.liftXPub $ toXPub bytes
          CredentialScript  script' ->
              CA.PaymentFromScript $ CA.toScriptHash script'
      stakingFrom cred = case cred of
          CredentialPubKey bytes ->
              CA.DelegationFromKey $ CA.liftXPub $ toXPub bytes
          CredentialScript script' ->
              CA.DelegationFromScript $ CA.toScriptHash script'
      (Right discriminant) = CA.mkNetworkDiscriminant netTag
