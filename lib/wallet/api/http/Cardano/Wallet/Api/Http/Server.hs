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

module Cardano.Wallet.Api.Http.Server
    ( server
    ) where

import Prelude

import Cardano.Address
    ( unAddress
    )
import Cardano.Address.Script
    ( prettyErrValidateScript
    , validateScript
    )
import Cardano.Api
    ( NetworkId
    )
import Cardano.Pool.Metadata
    ( HealthCheckSMASH (NoSmashConfigured)
    , defaultManagerSettings
    , healthCheck
    , newManager
    , toHealthCheckSMASH
    )
import Cardano.Wallet
    ( ErrCreateRandomAddress (..)
    , ErrNotASequentialWallet (..)
    , genesisData
    , networkLayer
    , normalizeDelegationAddress
    , normalizeSharedAddress
    , utxoAssumptionsForWallet
    )
import Cardano.Wallet.Address.Derivation
    ( Role (..)
    , delegationAddressS
    , paymentAddressS
    )
import Cardano.Wallet.Address.Derivation.Icarus
    ( IcarusKey (..)
    )
import Cardano.Wallet.Address.Derivation.Shared
    ( SharedKey (..)
    )
import Cardano.Wallet.Address.Derivation.SharedKey
    ( constructAddressFromIx
    )
import Cardano.Wallet.Address.Derivation.Shelley
    ( ShelleyKey (..)
    )
import Cardano.Wallet.Address.Discovery.Random
    ( RndState
    )
import Cardano.Wallet.Address.Discovery.Sequential
    ( SeqState
    )
import Cardano.Wallet.Address.Discovery.Shared
    ( CredentialType (..)
    , SharedState
    )
import Cardano.Wallet.Address.Encoding
    ( inspectAddress
    )
import Cardano.Wallet.Api
    ( Addresses
    , Api
    , ApiLayer (..)
    , Assets
    , ByronAddresses
    , ByronAssets
    , ByronCoinSelections
    , ByronMigrations
    , ByronTransactions
    , ByronWallets
    , CoinSelections
    , Network
    , Proxy_
    , SMASH
    , Settings
    , SharedAddresses
    , SharedTransactions
    , SharedWalletKeys
    , SharedWallets
    , ShelleyMigrations
    , ShelleyTransactions
    , StakePools
    , WalletKeys
    , Wallets
    )
import Cardano.Wallet.Api.Http.Shelley.Server
    ( apiError
    , balanceTransaction
    , constructSharedTransaction
    , constructTransaction
    , createMigrationPlan
    , decodeSharedTransaction
    , decodeTransaction
    , delegationFee
    , deleteTransaction
    , deleteWallet
    , derivePublicKey
    , getAccountPublicKey
    , getAsset
    , getAssetDefault
    , getBlocksLatestHeader
    , getCurrentEpoch
    , getNetworkClock
    , getNetworkInformation
    , getNetworkParameters
    , getPolicyKey
    , getTransaction
    , getUTxOsStatistics
    , getWallet
    , getWalletUtxoSnapshot
    , idleWorker
    , joinStakePool
    , liftHandler
    , listAddresses
    , listAssets
    , listStakeKeys
    , listTransactions
    , listWallets
    , migrateWallet
    , mkLegacyWallet
    , mkSharedWallet
    , mkShelleyWallet
    , patchSharedWallet
    , postAccountPublicKey
    , postAccountWallet
    , postExternalTransaction
    , postIcarusWallet
    , postLedgerWallet
    , postPolicyId
    , postPolicyKey
    , postRandomAddress
    , postRandomWallet
    , postRandomWalletFromXPrv
    , postSharedWallet
    , postTransactionFeeOld
    , postTransactionOld
    , postTrezorWallet
    , postWallet
    , putByronWalletPassphrase
    , putRandomAddress
    , putRandomAddresses
    , putWallet
    , putWalletByron
    , putWalletPassphrase
    , quitStakePool
    , rndStateChange
    , selectCoins
    , selectCoinsForJoin
    , selectCoinsForQuit
    , signMetadata
    , signTransaction
    , submitSharedTransaction
    , submitTransaction
    , withLegacyLayer
    , withLegacyLayer'
    )
import Cardano.Wallet.Api.Types
    ( AnyAddress (..)
    , AnyAddressType (..)
    , ApiAccountKey (..)
    , ApiAccountKeyShared (..)
    , ApiAddressData (..)
    , ApiAddressDataPayload (..)
    , ApiAddressInspect (..)
    , ApiAddressInspectData (..)
    , ApiCredential (..)
    , ApiDelegationAction (..)
    , ApiHealthCheck (..)
    , ApiMaintenanceAction (..)
    , ApiMaintenanceActionPostData (..)
    , ApiPostAccountKeyData (..)
    , ApiPostAccountKeyDataWithPurpose (..)
    , ApiSelectCoinsAction (..)
    , ApiSelectCoinsData (..)
    , ApiT (..)
    , ApiVerificationKeyShared (..)
    , ApiVerificationKeyShelley (..)
    , ApiWalletMode (..)
    , ApiWithdrawalPostData (..)
    , MaintenanceAction (..)
    , SettingsPutData (..)
    , SomeByronWalletPostData (..)
    )
import Cardano.Wallet.Api.Types.BlockHeader
    ( ApiBlockHeader
    )
import Cardano.Wallet.Api.Types.Error
    ( ApiErrorInfo (..)
    )
import Cardano.Wallet.Api.Types.SchemaMetadata
    ( TxMetadataSchema (..)
    , parseSimpleMetadataFlag
    )
import Cardano.Wallet.Flavor
    ( WalletFlavorS (..)
    )
import Cardano.Wallet.Pools
    ( StakePoolLayer (..)
    )
import Cardano.Wallet.Primitive.Ledger.Shelley
    ( rewardAccountFromAddress
    )
import Cardano.Wallet.Primitive.NetworkId
    ( HasSNetworkId (..)
    , networkIdVal
    )
import Cardano.Wallet.Primitive.Types
    ( PoolMetadataSource (..)
    , SmashServer (..)
    , poolMetadataSource
    )
import Cardano.Wallet.Shelley.BlockchainSource
    ( BlockchainSource (..)
    )
import Control.Monad
    ( when
    )
import Control.Monad.IO.Class
    ( liftIO
    )
import Control.Monad.Trans.Except
    ( except
    , throwE
    , withExceptT
    )
import Data.Coerce
    ( coerce
    )
import Data.Generics.Internal.VL.Lens
    ( (^.)
    )
import Data.Generics.Labels
    ()
import Data.List
    ( sortOn
    )
import Data.Maybe
    ( fromJust
    )
import Data.Text.Class
    ( TextDecodingError (..)
    )
import Network.Ntp
    ( NtpClient
    )
import Servant
    ( (:<|>) (..)
    , Handler (..)
    , NoContent (..)
    , Server
    , err400
    )
import Servant.Server
    ( ServerError (..)
    )

import qualified Cardano.Address.Derivation as CA
import qualified Cardano.Address.Script as CA
import qualified Cardano.Address.Style.Shelley as CA
import qualified Cardano.Api as Cardano
import qualified Cardano.Wallet.Address.Derivation.Shared as Shared
import qualified Cardano.Wallet.Address.Derivation.Shelley as Shelley
import qualified Data.Text as T

server
    :: forall n
     . HasSNetworkId n
    => ApiLayer (RndState n)
    -> ApiLayer (SeqState n IcarusKey)
    -> ApiLayer (SeqState n ShelleyKey)
    -> ApiLayer (SharedState n SharedKey)
    -> StakePoolLayer
    -> NtpClient
    -> BlockchainSource
    -> Server (Api n)
server byron icarus shelley multisig spl ntp blockchainSource =
         wallets
    :<|> walletKeys
    :<|> assets
    :<|> addresses
    :<|> coinSelections
    :<|> shelleyTransactions
    :<|> shelleyMigrations
    :<|> stakePools
    :<|> byronWallets
    :<|> byronAssets
    :<|> byronAddresses
    :<|> byronCoinSelections
    :<|> byronTransactions
    :<|> byronMigrations
    :<|> network' (networkIdVal (sNetworkId @n))
    :<|> proxy
    :<|> settingS
    :<|> smash
    :<|> sharedWallets multisig
    :<|> sharedWalletKeys multisig
    :<|> sharedAddresses multisig
    :<|> sharedTransactions multisig
    :<|> blocks
  where
    wallets :: Server Wallets
    wallets = deleteWallet shelley
        :<|> (fmap fst . getWallet shelley mkShelleyWallet)
        :<|> (fmap fst <$> listWallets shelley mkShelleyWallet)
        :<|> postWallet shelley Shelley.generateKeyFromSeed ShelleyKey
        :<|> putWallet shelley mkShelleyWallet
        :<|> putWalletPassphrase
                shelley Shelley.generateKeyFromSeed Shelley.getKey
        :<|> getWalletUtxoSnapshot shelley
        :<|> getUTxOsStatistics shelley

    walletKeys :: Server WalletKeys
    walletKeys = derivePublicKey shelley ApiVerificationKeyShelley
        :<|> signMetadata shelley
        :<|> postAccountPublicKey shelley ApiAccountKey
        :<|> getAccountPublicKey shelley ApiAccountKey
        :<|> getPolicyKey shelley
        :<|> postPolicyKey shelley
        :<|> postPolicyId shelley

    assets :: Server Assets
    assets =
        listAssets shelley
        :<|> getAsset shelley
        :<|> getAssetDefault shelley

    addresses :: Server (Addresses n)
    addresses = listAddresses shelley (normalizeDelegationAddress @_ @ShelleyKey @n)
        :<|> (handler ApiAddressInspect . inspectAddress . unApiAddressInspectData)
        :<|> (handler id . postAnyAddress (networkIdVal (sNetworkId @n)))
      where
        toServerError :: TextDecodingError -> ServerError
        toServerError = apiError err400 BadRequest . T.pack . getTextDecodingError

        handler :: (a -> result) -> Either TextDecodingError a -> Handler result
        handler transform =
            Handler . withExceptT toServerError . except . fmap transform

    -- Hlint doesn't seem to care about inlining properties:
    --   https://github.com/quchen/articles/blob/master/fbut.md#f-x---is-not-f--x---
    {- HLINT ignore "Redundant lambda" -}
    coinSelections :: Server (CoinSelections n)
    coinSelections = (\wid -> \case
        ApiSelectForPayment ascp ->
            selectCoins shelley (delegationAddressS @n) wid ascp
        ApiSelectForDelegation (ApiSelectCoinsAction action) ->
            case action of
                Join pid ->
                    selectCoinsForJoin
                        shelley
                        (knownPools spl)
                        (getPoolLifeCycleStatus spl)
                        (getApiT pid)
                        (getApiT wid)
                Quit ->
                    selectCoinsForQuit shelley wid
        )

    shelleyTransactions :: Server (ShelleyTransactions n)
    shelleyTransactions =
             constructTransaction shelley
                (delegationAddressS @n)
                (knownPools spl)
                (getPoolLifeCycleStatus spl)
        :<|> signTransaction shelley
        :<|>
            (\wid mMinWithdrawal mStart mEnd mOrder mLimit mAddress simpleMetadataFlag ->
                listTransactions shelley wid mMinWithdrawal mStart mEnd mOrder mLimit
                    mAddress (parseSimpleMetadataFlag simpleMetadataFlag)
            )
        :<|>
            (\wid txId simpleMetadataFlag ->
                getTransaction shelley wid txId
                    (parseSimpleMetadataFlag simpleMetadataFlag)
            )
        :<|> deleteTransaction shelley
        :<|> postTransactionOld shelley (delegationAddressS @n)
        :<|> postTransactionFeeOld shelley
        :<|> balanceTransaction
            shelley
            (delegationAddressS @n)
            (utxoAssumptionsForWallet ShelleyWallet)
            mempty
        :<|> decodeTransaction shelley
        :<|> submitTransaction @_ @_ @_ @n shelley

    shelleyMigrations :: Server (ShelleyMigrations n)
    shelleyMigrations =
             createMigrationPlan shelley (Just SelfWithdrawal)
        :<|> migrateWallet shelley (Just SelfWithdrawal)

    stakePools :: Server (StakePools n)
    stakePools =
             listStakePools_
        :<|> joinStakePool shelley
            (delegationAddressS @n) (knownPools spl) (getPoolLifeCycleStatus spl)
        :<|> quitStakePool shelley (delegationAddressS @n)
        :<|> (postPoolMaintenance :<|> getPoolMaintenance)
        :<|> delegationFee shelley
        :<|> listStakeKeys rewardAccountFromAddress shelley
      where
        listStakePools_ = \case
            Just (ApiT stake) -> do
                currentEpoch <- getCurrentEpoch shelley
                liftIO $ fmap ApiT <$> listStakePools spl currentEpoch stake
            Nothing -> Handler $ throwE $ apiError err400 QueryParamMissing $
                mconcat
                [ "The stake intended to delegate must be provided as a query "
                , "parameter as it affects the rewards and ranking."
                ]

        postPoolMaintenance action' = do
            case action' of
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
                (byron , putWalletByron byron mkLegacyWallet wid name)
                (icarus, putWalletByron icarus mkLegacyWallet wid name)
             )
        :<|> (\wid -> withLegacyLayer wid
                (byron , getWalletUtxoSnapshot byron wid)
                (icarus, getWalletUtxoSnapshot icarus wid)
             )
        :<|> (\wid -> withLegacyLayer wid
                (byron , getUTxOsStatistics byron wid)
                (icarus, getUTxOsStatistics icarus wid)
             )
        :<|> (\wid pwd -> withLegacyLayer wid
                (byron , putByronWalletPassphrase byron wid pwd)
                (icarus, putByronWalletPassphrase icarus wid pwd)
             )

    byronAssets :: Server ByronAssets
    byronAssets =
            (\wid -> withLegacyLayer wid
                (byron, listAssets byron wid)
                (icarus, listAssets icarus wid)
            )
        :<|> (\wid t n -> withLegacyLayer wid
                (byron, getAsset byron wid t n)
                (icarus, getAsset icarus wid t n)
            )
        :<|> (\wid t -> withLegacyLayer wid
                (byron, getAssetDefault byron wid t)
                (icarus, getAssetDefault icarus wid t)
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
        genChangeSequential paymentK _ = paymentAddressS @n paymentK
    byronCoinSelections _ _ = Handler
        $ throwE
        $ apiError err400 InvalidWalletType
        "Byron wallets don't have delegation capabilities."

    byronTransactions :: Server (ByronTransactions n)
    byronTransactions =
            (\wid r0 r1 s l a -> withLegacyLayer wid
                ( byron
                , listTransactions
                    byron wid Nothing r0 r1 s l a TxMetadataDetailedSchema
                )
                ( icarus
                , listTransactions
                    icarus wid Nothing r0 r1 s l a TxMetadataDetailedSchema
                )
             )
        :<|>
            (\wid txid -> withLegacyLayer wid
                ( byron
                , getTransaction byron wid txid TxMetadataDetailedSchema
                )
                ( icarus
                , getTransaction icarus wid txid TxMetadataDetailedSchema
                )
             )
        :<|> (\wid txid -> withLegacyLayer wid
                (byron , deleteTransaction byron wid txid)
                (icarus, deleteTransaction icarus wid txid)
             )
        :<|> (\wid tx -> withLegacyLayer wid
                 (byron, do
                    let pwd = coerce (getApiT $ tx ^. #passphrase)
                    genChange <- rndStateChange byron wid pwd
                    postTransactionOld byron genChange wid tx

                 )
                 (icarus, do
                    let genChange k _ = paymentAddressS @n k
                    postTransactionOld icarus genChange wid tx
                 )
             )
       :<|> (\wid tx -> withLegacyLayer wid
                (byron, postTransactionFeeOld byron wid tx)
                (icarus, postTransactionFeeOld icarus wid tx)
            )

    byronMigrations :: Server (ByronMigrations n)
    byronMigrations =
             (\wid postData -> withLegacyLayer wid
                (byron , createMigrationPlan byron Nothing wid postData)
                (icarus, createMigrationPlan icarus Nothing wid postData)
             )
        :<|> (\wid m -> withLegacyLayer wid
                (byron , migrateWallet byron Nothing wid m)
                (icarus, migrateWallet icarus Nothing wid m)
             )

    network' :: NetworkId -> Server Network
    network' nid =
        getNetworkInformation nid nl mode
        :<|> getNetworkParameters genesis nl
        :<|> getNetworkClock ntp
      where
        nl = icarus ^. networkLayer
        genesis@(_,_) = icarus ^. genesisData
        mode = case blockchainSource of
          NodeSource {} -> Node

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

    sharedWallets
        :: ApiLayer (SharedState n SharedKey)
        -> Server SharedWallets
    sharedWallets apilayer =
             postSharedWallet @_ @_ @SharedKey apilayer Shared.generateKeyFromSeed SharedKey
        :<|> (fmap fst . getWallet apilayer mkSharedWallet)
        :<|> putWallet apilayer mkSharedWallet
        :<|> (fmap fst <$> listWallets apilayer mkSharedWallet)
        :<|> patchSharedWallet @_ @_ @SharedKey apilayer SharedKey Payment
        :<|> patchSharedWallet @_ @_ @SharedKey apilayer SharedKey Delegation
        :<|> deleteWallet apilayer
        :<|> getUTxOsStatistics apilayer
        :<|> getWalletUtxoSnapshot apilayer

    sharedWalletKeys
        :: ApiLayer (SharedState n SharedKey)
        -> Server SharedWalletKeys
    sharedWalletKeys apilayer = derivePublicKey apilayer ApiVerificationKeyShared
        :<|> (\wid ix p -> postAccountPublicKey apilayer ApiAccountKeyShared wid ix (toKeyDataPurpose p) )
        :<|> getAccountPublicKey apilayer ApiAccountKeyShared
      where
          toKeyDataPurpose :: ApiPostAccountKeyData -> ApiPostAccountKeyDataWithPurpose
          toKeyDataPurpose (ApiPostAccountKeyData p f) =
              ApiPostAccountKeyDataWithPurpose p f Nothing

    sharedAddresses
        :: ApiLayer (SharedState n SharedKey)
        -> Server (SharedAddresses n)
    sharedAddresses apilayer =
        listAddresses apilayer normalizeSharedAddress

    sharedTransactions
        :: ApiLayer (SharedState n SharedKey)
        -> Server (SharedTransactions n)
    sharedTransactions apilayer =
        constructSharedTransaction apilayer (constructAddressFromIx @n UtxoInternal)
            (knownPools spl) (getPoolLifeCycleStatus spl)
        :<|> signTransaction apilayer
        :<|> decodeSharedTransaction apilayer
        :<|> submitSharedTransaction apilayer
        :<|>
            (\wid txId simpleMetadataFlag ->
                getTransaction apilayer wid txId
                    (parseSimpleMetadataFlag simpleMetadataFlag)
            )
        :<|>
            (\wid mMinWithdrawal mStart mEnd mOrder mLimit mAddress simpleMetadataFlag ->
                listTransactions apilayer wid mMinWithdrawal mStart mEnd mOrder
                    mLimit mAddress (parseSimpleMetadataFlag simpleMetadataFlag)
            )

    blocks :: Handler ApiBlockHeader
    blocks = getBlocksLatestHeader (shelley ^. networkLayer)

postAnyAddress
    :: NetworkId
    -> ApiAddressData
    -> Either TextDecodingError AnyAddress
postAnyAddress net addrData = do
    (addr, addrType) <- case addrData of
        (ApiAddressData (AddrEnterprise spendingCred) validation') -> do
            guardValidation validation' spendingCred
            pure ( unAddress $
                     CA.paymentAddress discriminant (spendingFrom spendingCred)
                 , EnterpriseDelegating )
        (ApiAddressData (AddrRewardAccount stakingCred) validation') -> do
            case CA.stakeAddress discriminant (stakingFrom stakingCred) of
                Right stakeAddr -> do
                    guardValidation validation' stakingCred
                    pure ( unAddress stakeAddr, RewardAccount )
                Left e -> error $ show e
        (ApiAddressData (AddrBase spendingCred stakingCred) validation') -> do
            guardValidation validation' spendingCred
            guardValidation validation' stakingCred
            pure ( unAddress $ CA.delegationAddress discriminant
                     (spendingFrom spendingCred) (stakingFrom stakingCred)
                 , EnterpriseDelegating )
    pure $ AnyAddress addr addrType (fromInteger netTag)
  where
      fromXPub = fromJust . CA.xpubFromBytes
      fromPub = fromJust . CA.pubFromBytes
      netTag = case net of
          Cardano.Mainnet -> 1
          _ -> 0
      spendingFrom cred = case cred of
          CredentialPubKey bytes ->
              CA.PaymentFromKey $ CA.liftPub $ fromPub bytes
          CredentialKeyHash bytes ->
              CA.PaymentFromKeyHash $ CA.KeyHash CA.Payment bytes
          CredentialExtendedPubKey bytes ->
              CA.PaymentFromExtendedKey $ CA.liftXPub $ fromXPub bytes
          CredentialScript script' ->
              CA.PaymentFromScript script'
          CredentialScriptHash scriptHash ->
              CA.PaymentFromScriptHash scriptHash
      stakingFrom cred = case cred of
          CredentialPubKey bytes ->
              CA.DelegationFromKey $ CA.liftPub $ fromPub bytes
          CredentialKeyHash bytes ->
              CA.DelegationFromKeyHash $ CA.KeyHash CA.Delegation bytes
          CredentialExtendedPubKey bytes ->
              CA.DelegationFromExtendedKey $ CA.liftXPub $ fromXPub bytes
          CredentialScript script' ->
              CA.DelegationFromScript script'
          CredentialScriptHash scriptHash ->
              CA.DelegationFromScriptHash scriptHash
      guardValidation v cred =
            when (fst $ checkValidation v cred) $
                Left $ snd $ checkValidation v cred
      checkValidation v cred = case cred of
          CredentialPubKey _ -> (False, TextDecodingError "")
          CredentialKeyHash _ -> (False, TextDecodingError "")
          CredentialExtendedPubKey _ -> (False, TextDecodingError "")
          CredentialScriptHash _ -> (False, TextDecodingError "")
          CredentialScript script' -> case v of
              Just (ApiT v') ->
                  case validateScript v' script' of
                      Left err -> (True, TextDecodingError $ prettyErrValidateScript err)
                      Right _ -> (False, TextDecodingError "")
              _ -> (False, TextDecodingError "")
      discriminant =
        case CA.mkNetworkDiscriminant netTag of
            Right d -> d
            Left e -> error $ show e
