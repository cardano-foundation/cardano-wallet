{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Integration.Scenario.API.Shelley.StakePools (spec) where

import Prelude hiding
    ( id
    )

import Cardano.Mnemonic
    ( mnemonicToText
    )
import Cardano.Pool.Metadata
    ( HealthCheckSMASH (..)
    )
import Cardano.Pool.Metadata.Types
    ( PoolMetadataGCStatus (NotApplicable)
    , StakePoolMetadata (StakePoolMetadata, description, homepage, name, ticker)
    )
import Cardano.Pool.Types
    ( PoolId (PoolId)
    , StakePoolTicker (StakePoolTicker)
    , decodePoolIdBech32
    )
import Cardano.Wallet.Api.Types
    ( ApiCertificate (JoinPool, QuitPool, RegisterRewardAccount)
    , ApiEra (..)
    , ApiHealthCheck
    , ApiPoolSpecifier (..)
    , ApiStakeKeys
    , ApiT (..)
    , ApiTransaction
    , ApiTxId (..)
    , ApiTxInput (..)
    , ApiWallet
    , ApiWalletDelegationStatus (..)
    , ApiWithdrawal (..)
    , WalletStyle (..)
    )
import Cardano.Wallet.Faucet.Mnemonics
    ( preregKeyWallet
    )
import Cardano.Wallet.Pools
    ( StakePool (..)
    , StakePoolFlag (Delisted)
    )
import Cardano.Wallet.Primitive.NetworkId
    ( HasSNetworkId
    )
import Cardano.Wallet.Primitive.Types
    ( FeePolicy (..)
    , LinearFunction (..)
    , PoolMetadataSource (..)
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..)
    )
import Cardano.Wallet.Primitive.Types.Tx.Constraints
    ( TxSize (..)
    )
import Cardano.Wallet.Primitive.Types.Tx.TxMeta
    ( Direction (..)
    , TxStatus (..)
    )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex
    , unsafeMkPercentage
    )
import Control.Monad
    ( forM_
    )
import Control.Monad.IO.Class
    ( liftIO
    )
import Control.Monad.Trans.Resource
    ( runResourceT
    )
import Data.Function
    ( (&)
    )
import Data.Generics.Internal.VL.Lens
    ( view
    , (^.)
    )
import Data.IORef
    ( readIORef
    )
import Data.List
    ( find
    , sortOn
    )
import Data.List.NonEmpty
    ( NonEmpty (..)
    )
import Data.Maybe
    ( fromMaybe
    , isJust
    , isNothing
    , listToMaybe
    , mapMaybe
    )
import Data.Ord
    ( Down (..)
    )
import Data.Quantity
    ( Quantity (..)
    )
import Data.Set
    ( Set
    )
import Data.Text.Class
    ( showT
    , toText
    )
import Numeric.Natural
    ( Natural
    )
import Test.Hspec
    ( SpecWith
    , describe
    , pendingWith
    )
import Test.Hspec.Expectations.Lifted
    ( expectationFailure
    , shouldBe
    , shouldSatisfy
    )
import Test.Hspec.Extra
    ( it
    )
import Test.Integration.Framework.Context
    ( Context (..)
    , PoolGarbageCollectionEvent (..)
    )
import Test.Integration.Framework.DSL
    ( Headers (..)
    , Payload (..)
    , arbitraryStake
    , bracketSettings
    , delegating
    , delegationFee
    , emptyWallet
    , eventually
    , eventuallyUsingDelay
    , expectErrorMessage
    , expectField
    , expectListField
    , expectListSize
    , expectResponseCode
    , fixturePassphrase
    , fixtureWallet
    , fixtureWalletWith
    , getFromResponse
    , getResponse
    , getRetirementEpoch
    , getSlotParams
    , joinStakePool
    , joinStakePoolUnsigned
    , json
    , listAddresses
    , minUTxOValue
    , notDelegating
    , notRetiringPools
    , postWallet
    , quitStakePool
    , quitStakePoolUnsigned
    , replaceStakeKey
    , request
    , rewardWallet
    , triggerMaintenanceAction
    , unsafeRequest
    , unsafeResponse
    , updateMetadataSource
    , verify
    , verifyMaintenanceAction
    , verifyMetadataSource
    , waitForEpoch
    , waitForNextEpoch
    , waitForTxImmutability
    , waitForTxStatus
    , waitNumberOfEpochBoundaries
    , walletId
    , (.<)
    , (.>)
    , (.>=)
    )
import Test.Integration.Framework.TestData
    ( errMsg403EmptyUTxO
    , errMsg403Fee
    , errMsg403NotDelegating
    , errMsg403PoolAlreadyJoined
    , errMsg403WrongPass
    , errMsg404NoSuchPool
    , errMsg404NoWallet
    )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Data.ByteString as BS
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Network.HTTP.Types.Status as HTTP
import qualified Prelude

spec :: forall n. HasSNetworkId n => SpecWith Context
spec = describe "SHELLEY_STAKE_POOLS" $ do
    let listPools ctx stake =
            request @[ApiT StakePool] ctx (Link.listStakePools stake) Default Empty
                & (fmap . fmap . fmap . fmap) getApiT

    it "STAKE_POOLS_MAINTENANCE_01 - \
        \trigger GC action when metadata source = direct" $ \ctx -> runResourceT $ bracketSettings ctx $ do
        updateMetadataSource ctx "direct"
        verifyMetadataSource ctx FetchDirect
        triggerMaintenanceAction ctx "gc_stake_pools"
        let delay = 500 * 1_000
            timeout = 10
        eventuallyUsingDelay delay timeout "GC Status shows as NotApplicable" $ do
          verifyMaintenanceAction ctx NotApplicable

    it "STAKE_POOLS_MAINTENANCE_02 - \
        \trigger GC action when metadata source = none" $ \ctx -> runResourceT $ bracketSettings ctx $ do
        updateMetadataSource ctx "none"
        verifyMetadataSource ctx FetchNone
        triggerMaintenanceAction ctx "gc_stake_pools"
        let delay = 500 * 1_000
            timeout = 10
        eventuallyUsingDelay delay timeout "GC Status shows as NotApplicable" $ do
          verifyMaintenanceAction ctx NotApplicable

    it "STAKE_POOLS_JOIN_01 - Cannot join non-existent wallet" $ \ctx -> runResourceT $ do
        w <- emptyWallet ctx
        let wid = w ^. walletId
        _ <- request @ApiWallet ctx
            (Link.deleteWallet @'Shelley w) Default Empty
        let poolIdAbsent = PoolId $ BS.pack $ replicate 32 1
        r <- joinStakePool @n ctx (SpecificPool poolIdAbsent) (w, fixturePassphrase)
        expectResponseCode HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet wid) r

    it "STAKE_POOLS_JOIN_01 - Cannot join non-existent stakepool" $ \ctx -> runResourceT $ do
        w <- fixtureWallet ctx
        let poolIdAbsent = PoolId $ BS.pack $ replicate 32 1
        r <- joinStakePool @n ctx (SpecificPool  poolIdAbsent) (w, fixturePassphrase)
        expectResponseCode HTTP.status404 r
        expectErrorMessage (errMsg404NoSuchPool (toText poolIdAbsent)) r

    it "STAKE_POOLS_JOIN_01 - \
        \Cannot join existent stakepool with wrong password" $ \ctx -> runResourceT $ do
        w <- fixtureWallet ctx
        pool : _ <- map (view #id . getApiT) . snd <$>
            unsafeRequest @[ApiT StakePool]
            ctx (Link.listStakePools arbitraryStake) Empty
        joinStakePool @n ctx (SpecificPool  pool) (w, "Wrong Passphrase")
            >>= flip verify
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403WrongPass
            ]

    it "STAKE_POOLS_JOIN_01rewards - \
        \Can join a pool, earn rewards and collect them" $ \ctx -> runResourceT $ do
        src <- fixtureWallet ctx
        dest <- emptyWallet ctx
        let deposit = depositAmt ctx
        pool : _  <- map (view #id) <$> notRetiringPools ctx

        -- Join Pool
        rJoin <- joinStakePool @n ctx (SpecificPool pool) (src, fixturePassphrase)
        verify rJoin
            [ expectResponseCode HTTP.status202
            , expectField (#status . #getApiT) (`shouldBe` Pending)
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            , expectField #depositTaken (`shouldBe` Quantity deposit)
            , expectField #inputs $ \inputs' -> do
                inputs' `shouldSatisfy` all (isJust . source)
            ]
        eventually "Wallet has joined pool and deposit info persists" $ do
            let endpoint = Link.getTransaction @'Shelley src (getResponse rJoin)
            request @(ApiTransaction n) ctx endpoint Default Empty
                >>= flip verify
                [ expectResponseCode HTTP.status200
                , expectField (#status . #getApiT) (`shouldBe` InLedger)
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectField #depositTaken (`shouldBe` Quantity deposit)
                , expectField #depositReturned (`shouldBe` Quantity 0)
                ]

        let txId = getFromResponse #id rJoin
        let link = Link.getTransaction @'Shelley src (ApiTxId txId)
        eventually "delegation transaction is in ledger" $ do
            rSrc <- request @(ApiTransaction n) ctx link Default Empty
            verify rSrc
                [ expectResponseCode HTTP.status200
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectField (#status . #getApiT) (`shouldBe` InLedger)
                , expectField #metadata (`shouldBe` Nothing)
                , expectField #depositTaken (`shouldBe` Quantity deposit)
                , expectField #depositReturned (`shouldBe` Quantity 0)
                , expectField (#fee . #getQuantity) (`shouldSatisfy` (> 0))
                , expectField #inputs $ \inputs' ->
                    inputs' `shouldSatisfy` all (isJust . source)
                ]

        -- Epoch A: delegation tx is in the ledger.
        -- Epoch A+1: stake is registered to a chosen pool.
        -- Epoch A+2: stake is active, rewards start accumulating.
        -- Epoch A+3: rewards from epoch A+2 are calculated.
        -- Epoch A+4: rewards from epoch A+2 are paid out.
        waitNumberOfEpochBoundaries 4 ctx

        (previousBalance, walletRewards) <- eventually "Wallet gets rewards" $ do
            let endpoint = Link.getWallet @'Shelley src
            r <- request @ApiWallet ctx endpoint Default Empty
            verify r [ expectField (#balance . #reward) (.> Quantity 0) ]
            pure
                ( getFromResponse (#balance . #available) r
                , getFromResponse (#balance . #reward) r
                )

        -- Try to use rewards
        addrs <- listAddresses @n ctx dest
        let coin = minUTxOValue (_mainEra ctx) :: Natural
        let addr = (addrs !! 1) ^. #id
        let payload = [json|
                { "payments":
                    [ { "address": #{addr}
                      , "amount":
                        { "quantity": #{coin}
                        , "unit": "lovelace"
                        }
                      }
                    ]
                , "passphrase": #{fixturePassphrase}
                }|]

        -- cannot use rewards by default
        r1 <- request @(ApiTransaction n) ctx
            (Link.createTransactionOld @'Shelley src)
            Default (Json payload)
        expectResponseCode HTTP.status202 r1
        eventually "Wallet has not consumed rewards" $ do
          let linkSrc = Link.getTransaction @'Shelley
                  src (getFromResponse Prelude.id r1)
          request @(ApiTransaction n) ctx linkSrc Default Empty
              >>= flip verify
                  [ expectField
                      (#status . #getApiT) (`shouldBe` InLedger)
                  ]
          request @ApiWallet ctx (Link.getWallet @'Shelley src) Default Empty
              >>= flip verify
                  [ expectField
                      (#balance . #reward) (`shouldBe` walletRewards)
                  ]

        -- Listing stake keys shows
        request @(ApiStakeKeys n) ctx (Link.listStakeKeys src) Default Empty
            >>= flip verify
            [ expectField (#_foreign) (`shouldBe` [])
            , expectField (#_ours) (\case
                [acc] -> do
                    (acc ^. #_stake) .> Quantity 0
                    acc ^. (#_delegation . #active . #status)
                        `shouldBe` Delegating
                    acc ^. (#_delegation . #active . #target)
                        `shouldBe` (Just (ApiT pool))
                _ -> expectationFailure "wrong number of accounts in \"ours\""
                )
            , expectField (#_none . #_stake) (.> Quantity 0)
            ]

        -- there's currently no withdrawals in the wallet
        rw1 <- request @[ApiTransaction n] ctx
            (Link.listTransactions' @'Shelley src (Just 1)
                Nothing Nothing Nothing Nothing Nothing)
            Default Empty
        verify rw1 [ expectListSize 0 ]

        -- can use rewards with an explicit withdrawal request to self.
        let payloadWithdrawal = [json|
                { "payments":
                    [ { "address": #{addr}
                      , "amount":
                        { "quantity": #{coin}
                        , "unit": "lovelace"
                        }
                      }
                    ]
                , "passphrase": #{fixturePassphrase},
                  "withdrawal": "self"
                }|]

        waitForNextEpoch ctx
        rTx <- request @(ApiTransaction n) ctx
            (Link.createTransactionOld @'Shelley src)
            Default (Json payloadWithdrawal)
        verify rTx
            [ expectField #amount (.> (Quantity coin))
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            ]
        let txAmount = getFromResponse #amount rTx

        -- Rewards are have been consumed.
        eventually "Wallet has consumed rewards" $ do
            request @ApiWallet ctx (Link.getWallet @'Shelley src) Default Empty
                >>= flip verify
                    [ expectField
                        (#balance . #reward)
                        (`shouldBe` (Quantity 0))
                    , expectField
                        (#balance . #available)
                        (.> previousBalance)
                    ]

        eventually "There's at least one outgoing transaction with a withdrawal" $ do
            rWithdrawal <- request @(ApiTransaction n) ctx
                (Link.getTransaction @'Shelley src
                    (getFromResponse Prelude.id rTx))
                Default Empty
            verify rWithdrawal
                [ expectResponseCode HTTP.status200
                , expectField #withdrawals (`shouldSatisfy` (not . null))
                , expectField #amount (`shouldBe` txAmount)
                ]
            rw2 <- request @[ApiTransaction n] ctx
                (Link.listTransactions' @'Shelley src (Just 1)
                    Nothing Nothing Nothing Nothing Nothing)
                Default Empty
            verify rw2 [ expectListSize 1 ]

        eventually "There's one incoming transaction with correct amount" $ do
            request @[ApiTransaction n] ctx (Link.listTransactions @'Shelley dest)
                Default Empty >>= flip verify
                [ expectListSize 2
                , expectListField 0 (#amount . #getQuantity) (`shouldBe` coin)
                , expectListField 1 (#amount . #getQuantity) (`shouldBe` coin)
                ]

        -- Quit delegation altogether.
        rq <- quitStakePool @n ctx (src, fixturePassphrase)
        verify rq
            -- pending tx for quitting pool is initially outgoing
            -- because there is a fee for this tx
            [ expectResponseCode HTTP.status202
            , expectField (#status . #getApiT) (`shouldBe` Pending)
            , expectField (#direction . #getApiT) (`shouldBe` Incoming)
            , expectField #depositTaken (`shouldBe` Quantity 0)
            , expectField #depositReturned (`shouldBe` Quantity deposit)
            , expectField (#fee . #getQuantity) (`shouldSatisfy` (> 0))
            ]
        let txid = getFromResponse Prelude.id rq
        let quitFeeAmt = getFromResponse #amount rq

        eventually "Certificates are inserted after quitting a pool" $ do
            let epg = Link.getTransaction @'Shelley src txid
            rlg <- request @(ApiTransaction n) ctx epg Default Empty
            verify rlg
                [ expectField
                    (#direction . #getApiT) (`shouldBe` Incoming)
                , expectField
                    #amount (`shouldBe` quitFeeAmt)
                , expectField
                    (#status . #getApiT) (`shouldBe` InLedger)
                , expectField #depositTaken (`shouldBe` Quantity 0)
                , expectField #depositReturned (`shouldBe` Quantity deposit)
                , expectField (#fee . #getQuantity) (`shouldSatisfy` (> 0))
                ]

            let epl = Link.listTransactions @'Shelley src
            rlq <- request @[ApiTransaction n] ctx epl Default Empty
            verify rlq
                [ expectListField 0
                    (#direction . #getApiT) (`shouldBe` Incoming)
                , expectListField 0
                    #amount (`shouldBe` quitFeeAmt)
                , expectListField 0
                    (#status . #getApiT) (`shouldBe` InLedger)
                , expectListField 1
                    (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectListField 1
                    (#status . #getApiT) (`shouldBe` InLedger)
                , expectListField 2
                    (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectListField 2
                    (#status . #getApiT) (`shouldBe` InLedger)
                ]

    it "STAKE_POOLS_JOIN_02 - \
        \Cannot join already joined stake pool" $ \ctx -> runResourceT $ do
        w <- fixtureWallet ctx
        pool : _  <- map (view #id) <$> notRetiringPools ctx

        waitForTxStatus ctx w InLedger . getResponse =<<
            joinStakePool @n ctx (SpecificPool pool) (w, fixturePassphrase)

        joinStakePool @n ctx (SpecificPool pool) (w, fixturePassphrase)
            >>= flip verify
            [ expectResponseCode HTTP.status403
            , expectErrorMessage (errMsg403PoolAlreadyJoined $ toText pool)
            ]

    it "STAKE_POOLS_JOIN_03 - Cannot join a pool that has retired" $ \ctx -> runResourceT $ do
        waitForEpoch 3 ctx -- One pool retires at epoch 3
        response <- listPools ctx arbitraryStake
        verify response [ expectListSize 3 ]
        let nonRetiredPoolIds = Set.fromList (view #id <$> getResponse response)
        let reportError = error $ unlines
                [ "Unable to find a retired pool ID."
                , "Test cluster pools:"
                , unlines (showT <$> Set.toList testClusterPoolIds)
                , "Non-retired pools:"
                , unlines (showT <$> Set.toList nonRetiredPoolIds)
                ]
        let retiredPoolIds =
                testClusterPoolIds `Set.difference` nonRetiredPoolIds
        let retiredPoolId =
                fromMaybe reportError $ listToMaybe $ Set.toList retiredPoolIds
        w <- fixtureWallet ctx
        r <- joinStakePool @n ctx (SpecificPool retiredPoolId) (w, fixturePassphrase)
        expectResponseCode HTTP.status404 r
        expectErrorMessage (errMsg404NoSuchPool (toText retiredPoolId)) r

    it "STAKE_POOLS_QUIT_02 - Passphrase must be correct to quit" $ \ctx -> runResourceT $ do
        w <- fixtureWallet ctx
        pool : _  <- map (view #id) <$> notRetiringPools ctx

        waitForTxStatus ctx w InLedger . getResponse =<<
            joinStakePool @n ctx (SpecificPool pool) (w, fixturePassphrase)

        let wrongPassphrase = "Incorrect Passphrase"
        quitStakePool @n ctx (w, wrongPassphrase) >>= flip verify
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403WrongPass
            ]

    it "STAKE_POOL_NEXT_02/STAKE_POOLS_QUIT_01 - \
        \Cannot quit when active: not_delegating"
        $ \ctx -> runResourceT $ do
        w <- fixtureWallet ctx
        quitStakePool @n ctx (w, fixturePassphrase) >>= flip verify
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403NotDelegating
            ]

    it "STAKE_POOLS_QUIT_03 - Can quit with rewards"
        $ \ctx -> runResourceT $ do
        (w, _) <- rewardWallet ctx

        pool:_:_ <- map (view #id . getApiT) . snd
            <$> unsafeRequest @[ApiT StakePool]
                ctx (Link.listStakePools arbitraryStake) Empty
        joinStakePool @n ctx (SpecificPool pool) (w, fixturePassphrase) >>= flip verify
            [ expectResponseCode HTTP.status202
            , expectField #depositTaken (`shouldBe` Quantity 0)
            , expectField #depositReturned (`shouldBe` Quantity 0)
            ]
        waitForTxImmutability ctx
        quitStakePool @n ctx (w, fixturePassphrase) >>= flip verify
            [ expectResponseCode HTTP.status202
            , expectField #depositTaken (`shouldBe` Quantity 0)
            , expectField #depositReturned (`shouldBe` Quantity 1_000_000)
            ]

    it "STAKE_POOLS_JOIN_01 - Can rejoin another stakepool" $ \ctx -> runResourceT $ do
        w <- fixtureWallet ctx

        -- make sure we are at the beginning of new epoch
        waitForNextEpoch ctx
        (currentEpoch, _) <- getSlotParams ctx

        pool1 : pool2 : _  <- map (view #id) <$> notRetiringPools ctx

        waitForTxStatus ctx w InLedger . getResponse =<<
            joinStakePool @n ctx (SpecificPool pool1) (w, fixturePassphrase)

        request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty
            >>= flip verify
            [ expectField (#delegation . #next) $ \case
                [dlg] -> do
                    (dlg ^. #status) `shouldBe` Delegating
                    (dlg ^. #target) `shouldBe` Just (ApiT pool1)
                    (view #epochNumber <$> dlg ^. #changesAt) `shouldBe`
                        Just (currentEpoch + 2)
                _ -> fail "next delegation should contain exactly one element"
            ]

        -- Epoch A: delegation tx happened.
        -- Epoch A+1: stake is registered to a chosen pool.
        -- Epoch A+2: stake is active, rewards start accumulating.
        waitNumberOfEpochBoundaries 2 ctx

        request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty
            >>= flip verify
                [expectField #delegation (`shouldBe` delegating (ApiT pool1) [])]

        -- join another stake pool
        waitForTxStatus ctx w InLedger . getResponse =<<
            joinStakePool @n ctx (SpecificPool pool2) (w, fixturePassphrase)

        waitNumberOfEpochBoundaries 2 ctx

        eventually "Wallet is now delegating to pool2" $ do
            request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty
                >>= flip verify
                [expectField #delegation (`shouldBe` delegating (ApiT pool2) [])]

    it "STAKE_POOLS_JOIN_04 - Rewards accumulate" $ \ctx -> runResourceT $ do
        w <- fixtureWallet ctx
        pool : _  <- map (view #id) <$> notRetiringPools ctx

        waitForTxStatus ctx w InLedger . getResponse =<<
            joinStakePool @n ctx (SpecificPool pool) (w, fixturePassphrase)

        -- Epoch A: delegation tx happened.
        -- Epoch A+1: stake is registered to a chosen pool.
        -- Epoch A+2: stake is active, rewards start accumulating.
        -- Epoch A+3: rewards from epoch A+2 are calculated.
        -- Epoch A+4: rewards from epoch A+2 are paid out.
        waitNumberOfEpochBoundaries 4 ctx

        eventually "Rewards are visible" $ do
            request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty
                >>= flip verify
                    [ expectField (#balance . #reward) (.> Quantity 0) ]

        -- Can quit with rewards
        quitStakePool @n ctx (w, fixturePassphrase) >>= flip verify
            [ expectResponseCode HTTP.status202
            , expectField #depositReturned (`shouldBe` Quantity 1_000_000)
            , expectField #withdrawals (\[ApiWithdrawal _ c] -> c .> Quantity 0)
            ]

    it "STAKE_POOLS_JOIN_05 - \
        \Can join when stake key already exists" $ \ctx -> runResourceT $ do
        let payload = Json [json| {
                "name": "Wallet with pre-registered stake key",
                "mnemonic_sentence": #{mnemonicToText preregKeyWallet},
                "passphrase": #{fixturePassphrase}
                } |]

        w <- unsafeResponse <$> postWallet ctx payload
        pool:_ <- map (view #id . getApiT) . snd <$>
            unsafeRequest @[ApiT StakePool]
                ctx (Link.listStakePools arbitraryStake) Empty

        eventually "wallet join a pool" $ do
            joinStakePool @n ctx (SpecificPool pool) (w, fixturePassphrase)
                >>= flip verify
                    [ expectResponseCode HTTP.status202
                    , expectField (#status . #getApiT) (`shouldBe` Pending)
                    , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                    ]

    describe "STAKE_POOLS_JOIN_UNSIGNED_01" $ do
        it "Can join a pool that's not retiring" $ \ctx -> runResourceT $ do
            nonRetiredPools <- eventually "One of the pools should retire." $ do
                response <- listPools ctx arbitraryStake

                verify response [ expectListSize 3 ]

                pure $ getFromResponse Prelude.id response

            let reportError = error $ unlines
                    [ "Unable to find a non-retiring pool ID."
                    , "Test cluster pools:"
                    , unlines (showT <$> Set.toList testClusterPoolIds)
                    , "Non-retired pools:"
                    , unlines (show <$> nonRetiredPools)
                    ]

            let nonRetiringPoolId = (view #id) . fromMaybe reportError
                    $ find (isNothing . getRetirementEpoch) nonRetiredPools

            let isValidCerts (Just (RegisterRewardAccount{}:|[JoinPool{}])) =
                    True
                isValidCerts _ =
                    False

            -- Join Pool
            w <- fixtureWallet ctx
            liftIO $ joinStakePoolUnsigned
                @n @'Shelley ctx w (ApiT nonRetiringPoolId) >>= \o -> do
                verify o
                    [ expectResponseCode HTTP.status200
                    , expectField #inputs
                        (`shouldSatisfy` (not . null))
                    , expectField #outputs
                        (`shouldSatisfy` null)
                    , expectField #change
                        (`shouldSatisfy` (not . null))
                    , expectField #certificates
                        (`shouldSatisfy` isValidCerts)
                    ]

    describe "STAKE_POOLS_JOIN_UNSIGNED_02"
        $ it "Can join a pool that's retiring" $ \ctx -> runResourceT $ do
            nonRetiredPools <- eventually "One of the pools should retire." $ do
                response <- listPools ctx arbitraryStake

                verify response [ expectListSize 3 ]

                pure $ getFromResponse Prelude.id response
            let reportError = error $ unlines
                    [ "Unable to find a retiring pool ID."
                    , "Test cluster pools:"
                    , unlines (showT <$> Set.toList testClusterPoolIds)
                    , "Non-retired pools:"
                    , unlines (show <$> nonRetiredPools)
                    ]

            let retiringPoolId = (view #id) . fromMaybe reportError
                    . find (isJust . getRetirementEpoch)
                    $ nonRetiredPools
            -- Join Pool
            w <- fixtureWallet ctx
            liftIO $ joinStakePoolUnsigned @n @'Shelley ctx w (ApiT retiringPoolId)
                >>= \o -> do
                    verify o
                        [ expectResponseCode HTTP.status200
                        , expectField #inputs
                            (`shouldSatisfy` (not . null))
                        , expectField #outputs
                            (`shouldSatisfy` null)
                        , expectField #change
                            (`shouldSatisfy` (not . null))
                        , expectField #certificates
                            (`shouldSatisfy` (not . null))
                        ]

    describe "STAKE_POOLS_JOIN_UNSIGNED_03"
        $ it "Cannot join a pool that's retired" $ \ctx -> runResourceT $ do
            nonRetiredPoolIds <-
                eventually "One of the pools should retire." $ do
                    response <- listPools ctx arbitraryStake
                    verify response [ expectListSize 3 ]
                    getFromResponse Prelude.id response
                        & fmap (view #id)
                        & Set.fromList
                        & pure
            let reportError = error $ unlines
                    [ "Unable to find a retired pool ID."
                    , "Test cluster pools:"
                    , unlines (showT <$> Set.toList testClusterPoolIds)
                    , "Non-retired pools:"
                    , unlines (showT <$> Set.toList nonRetiredPoolIds)
                    ]
            let retiredPoolIds =
                    testClusterPoolIds `Set.difference` nonRetiredPoolIds
            let retiredPoolId =
                    fromMaybe reportError $ listToMaybe $
                        Set.toList retiredPoolIds
            w <- fixtureWallet ctx
            r <- liftIO $ joinStakePoolUnsigned @n @'Shelley ctx w (ApiT retiredPoolId)
            expectResponseCode HTTP.status404 r
            expectErrorMessage (errMsg404NoSuchPool (toText retiredPoolId)) r

    describe "STAKE_POOLS_JOIN_UNSIGNED_04"
        $ it "Cannot join a pool that's never existed" $ \ctx -> runResourceT $ do
            (Right non_existing_pool_id) <- pure $ decodePoolIdBech32
                "pool1y25deq9kldy9y9gfvrpw8zt05zsrfx84zjhugaxrx9ftvwdpua2"
            w <- fixtureWallet ctx
            r <- liftIO $ joinStakePoolUnsigned
                @n @'Shelley ctx w (ApiT non_existing_pool_id)
            expectResponseCode HTTP.status404 r
            expectErrorMessage
                (errMsg404NoSuchPool (toText non_existing_pool_id)) r

    it "STAKE_POOLS_QUIT_UNSIGNED_01 - \
        \Join/quit when already joined a pool" $ \ctx -> runResourceT $ do
        w <- fixtureWallet ctx

        pool1 : pool2 : _  <- map (view #id) <$> notRetiringPools ctx

        joinStakePool @n ctx (SpecificPool pool1) (w, fixturePassphrase)
            >>= flip verify
            [ expectResponseCode HTTP.status202
            , expectField (#status . #getApiT) (`shouldBe` Pending)
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            ]

        waitNumberOfEpochBoundaries 2 ctx

        request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty
            >>= flip verify
            [ expectField #delegation (`shouldBe` delegating (ApiT pool1) []) ]

        -- Cannot join the same pool
        liftIO $ joinStakePoolUnsigned @n @'Shelley ctx w (ApiT pool1)
            >>= flip verify
            [ expectResponseCode HTTP.status403
            , expectErrorMessage (errMsg403PoolAlreadyJoined (toText pool1))
            ]

        -- Can join another pool
        liftIO $ joinStakePoolUnsigned @n @'Shelley ctx w (ApiT pool2)
            >>= flip verify
            [ expectResponseCode HTTP.status200
            , expectField #inputs (`shouldSatisfy` not . null)
            , expectField #certificates (`shouldSatisfy` \case
                Just (JoinPool{} :| []) -> True
                _ -> False
              )
            ]

        quitStakePoolUnsigned @n @'Shelley ctx w
            >>= flip verify
            [ expectResponseCode HTTP.status200
            , expectField #inputs (`shouldSatisfy` not . null)
            , expectField #outputs (`shouldSatisfy` null)
            , expectField #change (`shouldSatisfy` not . null)
            , expectField #certificates (`shouldSatisfy` \case
                Just (QuitPool{} :| []) -> True
                _ -> False
              )
            ]

    describe "STAKE_POOLS_QUIT_UNSIGNED_02"
        $ it "Cannot quit if not delegating" $ \ctx -> runResourceT $ do
            w <- fixtureWallet ctx

            quitStakePoolUnsigned @n @'Shelley ctx w >>= \r -> do
                expectResponseCode HTTP.status403 r
                flip expectErrorMessage r $ mconcat
                    [ "It seems that you're trying to retire from delegation "
                    , "although you're not even delegating, "
                    , "nor won't be in an immediate future"
                    ]

    describe "STAKE_POOLS_JOIN_01x - Fee boundary values" $ do
        it "STAKE_POOLS_JOIN_01x - \
            \I can join if I have just the right amount" $ \ctx -> runResourceT $ do
            w <- fixtureWalletWith @n ctx [costOfJoining ctx + depositAmt ctx]
            pool:_ <- map (view #id . getApiT) . snd <$>
                unsafeRequest @[ApiT StakePool]
                    ctx (Link.listStakePools arbitraryStake) Empty
            joinStakePool @n ctx (SpecificPool pool) (w, fixturePassphrase)>>= flip verify
                [ expectResponseCode HTTP.status202
                , expectField (#status . #getApiT) (`shouldBe` Pending)
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                ]

        it "STAKE_POOLS_JOIN_01x - \
           \I cannot join if I have not enough fee to cover" $ \ctx -> runResourceT $ do
            w <- fixtureWalletWith @n ctx [costOfJoining ctx + depositAmt ctx - 1]
            pool:_ <- map (view #id . getApiT) . snd <$>
                unsafeRequest @[ApiT StakePool]
                    ctx (Link.listStakePools arbitraryStake) Empty
            joinStakePool @n ctx (SpecificPool pool) (w, fixturePassphrase) >>= flip verify
                [ expectResponseCode HTTP.status403
                , expectErrorMessage errMsg403Fee
                ]

    describe "STAKE_POOLS_QUIT_01x - Fee boundary values" $ do

        it "STAKE_POOLS_QUIT_01xx - \
            \I can quit if I have enough to cover fee" $ \ctx -> runResourceT $ do
            -- change needed to satisfy minUTxOValue
            let initBalance =
                    [ costOfJoining ctx
                    + depositAmt ctx
                    + minUTxOValue (_mainEra ctx)
                    + costOfQuitting ctx
                    + minUTxOValue (_mainEra ctx)
                    ]
            w <- fixtureWalletWith @n ctx initBalance

            pool:_ <- map (view #id . getApiT) . snd
                <$> unsafeRequest @[ApiT StakePool]
                    ctx (Link.listStakePools arbitraryStake) Empty

            rJoin <- joinStakePool @n
                ctx (SpecificPool pool) (w, fixturePassphrase)
            verify rJoin
                [ expectResponseCode HTTP.status202
                , expectField (#status . #getApiT) (`shouldBe` Pending)
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                ]
            eventually "join transaction is in ledger" $ do
                let txId = ApiTxId (getFromResponse #id rJoin)
                request @(ApiTransaction n)
                    ctx (Link.getTransaction @'Shelley w txId) Default Empty
                >>= flip verify
                    [ expectField (#status . #getApiT) (`shouldBe` InLedger) ]

            -- Epoch A: delegation tx happened.
            -- Epoch A+1: stake is live, registered to a chosen pool.
            -- Epoch A+2: stake is active, rewards start accumulating.
            waitNumberOfEpochBoundaries 2 ctx

            request @ApiWallet ctx (Link.getWallet @'Shelley w)
                Default Empty >>= flip verify
                [ expectField #delegation (`shouldBe` delegating (ApiT pool) [])
                ]

            rQuit <- quitStakePool @n ctx (w, fixturePassphrase)
            verify rQuit
                [ expectResponseCode HTTP.status202
                , expectField (#status . #getApiT) (`shouldBe` Pending)
                , expectField (#direction . #getApiT) (`shouldBe` Incoming)
                , expectField #inputs $ \inputs' -> do
                    inputs' `shouldSatisfy` all (isJust . source)
                ]

            eventually "quit transaction is in ledger" $ do
                let txId = ApiTxId (getFromResponse #id rQuit)
                request @(ApiTransaction n)
                    ctx (Link.getTransaction @'Shelley w txId) Default Empty
                >>= flip verify
                    [ expectResponseCode HTTP.status200
                    , expectField (#direction . #getApiT) (`shouldBe` Incoming)
                    , expectField (#status . #getApiT) (`shouldBe` InLedger)
                    , expectField #metadata  (`shouldBe` Nothing)
                    , expectField #inputs (`shouldSatisfy` all (isJust . source))
                    ]

            -- Epoch A: un-delegation tx happened.
            -- Epoch A+1: un-delegation has been registered.
            -- Epoch A+2: wallet is not delegating;
            waitNumberOfEpochBoundaries 2 ctx

            request @ApiWallet ctx (Link.getWallet @'Shelley w)
                Default Empty >>= flip verify
                [ expectField #delegation
                    (`shouldBe` notDelegating [])
                , expectField (#balance . #total)
                    (.>= Quantity (depositAmt ctx))
                , expectField (#balance . #available)
                    (.>= Quantity (depositAmt ctx))
                ]

        it "STAKE_POOLS_QUIT_01x - \
            \I cannot quit if I have not enough to cover fees" $ \ctx -> runResourceT $ do
            let initBalance = [costOfJoining ctx + depositAmt ctx]
            w <- fixtureWalletWith @n ctx initBalance

            pool:_ <- map (view #id . getApiT) . snd
                <$> unsafeRequest @[ApiT StakePool]
                    ctx (Link.listStakePools arbitraryStake) Empty

            joinStakePool @n ctx (SpecificPool pool) (w, fixturePassphrase) >>= flip verify
                [ expectResponseCode HTTP.status202
                , expectField (#status . #getApiT) (`shouldBe` Pending)
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                ]

            -- Epoch A: delegation tx happened.
            -- Epoch A+1: stake is live, registered to a chosen pool.
            -- Epoch A+2: stake is active, rewards start accumulating.
            waitNumberOfEpochBoundaries 2 ctx

            request @ApiWallet ctx (Link.getWallet @'Shelley w)
                Default Empty >>= flip verify
                [ expectField #delegation (`shouldBe` delegating (ApiT pool) [])
                ]

            quitStakePool @n ctx (w, fixturePassphrase) >>= flip verify
                [ expectResponseCode HTTP.status403
                , expectErrorMessage errMsg403EmptyUTxO
                ]

    it "STAKE_POOLS_ESTIMATE_FEE_01 - can estimate fees" $ \ctx -> runResourceT $ do
        w <- fixtureWallet ctx
        delegationFee ctx w >>= flip verify
            [ expectResponseCode HTTP.status200
            , expectField (#deposit . #getQuantity) (`shouldBe` depositAmt ctx)
            , expectField (#estimatedMin . #getQuantity) (.< costOfJoining ctx)
            , expectField (#estimatedMax . #getQuantity) (.< costOfJoining ctx)
            , expectField #minimumCoins (`shouldBe` [])
            ]

    it "STAKE_POOLS_ESTIMATE_FEE_02 - \
        \empty wallet cannot estimate fee" $ \ctx -> runResourceT $ do
        w <- emptyWallet ctx
        delegationFee ctx w >>= flip verify
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403EmptyUTxO
            ]

    describe "STAKE_POOLS_LIST_01 - List stake pools" $ do

        it "has non-zero saturation & stake" $ \ctx -> runResourceT $ do
            eventually "list pools returns non-empty list" $ do
                r <- listPools ctx arbitraryStake
                expectResponseCode HTTP.status200 r
                verify r
                    [ expectListSize 3
                    -- At the time of setup, the pools have 1/3 stake each, but
                    -- this could potentially be changed by other tests. Hence,
                    -- we try to be forgiving here.
                    , expectListField 0
                        (#metrics . #relativeStake)
                            (.> Quantity (unsafeMkPercentage 0))
                    , expectListField 1
                        (#metrics . #relativeStake)
                            (.> Quantity (unsafeMkPercentage 0))
                    , expectListField 2
                        (#metrics . #relativeStake)
                            (.> Quantity (unsafeMkPercentage 0))
                    ]

        it "pools have the correct retirement information" $ \ctx -> runResourceT $ do

            let expectedRetirementEpochs = Set.fromList
                    [ Nothing
                    , Just 100_000
                    , Just 1_000_000
                    ]

            eventually "pools have the correct retirement information" $ do
                response <- listPools ctx arbitraryStake
                expectResponseCode HTTP.status200 response

                let actualRetirementEpochs =
                        getFromResponse Prelude.id response
                        & fmap getRetirementEpoch
                        & Set.fromList
                actualRetirementEpochs `shouldBe` expectedRetirementEpochs

        it "eventually has correct margin, cost and pledge" $ \ctx -> runResourceT $ do
            eventually "pool worker finds the certificate" $ do
                r <- listPools ctx arbitraryStake
                expectResponseCode HTTP.status200 r
                let oneMillionAda = 1_000_000_000_000
                let pools' = either (error . show) Prelude.id $ snd r

                -- To ignore the ordering of the pools, we use Set.
                setOf pools' (view #cost)
                    `shouldBe` Set.singleton (Quantity 0)

                setOf pools' (view #margin)
                    `shouldBe` Set.singleton (Quantity $ unsafeMkPercentage 0.1)

                setOf pools' (view #pledge)
                    `shouldBe` Set.fromList
                        [ Quantity $ 100 * oneMillionAda
                        , Quantity $ 100 * oneMillionAda
                        ]

        it "at least one pool eventually produces block" $ \ctx -> runResourceT $ do
            eventually "eventually produces block" $ do
                (_, Right r) <- listPools ctx arbitraryStake
                let production = sum $
                        getQuantity . view (#metrics . #producedBlocks) <$> r
                let saturation =
                        view (#metrics . #saturation) <$> r

                production `shouldSatisfy` (> 0)
                saturation `shouldSatisfy` (any (> 0))

        it "contains pool metadata" $ \ctx -> runResourceT $ bracketSettings ctx $ do
            updateMetadataSource ctx "direct"
            eventually "metadata is fetched" $ do
                r <- listPools ctx arbitraryStake
                verify r
                    [ expectListSize 3
                    , expectField Prelude.id $ \pools' -> do
                        let metadataActual = Set.fromList $
                                mapMaybe (view #metadata) pools'
                        metadataActual
                            `shouldSatisfy` (`Set.isSubsetOf` metadataPossible)
                        metadataActual
                            `shouldSatisfy` (not . Set.null)
                    ]

        it "contains and is sorted by non-myopic-rewards" $ \ctx -> runResourceT $ do
            eventually "eventually shows non-zero rewards" $ do
                Right pools'@[pool1,_pool2,pool3] <-
                    snd <$> listPools ctx arbitraryStake
                let rewards = view (#metrics . #nonMyopicMemberRewards)

                (rewards <$> pools') `shouldBe`
                    (rewards <$> sortOn (Down . rewards) pools')
                -- Make sure the rewards are not all equal:
                rewards pool1 .> rewards pool3

        it "non-myopic-rewards are based on stake" $ \ctx -> runResourceT $ do
            eventually "rewards are smaller for smaller stakes" $ do
                let stakeSmall = Just (Coin 1_000)
                let stakeBig = Just (Coin 10_000_000_000_000_000)
                Right poolsStakeSmall <- snd <$> listPools ctx stakeSmall
                Right poolsStakeBig <- snd <$> listPools ctx stakeBig
                let rewards =
                        view (#metrics . #nonMyopicMemberRewards . #getQuantity)
                let rewardsStakeSmall = sum (rewards <$> poolsStakeSmall)
                let rewardsStakeBig = sum (rewards <$> poolsStakeBig)

                rewardsStakeBig .> rewardsStakeSmall

    it "STAKE_POOLS_LIST_05 - Fails without query parameter" $ \ctx -> runResourceT $ do
        r <- request @[ApiT StakePool] ctx
            (Link.listStakePools Nothing) Default Empty
        expectResponseCode HTTP.status400 r

    it "STAKE_POOLS_LIST_06 - \
        \NonMyopicMemberRewards are 0 when stake is 0" $ \ctx -> runResourceT $ do
        liftIO $ pendingWith "This assumption seems false, for some reasons..."
        let stake = Just $ Coin 0
        r <- request @[ApiT StakePool] ctx (Link.listStakePools stake) Default Empty
            & (fmap . fmap . fmap . fmap) getApiT
        expectResponseCode HTTP.status200 r
        verify r
            [ expectListSize 3
            , expectListField 0
                (#metrics . #nonMyopicMemberRewards) (`shouldBe` Quantity 0)
            , expectListField 1
                (#metrics . #nonMyopicMemberRewards) (`shouldBe` Quantity 0)
            , expectListField 2
                (#metrics . #nonMyopicMemberRewards) (`shouldBe` Quantity 0)
            ]

    it "STAKE_POOLS_GARBAGE_COLLECTION_01 - \
        \retired pools are garbage collected on schedule and not before" $
        \ctx -> runResourceT $ do

            -- The retirement epoch of the only test pool that is configured
            -- to retire within the lifetime of an integration test run.
            -- See 'testPoolConfigs' for the source of this value.
            --
            let testPoolRetirementEpoch = 3

            -- The last epoch for which we will look for an associated pool
            -- garbage collection event. It corresponds to an arbitrary point
            -- in time just a few epochs after the garbage collection of the
            -- test pool.
            --
            -- Even though our test pool is configured to retire at the start
            -- of epoch 3, it should only actually be removed from the database
            -- during epoch 5. This is because the garbage collector is
            -- designed to wait two epochs after a pool retires before actually
            -- removing that pool, in order to avoid any issues with rollback.
            --
            -- See 'garbageCollectPools' for more information.
            --
            let lastGarbageCollectionEpoch = 8

            -- First wait until garbage has been collected for the last epoch
            -- of interest.
            --
            -- If this test case is executed as part of a long integration test
            -- run, this stage should complete very quickly, without any delay.
            --
            -- If this test case is run in isolation, this initial stage will
            -- require a few minutes to complete.
            --
            liftIO $ forM_ [1 .. lastGarbageCollectionEpoch] $ \epochNo -> do
                let stateDescription = mconcat
                        [ "Garbage has been collected for epoch "
                        , show epochNo
                        , "."
                        ]
                -- It's important that we wait incrementally, as an individual
                -- call to 'eventually' must complete within a fairly short
                -- period of time.
                eventually stateDescription $ do
                    events <- readIORef (view #_poolGarbageCollectionEvents ctx)
                    length events `shouldSatisfy` (>= epochNo)

            -- Check that exactly one pool was garbage collected, and no more:
            events <- liftIO $ readIORef (view #_poolGarbageCollectionEvents ctx)
            let certificates = poolGarbageCollectionCertificates =<< events
            liftIO $ certificates `shouldSatisfy` ((== 1) . length)
            let [certificate] = certificates
            let [event] = events &
                    filter (not . null . poolGarbageCollectionCertificates)

            -- Check that the removed pool was removed at the correct epoch:
            view #retirementEpoch certificate
                `shouldBe` testPoolRetirementEpoch
            poolGarbageCollectionEpochNo event
                `shouldBe` testPoolRetirementEpoch

            -- Check that the removed pool was one of the test pools:
            view #poolId certificate
                `shouldSatisfy` (`Set.member` testClusterPoolIds)

            -- Check that garbage collection occurred exactly once per epoch:
            let epochs = poolGarbageCollectionEpochNo <$> events
            (reverse epochs `zip` [1 ..]) `shouldSatisfy` all (uncurry (==))

    it "STAKE_POOLS_SMASH_01 - fetching metadata from SMASH works with delisted pools" $
        \ctx -> runResourceT $ bracketSettings ctx $ do
            updateMetadataSource ctx (_smashUrl ctx)
            -- This can be slow; let's retry less frequently and with a longer
            -- timeout.
            let s = 1_000_000
            eventuallyUsingDelay (10 * s) 300 "metadata is fetched" $ do
                r <- listPools ctx arbitraryStake
                verify r
                    [ expectListSize 3
                    , expectField Prelude.id $ \pools' -> do
                        let metadataActual = Set.fromList $
                                mapMaybe (view #metadata) pools'
                            delistedPools = filter (\pool -> Delisted `elem` flags pool)
                                pools'
                        metadataActual
                            `shouldSatisfy` (`Set.isSubsetOf` metadataPossible)
                        metadataActual
                            `shouldSatisfy` (not . Set.null)
                        (fmap (view #id) delistedPools)
                            `shouldBe` [PoolId . unsafeFromHex $
                                "b45768c1a2da4bd13ebcaa1ea51408eda31dcc21765ccbd407cda9f2"]
                    ]

            updateMetadataSource ctx "direct"
            eventually "pools are not delisted anymore" $ do
                r <- listPools ctx arbitraryStake
                verify r
                    [ expectListSize 3
                    , expectField Prelude.id $ \pools' -> do
                        let metadataActual = Set.fromList $
                                mapMaybe (view #metadata) pools'
                            delistedPools = filter (\pool -> Delisted `elem` flags pool)
                                pools'
                        metadataActual
                            `shouldSatisfy` (`Set.isSubsetOf` metadataPossible)
                        metadataActual
                            `shouldSatisfy` (not . Set.null)
                        (fmap (view #id) delistedPools)
                            `shouldBe` []
                    ]

    it "STAKE_POOLS_SMASH_HEALTH_01 - Can check SMASH health when configured" $
        \ctx -> runResourceT $ bracketSettings ctx $ do
            updateMetadataSource ctx (_smashUrl ctx)
            r <- request @ApiHealthCheck
                ctx Link.getCurrentSMASHHealth
                Default Empty
            expectResponseCode HTTP.status200 r
            expectField #health (`shouldBe` Available) r

    describe "STAKE_POOLS_SMASH_HEALTH_02 - Cannot check SMASH health when not configured" $
        forM_ ["direct", "none"] $ \fetching -> it fetching $
            \ctx -> runResourceT $ bracketSettings ctx $ do
                updateMetadataSource ctx (T.pack fetching)
                r <- request @ApiHealthCheck
                    ctx Link.getCurrentSMASHHealth
                    Default Empty
                expectResponseCode HTTP.status200 r
                expectField #health (`shouldBe` NoSmashConfigured) r

    it "STAKE_POOLS_SMASH_HEALTH_03 - Can check SMASH health via url" $
        \ctx -> runResourceT $ do
            let withUrl f (method, link) = (method, link <> "?url=" <> f)
            let link = withUrl (_smashUrl ctx) Link.getCurrentSMASHHealth

            r <- request @ApiHealthCheck ctx link Default Empty
            expectResponseCode HTTP.status200 r
            expectField #health (`shouldBe` Available) r

    describe "STAKE_POOLS_SMASH_HEALTH_04 - SMASH url needs to be valid" $ do
        let m = [ ("ftp://localhost", "only http/https is supported")
                , ("thats_not_link", "Not a valid absolute URI")
                ]
        forM_ m $ \(url, message) -> it url $ \ctx -> runResourceT $ do
            let withUrl f (method, link) = (method, link <> "?url=" <> T.pack f)
            let link = withUrl url Link.getCurrentSMASHHealth

            r <- request @ApiHealthCheck ctx link Default Empty
            verify r
                [ expectResponseCode HTTP.status400
                , expectErrorMessage message
                ]

    it "STAKE_KEY_LIST_01 - Can list stake keys" $ \ctx -> runResourceT $ do
        w <- fixtureWallet ctx
        let balance = Quantity 1_000_000_000_000

        -- fixtureWallets have funds on payment addresses, so their entire ada
        -- balance is not associated with their first stake key.
        request @(ApiStakeKeys n) ctx (Link.listStakeKeys w) Default Empty
            >>= flip verify
            [ expectField (#_foreign) (`shouldBe` [])
            , expectField (#_ours) (\case
                [acc] -> do
                    (acc ^. #_index) `shouldBe` 0
                    (acc ^. #_stake) `shouldBe` Quantity 0
                    acc ^. (#_delegation . #active . #status)
                        `shouldBe` NotDelegating
                _ -> expectationFailure "wrong number of accounts in \"ours\""
                )
            , expectField (#_none . #_stake) (`shouldBe` balance)
            ]

        -- By sending funds to ourselves, we associate funds with our stake key.
        -- Both from the payment output itself and the change.
        addrs <- listAddresses @n ctx w
        let addr = (addrs !! 1) ^. #id
        let payload = [json|
                { "payments":
                    [ { "address": #{addr}
                      , "amount":
                        { "quantity": 950000000000
                        , "unit": "lovelace"
                        }
                      }
                    ]
                , "passphrase": #{fixturePassphrase}
                }|]

        request @(ApiTransaction n) ctx
            (Link.createTransactionOld @'Shelley w)
            Default (Json payload)
            >>= flip verify
                [ expectResponseCode HTTP.status202 ]

        waitForTxImmutability ctx

        request @(ApiStakeKeys n) ctx (Link.listStakeKeys w) Default Empty
            >>= flip verify
            [ expectField (#_foreign) (`shouldBe` [])
            , expectField (#_ours) (\case
                [acc] -> do
                    (acc ^. #_stake) .> Quantity 0
                    acc ^. (#_delegation . #active . #status)
                        `shouldBe` NotDelegating
                _ -> expectationFailure "wrong number of accounts in \"ours\""
                )
            , expectField (#_none . #_stake) (.< balance)
            ]

    it "STAKE_KEY_LIST_02 - Can list foreign stake key from UTxO" $ \ctx -> runResourceT $ do
        w <- fixtureWallet ctx
        let balance = Quantity 1_000_000_000_000
        otherWallet <- emptyWallet ctx

        -- We send funds to one of our addresses but with a modified stake key.
        foreignAddr <- head . map (view #id) <$> listAddresses @n ctx otherWallet
        ourAddr <- head . map (view #id) <$> listAddresses @n ctx w
        let ourAddr' = replaceStakeKey ourAddr foreignAddr
        let payload = [json|
                { "payments":
                    [ { "address": #{ourAddr'}
                      , "amount":
                        { "quantity": 950000000000
                        , "unit": "lovelace"
                        }
                      }
                    ]
                , "passphrase": #{fixturePassphrase}
                }|]
        request @(ApiTransaction n) ctx
            (Link.createTransactionOld @'Shelley w)
            Default (Json payload)
            >>= flip verify
                [ expectResponseCode HTTP.status202 ]
        waitForTxImmutability ctx

        request @(ApiStakeKeys n) ctx (Link.listStakeKeys w) Default Empty
            >>= flip verify
            [ expectField (#_foreign) (\case
                [acc] -> do
                    (acc ^. #_stake) .> Quantity 0
                _ -> expectationFailure "wrong number of accounts in \"foreign\""
                )
            , expectField (#_ours) (\case
                [acc] -> do
                    -- NOTE: because of the change from the transaction, this
                    -- is no longer 0:
                    (acc ^. #_stake) .> Quantity 0
                    acc ^. (#_delegation . #active . #status)
                        `shouldBe` NotDelegating
                _ -> expectationFailure "wrong number of accounts in \"ours\""
                )
            , expectField (#_none . #_stake) (.< balance)
            ]
  where
    metadataPossible = Set.fromList
        [ StakePoolMetadata
            { ticker = (StakePoolTicker "GPA")
            , name = "Genesis Pool A"
            , description = Nothing
            , homepage = "https://iohk.io"
            }
        , StakePoolMetadata
            { ticker = (StakePoolTicker "GPB")
            , name = "Genesis Pool B"
            , description = Nothing
            , homepage = "https://iohk.io"
            }
        , StakePoolMetadata
            { ticker = (StakePoolTicker "GPC")
            , name = "Genesis Pool C"
            , description = Just "Lorem Ipsum Dolor Sit Amet."
            , homepage = "https://iohk.io"
            }
        , StakePoolMetadata
            { ticker = (StakePoolTicker "GPD")
            , name = "Genesis Pool D"
            , description = Just "Lorem Ipsum Dolor Sit Amet."
            , homepage = "https://iohk.io"
            }
        ]

    setOf :: Ord b => [a] -> (a -> b) -> Set b
    setOf xs f = Set.fromList $ map f xs

    depositAmt :: Context -> Natural
    depositAmt ctx =
        let
            c = ctx ^. #_networkParameters . #protocolParameters . #stakeKeyDeposit
        in
            fromIntegral (unCoin c)

    costOfJoining :: Context -> Natural
    costOfJoining ctx =
        if _mainEra ctx >= ApiBabbage
        then costOf (TxSize 485) ctx
        else costOf (TxSize 479) ctx

    costOfQuitting :: Context -> Natural
    costOfQuitting ctx =
        if _mainEra ctx >= ApiBabbage
        then costOf (TxSize 334) ctx
        else costOf (TxSize 332) ctx

    costOf :: TxSize -> Context -> Natural
    costOf (TxSize txSizeInBytes) ctx =
        txSizeInBytes * round slope + round intercept
      where
        pp = ctx ^. #_networkParameters . #protocolParameters
        LinearFee LinearFunction {..} = pp ^. #txParameters . #getFeePolicy

-- The complete set of pool identifiers in the static test pool cluster.
--
-- NOTE: This set effectively duplicates the set of pool identifiers defined
-- in the 'operators' constant of 'Cardano.Wallet.Shelley.Launch'.
--
-- TODO: Remove this duplication.
--
testClusterPoolIds :: Set PoolId
testClusterPoolIds = Set.fromList $ PoolId . unsafeFromHex <$>
    [ "1b3dc19c6ab89eaffc8501f375bb03c11bf8ed5d183736b1d80413d6"
    , "b45768c1a2da4bd13ebcaa1ea51408eda31dcc21765ccbd407cda9f2"
    , "bb114cb37d75fa05260328c235a3dae295a33d0ba674a5eb1e3e568e"
    , "ec28f33dcbe6d6400a1e5e339bd0647c0973ca6c0cf9c2bbe6838dc6"
    ]
