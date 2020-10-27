{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Integration.Scenario.API.Shelley.StakePools
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiCertificate (JoinPool, QuitPool, RegisterRewardAccount)
    , ApiListStakePools (..)
    , ApiStakePool
    , ApiT (..)
    , ApiTransaction
    , ApiWallet
    , ApiWalletDelegationStatus (..)
    , DecodeAddress
    , DecodeStakeAddress
    , EncodeAddress
    , WalletStyle (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( PaymentAddress )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey )
import Cardano.Wallet.Primitive.Fee
    ( FeePolicy (..) )
import Cardano.Wallet.Primitive.Types
    ( Coin (..)
    , Direction (..)
    , PoolId (..)
    , StakePoolMetadata (..)
    , StakePoolTicker (..)
    , TxStatus (..)
    , decodePoolIdBech32
    )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex, unsafeMkPercentage )
import Control.Monad
    ( forM_ )
import Data.Bifunctor
    ( second )
import Data.Function
    ( (&) )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.IORef
    ( readIORef )
import Data.List
    ( find, sortOn )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Maybe
    ( fromMaybe, isJust, isNothing, listToMaybe, mapMaybe )
import Data.Ord
    ( Down (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Set
    ( Set )
import Data.Text
    ( Text )
import Data.Text.Class
    ( showT, toText )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( SpecWith, describe, pendingWith, shouldBe, shouldSatisfy )
import Test.Hspec.Extra
    ( it )
import Test.Integration.Framework.Context
    ( Context (..), PoolGarbageCollectionEvent (..) )
import Test.Integration.Framework.DSL
    ( Headers (..)
    , Payload (..)
    , delegating
    , delegationFee
    , emptyWallet
    , eventually
    , expectErrorMessage
    , expectField
    , expectListField
    , expectListSize
    , expectResponseCode
    , fixturePassphrase
    , fixtureWallet
    , fixtureWalletWith
    , getFromResponse
    , getRetirementEpoch
    , getSlotParams
    , joinStakePool
    , joinStakePoolUnsigned
    , json
    , listAddresses
    , minUTxOValue
    , notDelegating
    , quitStakePool
    , quitStakePoolUnsigned
    , request
    , unsafeRequest
    , verify
    , waitForNextEpoch
    , walletId
    , (.>)
    )
import Test.Integration.Framework.TestData
    ( errMsg403DelegationFee
    , errMsg403NonNullReward
    , errMsg403NotDelegating
    , errMsg403PoolAlreadyJoined
    , errMsg403WrongPass
    , errMsg404NoSuchPool
    , errMsg404NoWallet
    )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Data.ByteString as BS
import qualified Data.Set as Set
import qualified Network.HTTP.Types.Status as HTTP

spec :: forall n t.
    ( DecodeAddress n
    , DecodeStakeAddress n
    , EncodeAddress n
    , PaymentAddress n ShelleyKey
    ) => SpecWith (Context t)
spec = describe "SHELLEY_STAKE_POOLS" $ do
    let listPools ctx stake = (second . second) (view #pools)
            <$> request @(ApiListStakePools ApiStakePool) @IO ctx
                (Link.listStakePools stake) Default Empty

    it "STAKE_POOLS_JOIN_01 - Cannot join non-existent wallet" $ \ctx -> do
        w <- emptyWallet ctx
        let wid = w ^. walletId
        _ <- request @ApiWallet ctx
            (Link.deleteWallet @'Shelley w) Default Empty
        let poolIdAbsent = PoolId $ BS.pack $ replicate 32 1
        r <- joinStakePool @n ctx (ApiT poolIdAbsent) (w, fixturePassphrase)
        expectResponseCode HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet wid) r

    it "STAKE_POOLS_JOIN_01 - Cannot join non-existent stakepool" $ \ctx -> do
        w <- fixtureWallet ctx
        let poolIdAbsent = PoolId $ BS.pack $ replicate 32 1
        r <- joinStakePool @n ctx (ApiT poolIdAbsent) (w, fixturePassphrase)
        expectResponseCode HTTP.status404 r
        expectErrorMessage (errMsg404NoSuchPool (toText poolIdAbsent)) r

    it "STAKE_POOLS_JOIN_01 - \
        \Cannot join existent stakepool with wrong password" $ \ctx -> do
        w <- fixtureWallet ctx
        pool:_ <- map (view #id) . view #pools . snd <$> unsafeRequest
            @(ApiListStakePools ApiStakePool)
            ctx (Link.listStakePools arbitraryStake) Empty
        joinStakePool @n ctx pool (w, "Wrong Passphrase") >>= flip verify
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403WrongPass
            ]

    it "STAKE_POOLS_JOIN_01rewards - \
        \Can join a pool, earn rewards and collect them" $ \ctx -> do
        -- Setup
        src <- fixtureWallet ctx
        dest <- emptyWallet ctx

        -- Join Pool
        pool:_ <- map (view #id) . view #pools . snd <$>
            unsafeRequest @(ApiListStakePools ApiStakePool) ctx
            (Link.listStakePools arbitraryStake) Empty
        joinStakePool @n ctx pool (src, fixturePassphrase) >>= flip verify
            [ expectResponseCode HTTP.status202
            , expectField (#status . #getApiT) (`shouldBe` Pending)
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            ]

        -- Earn rewards
        waitForNextEpoch ctx
        waitForNextEpoch ctx
        (previousBalance, walletRewards) <-
            eventually "Wallet gets rewards" $ do
                r <- request @ApiWallet ctx (Link.getWallet @'Shelley src)
                    Default Empty
                verify r
                    [ expectField
                        (#balance . #getApiT . #reward)
                        (.> (Quantity 0))
                    ]
                let availableBalance =
                        getFromResponse (#balance . #getApiT . #available) r
                let rewardBalance =
                        getFromResponse (#balance . #getApiT . #reward) r
                pure (availableBalance, rewardBalance)

        -- Try to use rewards
        addrs <- listAddresses @n ctx dest
        let coin = minUTxOValue :: Natural
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
            (Link.createTransaction @'Shelley src)
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
                      (#balance . #getApiT . #reward) (`shouldBe` walletRewards)
                  ]

        -- there's currently no withdrawals in the wallet
        rw1 <- request @[ApiTransaction n] ctx
            (Link.listTransactions' @'Shelley src (Just 1)
                Nothing Nothing Nothing)
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

        rTx <- request @(ApiTransaction n) ctx
            (Link.createTransaction @'Shelley src)
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
                        (#balance . #getApiT . #reward)
                        (`shouldBe` (Quantity 0))
                    , expectField
                        (#balance . #getApiT . #available)
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
                    Nothing Nothing Nothing)
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
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            ]
        let txid = getFromResponse Prelude.id rq
        let (Quantity quitFeeAmt) = getFromResponse #amount rq
        let finalQuitAmt = Quantity (depositAmt ctx - quitFeeAmt)

        eventually "Certificates are inserted after quiting a pool" $ do
            -- last made transaction `txid` is for quitting pool and,
            -- in fact, it becomes incoming because there is
            -- keyDeposit being returned
            let epg = Link.getTransaction @'Shelley src txid
            rlg <- request @(ApiTransaction n) ctx epg Default Empty
            verify rlg
                [ expectField
                    (#direction . #getApiT) (`shouldBe` Incoming)
                , expectField
                    #amount (`shouldBe` finalQuitAmt)
                , expectField
                    (#status . #getApiT) (`shouldBe` InLedger)
                ]

            let epl = Link.listTransactions @'Shelley src
            rlq <- request @[ApiTransaction n] ctx epl Default Empty
            verify rlq
                [ expectListField 0
                    (#direction . #getApiT) (`shouldBe` Incoming)
                , expectListField 0
                    #amount (`shouldBe` finalQuitAmt)
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
        \Cannot join already joined stake pool" $ \ctx -> do
        w <- fixtureWallet ctx
        pool:_ <- map (view #id) . view #pools . snd
            <$> unsafeRequest @(ApiListStakePools ApiStakePool)
                ctx (Link.listStakePools arbitraryStake) Empty
        joinStakePool @n ctx pool (w, fixturePassphrase) >>= flip verify
            [ expectResponseCode HTTP.status202
            , expectField (#status . #getApiT) (`shouldBe` Pending)
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            ]

        -- Wait for the certificate to be inserted
        eventually "Certificates are inserted" $ do
            let ep = Link.listTransactions @'Shelley w
            request @[ApiTransaction n] ctx ep Default Empty >>= flip verify
                [ expectListField 0
                    (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectListField 0
                    (#status . #getApiT) (`shouldBe` InLedger)
                ]
        joinStakePool @n ctx pool (w, fixturePassphrase) >>= flip verify
            [ expectResponseCode HTTP.status403
            , expectErrorMessage
                (errMsg403PoolAlreadyJoined $ toText $ getApiT pool)
            ]

    it "STAKE_POOLS_JOIN_03 - Cannot join a pool that has retired" $ \ctx -> do
        nonRetiredPoolIds <- eventually "One of the pools should retire." $ do
            response <- listPools ctx arbitraryStake
            verify response [ expectListSize 3 ]
            getFromResponse Prelude.id response
                & fmap (view (#id . #getApiT))
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
                fromMaybe reportError $ listToMaybe $ Set.toList retiredPoolIds
        w <- fixtureWallet ctx
        r <- joinStakePool @n ctx (ApiT retiredPoolId) (w, fixturePassphrase)
        expectResponseCode HTTP.status404 r
        expectErrorMessage (errMsg404NoSuchPool (toText retiredPoolId)) r

    it "STAKE_POOLS_QUIT_02 - Passphrase must be correct to quit" $ \ctx -> do
        w <- fixtureWallet ctx
        pool:_ <- map (view #id) . view #pools . snd
            <$> unsafeRequest @(ApiListStakePools ApiStakePool)
                ctx (Link.listStakePools arbitraryStake) Empty
        joinStakePool @n ctx pool (w, fixturePassphrase) >>= flip verify
            [ expectResponseCode HTTP.status202
            , expectField (#status . #getApiT) (`shouldBe` Pending)
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            ]

        -- Wait for the certificate to be inserted
        eventually "Certificates are inserted" $ do
            let ep = Link.listTransactions @'Shelley w
            request @[ApiTransaction n] ctx ep Default Empty >>= flip verify
                [ expectListField 0
                    (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectListField 0
                    (#status . #getApiT) (`shouldBe` InLedger)
                ]
        let wrongPassphrase = "Incorrect Passphrase"
        quitStakePool @n ctx (w, wrongPassphrase) >>= flip verify
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403WrongPass
            ]

    it "STAKE_POOL_NEXT_02/STAKE_POOLS_QUIT_01 - \
        \Cannot quit when active: not_delegating"
        $ \ctx -> do
        w <- fixtureWallet ctx
        quitStakePool @n ctx (w, fixturePassphrase) >>= flip verify
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403NotDelegating
            ]

    it "STAKE_POOLS_JOIN_01 - Can rejoin another stakepool" $ \ctx -> do
        w <- fixtureWallet ctx

        -- make sure we are at the beginning of new epoch
        (currentEpoch, _) <- getSlotParams ctx
        waitForNextEpoch ctx

        pool1:pool2:_ <- map (view #id) . view #pools . snd
            <$> unsafeRequest @(ApiListStakePools ApiStakePool)
                ctx (Link.listStakePools arbitraryStake) Empty

        joinStakePool @n ctx pool1 (w, fixturePassphrase) >>= flip verify
            [ expectResponseCode HTTP.status202
            , expectField (#status . #getApiT) (`shouldBe` Pending)
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            ]

        -- Wait for the certificate to be inserted
        eventually "Certificates are inserted" $ do
            let ep = Link.listTransactions @'Shelley w
            request @[ApiTransaction n] ctx ep Default Empty >>= flip verify
                [ expectListField 0
                    (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectListField 0
                    (#status . #getApiT) (`shouldBe` InLedger)
                ]

        request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty
            >>= flip verify
                [ expectField (#delegation . #next)
                    (\case
                        [dlg] -> do
                            (dlg ^. #status) `shouldBe`
                                Delegating
                            (dlg ^. #target) `shouldBe`
                                Just pool1
                            (view #epochNumber <$> dlg ^. #changesAt) `shouldBe`
                                Just (ApiT $ currentEpoch + 3)
                        _ ->
                            fail "next delegation should contain exactly one element"
                    )
                ]
        eventually "Wallet is delegating to p1" $ do
            request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty
                >>= flip verify
                    [ expectField #delegation (`shouldBe` delegating pool1 [])
                    ]

        -- join another stake pool
        joinStakePool @n ctx pool2 (w, fixturePassphrase) >>= flip verify
            [ expectResponseCode HTTP.status202
            , expectField (#status . #getApiT) (`shouldBe` Pending)
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            ]

        -- Wait for the certificate to be inserted
        eventually "Certificates are inserted" $ do
            let ep = Link.listTransactions @'Shelley w
            request @[ApiTransaction n] ctx ep Default Empty >>= flip verify
                [ expectListField 1
                    (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectListField 1
                    (#status . #getApiT) (`shouldBe` InLedger)
                ]

        eventually "Wallet is delegating to p2" $ do
            request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty
                >>= flip verify
                    [ expectField #delegation (`shouldBe` delegating pool2 [])
                    ]

    it "STAKE_POOLS_JOIN_04 - Rewards accumulate" $ \ctx -> do
        w <- fixtureWallet ctx
        pool:_ <- map (view #id) . view #pools . snd
            <$> unsafeRequest @(ApiListStakePools ApiStakePool)
                ctx (Link.listStakePools arbitraryStake) Empty
        -- Join a pool
        joinStakePool @n ctx pool (w, fixturePassphrase) >>= flip verify
            [ expectResponseCode HTTP.status202
            , expectField (#status . #getApiT) (`shouldBe` Pending)
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            ]
        eventually "Certificates are inserted" $ do
            let ep = Link.listTransactions @'Shelley w
            request @[ApiTransaction n] ctx ep Default Empty >>= flip verify
                [ expectListField 0
                    (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectListField 0
                    (#status . #getApiT) (`shouldBe` InLedger)
                ]

        waitForNextEpoch ctx
        waitForNextEpoch ctx

        -- Wait for money to flow
        eventually "Wallet gets rewards" $ do
            request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty
                >>= flip verify
                    [ expectField (#balance . #getApiT . #reward)
                        (.> (Quantity 0))
                    ]

        -- Can't quite if unspoiled rewards.
        quitStakePool @n ctx (w, fixturePassphrase) >>= flip verify
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403NonNullReward
            ]

    it "STAKE_POOLS_JOIN_05 - \
        \Can join when stake key already exists" $ \ctx -> do
        let walletWithPreRegKey =
                [ "over", "decorate", "flock", "badge", "beauty"
                , "stamp" , "chest", "owner", "excess", "omit"
                , "bid", "raccoon", "spin" , "reduce", "rival"
                ] :: [Text]
        let payload = Json [json| {
                "name": "Wallet with pre-registered stake key",
                "mnemonic_sentence": #{walletWithPreRegKey},
                "passphrase": #{fixturePassphrase}
                } |]

        (_, w) <- unsafeRequest @ApiWallet ctx
            (Link.postWallet @'Shelley) payload
        pool:_ <- map (view #id) . view #pools . snd <$>
            unsafeRequest @(ApiListStakePools ApiStakePool)
                ctx (Link.listStakePools arbitraryStake) Empty

        eventually "wallet join a pool" $ do
            joinStakePool @n ctx pool (w, fixturePassphrase) >>= flip verify
                [ expectResponseCode HTTP.status202
                , expectField (#status . #getApiT) (`shouldBe` Pending)
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                ]

    describe "STAKE_POOLS_JOIN_UNSIGNED_01" $ do
        it "Can join a pool that's not retiring" $ \ctx -> do
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
                    . find (isNothing . getRetirementEpoch)
                    $ nonRetiredPools

            let isValidCerts (Just (RegisterRewardAccount{}:|[JoinPool{}])) =
                    True
                isValidCerts _ =
                    False

            -- Join Pool
            w <- fixtureWallet ctx
            joinStakePoolUnsigned
                @n @'Shelley ctx w nonRetiringPoolId >>= \o -> do
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
        $ it "Can join a pool that's retiring" $ \ctx -> do
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
            joinStakePoolUnsigned @n @'Shelley ctx w retiringPoolId >>= \o -> do
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
        $ it "Cannot join a pool that's retired" $ \ctx -> do
            nonRetiredPoolIds <-
                eventually "One of the pools should retire." $ do
                    response <- listPools ctx arbitraryStake
                    verify response [ expectListSize 3 ]
                    getFromResponse Prelude.id response
                        & fmap (view (#id . #getApiT))
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
            r <- joinStakePoolUnsigned @n @'Shelley ctx w (ApiT retiredPoolId)
            expectResponseCode HTTP.status404 r
            expectErrorMessage (errMsg404NoSuchPool (toText retiredPoolId)) r

    describe "STAKE_POOLS_JOIN_UNSIGNED_04"
        $ it "Cannot join a pool that's never existed" $ \ctx -> do
            (Right non_existing_pool_id) <- pure $ decodePoolIdBech32
                "pool1y25deq9kldy9y9gfvrpw8zt05zsrfx84zjhugaxrx9ftvwdpua2"
            w <- fixtureWallet ctx
            r <- joinStakePoolUnsigned
                @n @'Shelley ctx w (ApiT non_existing_pool_id)
            expectResponseCode HTTP.status404 r
            expectErrorMessage
                (errMsg404NoSuchPool (toText non_existing_pool_id)) r

    describe "STAKE_POOLS_QUIT_UNSIGNED_01"
        $ it "Join/quit when already joined a pool" $ \ctx -> do
            w <- fixtureWallet ctx

            pool1:pool2:_ <- map (view #id) . view #pools . snd <$>
                unsafeRequest @(ApiListStakePools ApiStakePool)
                    ctx (Link.listStakePools arbitraryStake) Empty

            joinStakePool @n ctx pool1 (w, fixturePassphrase) >>= flip verify
                [ expectResponseCode HTTP.status202
                , expectField (#status . #getApiT) (`shouldBe` Pending)
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                ]

            eventually "Wallet is delegating to p1" $ do
                request @ApiWallet ctx (Link.getWallet @'Shelley w)
                    Default Empty >>= flip verify
                    [ expectField #delegation (`shouldBe` delegating pool1 [])
                    ]

            -- Cannot join the same pool
            let pid = toText $ pool1 ^. #getApiT
            joinStakePoolUnsigned @n @'Shelley ctx w pool1 >>= \o -> do
                verify o
                    [ expectResponseCode HTTP.status403
                    , expectErrorMessage (errMsg403PoolAlreadyJoined pid)
                    ]

            -- Can join another pool
            let isValidCertsJoin (Just (JoinPool{}:|[])) = True
                isValidCertsJoin _ = False
            joinStakePoolUnsigned @n @'Shelley ctx w pool2 >>= \o -> do
                verify o
                    [ expectResponseCode HTTP.status200
                    , expectField #inputs
                        (`shouldSatisfy` (not . null))
                    , expectField #certificates
                        (`shouldSatisfy` isValidCertsJoin)
                    ]

            -- Can quit pool
            let isValidCertsQuit (Just (QuitPool{}:|[])) = True
                isValidCertsQuit _ = False
            quitStakePoolUnsigned @n @'Shelley ctx w >>= \o -> do
                verify o
                    [ expectResponseCode HTTP.status200
                    , expectField #inputs
                        (`shouldSatisfy` (not . null))
                    , expectField #outputs
                        (`shouldSatisfy` null)
                    , expectField #change
                        (`shouldSatisfy` (not . null))
                    , expectField #certificates
                        (`shouldSatisfy` ((== 1) . length))
                    , expectField #certificates
                        (`shouldSatisfy` isValidCertsQuit)
                    ]

    describe "STAKE_POOLS_QUIT_UNSIGNED_02"
        $ it "Cannot quit if not delegating" $ \ctx -> do
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
            \I can join if I have just the right amount" $ \ctx -> do
            w <- fixtureWalletWith @n ctx [costOfJoining ctx + depositAmt ctx]
            pool:_ <- map (view #id) . view #pools . snd <$>
                unsafeRequest @(ApiListStakePools ApiStakePool)
                    ctx (Link.listStakePools arbitraryStake) Empty
            joinStakePool @n ctx pool (w, fixturePassphrase)>>= flip verify
                [ expectResponseCode HTTP.status202
                , expectField (#status . #getApiT) (`shouldBe` Pending)
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                ]

        it "STAKE_POOLS_JOIN_01x - \
           \I cannot join if I have not enough fee to cover" $ \ctx -> do
            w <- fixtureWalletWith @n ctx [costOfJoining ctx + depositAmt ctx - 1]
            pool:_ <- map (view #id) . view #pools . snd <$>
                unsafeRequest @(ApiListStakePools ApiStakePool)
                    ctx (Link.listStakePools arbitraryStake) Empty
            joinStakePool @n ctx pool (w, fixturePassphrase) >>= flip verify
                [ expectResponseCode HTTP.status403
                , expectErrorMessage (errMsg403DelegationFee 1)
                ]

    describe "STAKE_POOLS_QUIT_01x - Fee boundary values" $ do

        it "STAKE_POOLS_QUIT_01xx - \
            \I can quit if I have enough to cover fee" $ \ctx -> do
            -- change needed to satisfy minUTxOValue
            let change = minUTxOValue - costOfQuitting ctx
            let initBalance =
                    [ costOfJoining ctx
                    + depositAmt ctx
                    + costOfQuitting ctx
                    + change
                    + costOfChange ctx
                    ]
            w <- fixtureWalletWith @n ctx initBalance

            pool:_ <- map (view #id) . view #pools . snd
                <$> unsafeRequest @(ApiListStakePools ApiStakePool)
                    ctx (Link.listStakePools arbitraryStake) Empty

            joinStakePool @n ctx pool (w, fixturePassphrase) >>= flip verify
                [ expectResponseCode HTTP.status202
                , expectField (#status . #getApiT) (`shouldBe` Pending)
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                ]

            eventually "Wallet is delegating to p1" $ do
                request @ApiWallet ctx (Link.getWallet @'Shelley w)
                    Default Empty >>= flip verify
                    [ expectField #delegation (`shouldBe` delegating pool [])
                    ]

            quitStakePool @n ctx (w, fixturePassphrase) >>= flip verify
                [ expectResponseCode HTTP.status202
                ]
            eventually "Wallet is not delegating and it got his deposit back" $
                do
                request @ApiWallet ctx (Link.getWallet @'Shelley w)
                    Default Empty >>= flip verify
                    [ expectField #delegation (`shouldBe` notDelegating [])
                    , expectField
                        (#balance . #getApiT . #total)
                            (`shouldSatisfy` (== (Quantity (depositAmt ctx + change))))
                    , expectField
                        (#balance . #getApiT . #available)
                            (`shouldSatisfy` (== (Quantity (depositAmt ctx + change))))
                    ]

        it "STAKE_POOLS_QUIT_01x - \
            \I cannot quit if I have not enough to cover fees" $ \ctx -> do
            let initBalance = [ costOfJoining ctx + depositAmt ctx ]
            w <- fixtureWalletWith @n ctx initBalance

            pool:_ <- map (view #id) . view #pools . snd
                <$> unsafeRequest @(ApiListStakePools ApiStakePool)
                    ctx (Link.listStakePools arbitraryStake) Empty

            joinStakePool @n ctx pool (w, fixturePassphrase) >>= flip verify
                [ expectResponseCode HTTP.status202
                , expectField (#status . #getApiT) (`shouldBe` Pending)
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                ]

            eventually "Wallet is delegating to p1" $ do
                request @ApiWallet ctx (Link.getWallet @'Shelley w)
                    Default Empty >>= flip verify
                    [ expectField #delegation (`shouldBe` delegating pool [])
                    ]

            quitStakePool @n ctx (w, fixturePassphrase) >>= flip verify
                [ expectResponseCode HTTP.status403
                , expectErrorMessage $ errMsg403DelegationFee 115900
                ]

    it "STAKE_POOLS_ESTIMATE_FEE_02 - \
        \empty wallet cannot estimate fee" $ \ctx -> do
        w <- emptyWallet ctx
        delegationFee ctx w >>= flip verify
            [ expectResponseCode HTTP.status403
            , expectErrorMessage $ errMsg403DelegationFee
                (costOfJoining ctx - costOfChange ctx)
            ]

    describe "STAKE_POOLS_LIST_01 - List stake pools" $ do

        it "has non-zero saturation & stake" $ \ctx -> do
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

        it "pools have the correct retirement information" $ \ctx -> do

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

        it "eventually has correct margin, cost and pledge" $ \ctx -> do
            eventually "pool worker finds the certificate" $ do
                r <- listPools ctx arbitraryStake
                expectResponseCode HTTP.status200 r
                let oneMillionAda = 1_000_000_000_000
                let pools' = either (error . show) Prelude.id $ snd r

                -- To ignore the ordering of the pools, we use Set.
                setOf pools' (view #cost)
                    `shouldBe` Set.singleton (Quantity 0)

                setOf pools' (view #margin)
                    `shouldBe`
                    Set.singleton
                        (Quantity $ unsafeMkPercentage 0.1)

                setOf pools' (view #pledge)
                    `shouldBe`
                    Set.fromList
                        [ Quantity oneMillionAda
                        , Quantity $ 2 * oneMillionAda
                        ]

        it "at least one pool eventually produces block" $ \ctx -> do
            eventually "eventually produces block" $ do
                (_, Right r) <- listPools ctx arbitraryStake
                let production = sum $
                        getQuantity . view (#metrics . #producedBlocks) <$> r
                let saturation =
                        view (#metrics . #saturation) <$> r

                production `shouldSatisfy` (> 0)
                saturation `shouldSatisfy` (any (> 0))

        it "contains pool metadata" $ \ctx -> do
            eventually "metadata is fetched" $ do
                r <- listPools ctx arbitraryStake
                let metadataPossible = Set.fromList
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

                verify r
                    [ expectListSize 3
                    , expectField Prelude.id $ \pools' -> do
                        let metadataActual = Set.fromList $
                                mapMaybe (fmap getApiT . view #metadata) pools'
                        metadataActual
                            `shouldSatisfy` (`Set.isSubsetOf` metadataPossible)
                    ]

        it "contains and is sorted by non-myopic-rewards" $ \ctx -> do
            eventually "eventually shows non-zero rewards" $ do
                Right pools'@[pool1,_pool2,pool3] <-
                    snd <$> listPools ctx arbitraryStake
                let rewards = view (#metrics . #nonMyopicMemberRewards)
                print (rewards <$> pools') -- FIXME temporary
                (rewards <$> pools') `shouldBe`
                    (rewards <$> sortOn (Down . rewards) pools')
                -- Make sure the rewards are not all equal:
                rewards pool1 .> rewards pool3

        it "non-myopic-rewards are based on stake" $ \ctx -> do
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

    it "STAKE_POOLS_LIST_05 - Fails without query parameter" $ \ctx -> do
        r <- request @(ApiListStakePools ApiStakePool) @IO ctx
            (Link.listStakePools Nothing) Default Empty
        expectResponseCode HTTP.status400 r

    it "STAKE_POOLS_LIST_06 - \
        \NonMyopicMemberRewards are 0 when stake is 0" $ \ctx -> do
        pendingWith "This assumption seems false, for some reasons..."
        let stake = Just $ Coin 0
        r <- request @(ApiListStakePools ApiStakePool) @IO
            ctx (Link.listStakePools stake)
            Default Empty
        expectResponseCode HTTP.status200 r
        verify ((second . second) (view #pools) r)
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
        \ctx -> do

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
            forM_ [1 .. lastGarbageCollectionEpoch] $ \epochNo -> do
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
            events <- readIORef (view #_poolGarbageCollectionEvents ctx)
            let certificates = poolGarbageCollectionCertificates =<< events
            certificates `shouldSatisfy` ((== 1) . length)
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

  where
    arbitraryStake :: Maybe Coin
    arbitraryStake = Just $ ada 10_000_000_000
      where ada = Coin . (1000*1000*)

    setOf :: Ord b => [a] -> (a -> b) -> Set b
    setOf xs f = Set.fromList $ map f xs

    depositAmt :: Context t -> Natural
    depositAmt ctx =
        let
            pp = ctx ^. #_networkParameters . #protocolParameters
            LinearFee _ _ (Quantity c) = pp ^. #txParameters . #getFeePolicy
        in
            round c

    costOfJoining :: Context t -> Natural
    costOfJoining = costOf (\coeff cst -> 364 * coeff + cst)

    costOfQuitting :: Context t -> Natural
    costOfQuitting = costOf (\coeff cst -> 297 * coeff + cst)

    costOfChange :: Context t -> Natural
    costOfChange = costOf (\coeff _cst -> 133 * coeff)

    costOf :: (Natural -> Natural -> Natural) -> Context t -> Natural
    costOf withCoefficients ctx =
        withCoefficients coeff cst
      where
        pp = ctx ^. #_networkParameters . #protocolParameters
        (cst, coeff) = (round $ getQuantity a, round $ getQuantity b)
        LinearFee a b _ = pp ^. #txParameters . #getFeePolicy

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
