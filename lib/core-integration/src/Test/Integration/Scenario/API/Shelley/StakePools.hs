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
    ( ApiStakePool
    , ApiT (..)
    , ApiTransaction
    , ApiWallet
    , ApiWalletDelegationStatus (..)
    , ApiWithdrawRewards (..)
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
    , EpochNo (..)
    , PoolId (..)
    , StakePoolMetadata (..)
    , StakePoolTicker (..)
    , TxStatus (..)
    )
import Cardano.Wallet.Unsafe
    ( unsafeMkPercentage )
import Data.Function
    ( (&) )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.List
    ( sortOn )
import Data.Maybe
    ( mapMaybe )
import Data.Ord
    ( Down (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Set
    ( Set )
import Data.Text
    ( Text )
import Data.Text.Class
    ( toText )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( SpecWith, describe, pendingWith, shouldBe, shouldSatisfy )
import Test.Hspec.Extra
    ( it )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
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
    , getSlotParams
    , joinStakePool
    , json
    , listAddresses
    , notDelegating
    , quitStakePool
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
spec = do
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
        pool:_ <- map (view #id) . snd <$> unsafeRequest
            @[ApiStakePool] ctx (Link.listStakePools arbitraryStake) Empty
        joinStakePool @n ctx pool (w, "Wrong Passphrase") >>= flip verify
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403WrongPass
            ]

    it "STAKE_POOLS_JOIN_01 - \
        \Can join a pool, earn rewards and collect them" $ \ctx -> do
        -- Setup
        w <- fixtureWallet ctx

        -- Join Pool
        pool:_ <- map (view #id) . snd <$> unsafeRequest @[ApiStakePool] ctx
            (Link.listStakePools arbitraryStake) Empty
        joinStakePool @n ctx pool (w, fixturePassphrase) >>= flip verify
            [ expectResponseCode HTTP.status202
            , expectField (#status . #getApiT) (`shouldBe` Pending)
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            ]

        -- Earn rewards
        waitForNextEpoch ctx
        waitForNextEpoch ctx
        (previousBalance, walletRewards) <-
            eventually "Wallet gets rewards" $ do
                r <- request @ApiWallet ctx (Link.getWallet @'Shelley w)
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
        addrs <- listAddresses @n ctx w
        let coin = 1 :: Natural
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
            (Link.createTransaction @'Shelley w)
            Default (Json payload)
        expectResponseCode HTTP.status202 r1
        eventually "Wallet has not consumed rewards" $ do
          let linkSrc = Link.getTransaction @'Shelley
                  w (getFromResponse Prelude.id r1)
          request @(ApiTransaction n) ctx linkSrc Default Empty
              >>= flip verify
                  [ expectField
                      (#status . #getApiT) (`shouldBe` InLedger)
                  ]
          request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty
              >>= flip verify
                  [ expectField
                      (#balance . #getApiT . #reward) (`shouldBe` walletRewards)
                  ]

        -- there's currently no withdrawals in the wallet
        rw1 <- request @[ApiTransaction n] ctx
            (Link.listTransactions' @'Shelley w (Just 1)
                Nothing Nothing Nothing)
            Default Empty
        verify rw1 [ expectListSize 0 ]

        -- can use rewards with special transaction query param
        -- (ApiWithdrawRewards True)
        rTx <- request @(ApiTransaction n) ctx
            (Link.createTransaction' @'Shelley w (ApiWithdrawRewards True))
            Default (Json payload)
        verify rTx
            [ expectField #amount (.> (Quantity coin))
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            ]
        let totalAmtAfterWithdrawals = getFromResponse #amount rTx

        -- Rewards are have been consumed.
        eventually "Wallet has consumed rewards" $ do
            request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty
                >>= flip verify
                    [ expectField
                        (#balance . #getApiT . #reward)
                        (`shouldBe` (Quantity 0))
                    , expectField
                        (#balance . #getApiT . #available)
                        (.> previousBalance)
                    ]

        eventually "There's at least one transaction with a withdrawal" $ do
            rWithdrawal <- request @(ApiTransaction n) ctx
                (Link.getTransaction @'Shelley w
                    (getFromResponse Prelude.id rTx))
                Default Empty
            verify rWithdrawal
                [ expectResponseCode HTTP.status200
                , expectField #withdrawals (`shouldSatisfy` (not . null))
                , expectField #amount (`shouldBe` totalAmtAfterWithdrawals)
                ]
            rw2 <- request @[ApiTransaction n] ctx
                (Link.listTransactions' @'Shelley w (Just 1)
                    Nothing Nothing Nothing)
                Default Empty
            verify rw2 [ expectListSize 1 ]

        -- Quit delegation altogether.
        quitStakePool @n ctx (w, fixturePassphrase) >>= flip verify
            [ expectResponseCode HTTP.status202
            , expectField (#status . #getApiT) (`shouldBe` Pending)
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            ]
        eventually "Certificates are inserted after quiting a pool" $ do
            let ep = Link.listTransactions @'Shelley w
            request @[ApiTransaction n] ctx ep Default Empty >>= flip verify
                [ expectListField 0
                    (#direction . #getApiT) (`shouldBe` Outgoing)
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
        pool:_ <- map (view #id) . snd <$> unsafeRequest @[ApiStakePool]
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

    it "STAKE_POOLS_QUIT_02 - Passphrase must be correct to quit" $ \ctx -> do
        w <- fixtureWallet ctx
        pool:_ <- map (view #id) . snd <$> unsafeRequest @[ApiStakePool]
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

        pool1:pool2:_ <- map (view #id) . snd
            <$> unsafeRequest @[ApiStakePool] ctx
                (Link.listStakePools arbitraryStake) Empty

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
        pool:_ <- map (view #id) . snd <$> unsafeRequest @[ApiStakePool]
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
        pool:_ <- map (view #id) . snd <$> unsafeRequest @[ApiStakePool]
            ctx (Link.listStakePools arbitraryStake) Empty

        eventually "wallet join a pool" $ do
            joinStakePool @n ctx pool (w, fixturePassphrase) >>= flip verify
                [ expectResponseCode HTTP.status202
                , expectField (#status . #getApiT) (`shouldBe` Pending)
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                ]

    describe "STAKE_POOLS_JOIN_01x - Fee boundary values" $ do
        it "STAKE_POOLS_JOIN_01x - \
            \I can join if I have just the right amount" $ \ctx -> do
            w <- fixtureWalletWith @n ctx [costOfJoining ctx + depositAmt ctx]
            pool:_ <- map (view #id) . snd <$> unsafeRequest @[ApiStakePool]
                ctx (Link.listStakePools arbitraryStake) Empty
            joinStakePool @n ctx pool (w, fixturePassphrase)>>= flip verify
                [ expectResponseCode HTTP.status202
                , expectField (#status . #getApiT) (`shouldBe` Pending)
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                ]

        it "STAKE_POOLS_JOIN_01x - \
           \I cannot join if I have not enough fee to cover" $ \ctx -> do
            w <- fixtureWalletWith @n ctx [costOfJoining ctx + depositAmt ctx - 1]
            pool:_ <- map (view #id) . snd <$> unsafeRequest @[ApiStakePool]
                ctx (Link.listStakePools arbitraryStake) Empty
            joinStakePool @n ctx pool (w, fixturePassphrase) >>= flip verify
                [ expectResponseCode HTTP.status403
                , expectErrorMessage (errMsg403DelegationFee 1)
                ]

    describe "STAKE_POOLS_QUIT_01x - Fee boundary values" $ do

        it "STAKE_POOLS_QUIT_01x - \
            \I can quit if I have enough to cover fee" $ \ctx -> do
            let initBalance =
                    [ costOfJoining ctx
                    + depositAmt ctx
                    + costOfQuitting ctx
                    + costOfChange ctx
                    ]
            w <- fixtureWalletWith @n ctx initBalance

            pool:_ <- map (view #id) . snd <$> unsafeRequest @[ApiStakePool]
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
                            (`shouldSatisfy` (== (Quantity (depositAmt ctx))))
                    , expectField
                        (#balance . #getApiT . #available)
                            (`shouldSatisfy` (== (Quantity (depositAmt ctx))))
                    ]

        it "STAKE_POOLS_QUIT_01x - \
            \I cannot quit if I have not enough to cover fees" $ \ctx -> do
            let initBalance = [ costOfJoining ctx + depositAmt ctx ]
            w <- fixtureWalletWith @n ctx initBalance

            pool:_ <- map (view #id) . snd <$> unsafeRequest @[ApiStakePool]
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

    let listPools ctx stake = request @[ApiStakePool] @IO ctx
            (Link.listStakePools stake) Default Empty

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

                let getRetirementEpoch :: ApiStakePool -> Maybe EpochNo
                    getRetirementEpoch =
                        fmap (view (#epochNumber . #getApiT))
                        .
                        view #retirement

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
                let pools = either (error . show) Prelude.id $ snd r

                -- To ignore the ordering of the pools, we use Set.
                setOf pools (view #cost)
                    `shouldBe` Set.singleton (Quantity 0)

                setOf pools (view #margin)
                    `shouldBe`
                    Set.singleton
                        (Quantity $ unsafeMkPercentage 0.1)

                setOf pools (view #pledge)
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
                    , expectField Prelude.id $ \pools -> do
                        let metadataActual = Set.fromList $
                                mapMaybe (fmap getApiT . view #metadata) pools
                        metadataActual
                            `shouldSatisfy` (`Set.isSubsetOf` metadataPossible)
                    ]

        it "contains and is sorted by non-myopic-rewards" $ \ctx -> do
            eventually "eventually shows non-zero rewards" $ do
                Right pools@[pool1,_pool2,pool3] <-
                    snd <$> listPools ctx arbitraryStake
                let rewards = view (#metrics . #nonMyopicMemberRewards)
                print (rewards <$> pools) -- FIXME temporary
                (rewards <$> pools) `shouldBe`
                    (rewards <$> sortOn (Down . rewards) pools)
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
        r <- request @[ApiStakePool] @IO ctx
            (Link.listStakePools Nothing) Default Empty
        expectResponseCode HTTP.status400 r

    it "STAKE_POOLS_LIST_06 - \
        \NonMyopicMemberRewards are 0 when stake is 0" $ \ctx -> do
        pendingWith "This assumption seems false, for some reasons..."
        let stake = Just $ Coin 0
        r <- request @[ApiStakePool] @IO ctx (Link.listStakePools stake)
            Default Empty
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
