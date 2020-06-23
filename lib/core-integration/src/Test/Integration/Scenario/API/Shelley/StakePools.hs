{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
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
    , DecodeAddress
    , EncodeAddress
    , WalletStyle (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( PaymentAddress )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey )
import Cardano.Wallet.Primitive.Types
    ( Coin (..)
    , Direction (..)
    , PoolId (..)
    , StakePoolMetadata (..)
    , StakePoolTicker (..)
    , TxStatus (..)
    )
import Cardano.Wallet.Unsafe
    ( unsafeMkPercentage )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.List
    ( sortOn )
import Data.Maybe
    ( mapMaybe )
import Data.Quantity
    ( Quantity (..) )
import Data.Text.Class
    ( toText )
import Test.Hspec
    ( SpecWith, describe, it, shouldBe, xit )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , TxDescription (..)
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
    , mkEpochInfo
    , notDelegating
    , quitStakePool
    , request
    , unsafeRequest
    , verify
    , waitForNextEpoch
    , walletId
    , (.<=)
    , (.>)
    )
import Test.Integration.Framework.TestData
    ( errMsg403DelegationFee
    , errMsg403NotDelegating
    , errMsg403PoolAlreadyJoined
    , errMsg403WrongPass
    , errMsg404NoSuchPool
    , errMsg404NoWallet
    )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Data.ByteString as BS
import qualified Network.HTTP.Types.Status as HTTP


spec :: forall n t.
    ( DecodeAddress n
    , EncodeAddress n
    , PaymentAddress n ShelleyKey
    ) => SpecWith (Context t)
spec = do
    it "STAKE_POOLS_JOIN_01 - Cannot join non-existant wallet" $ \ctx -> do
        w <- emptyWallet ctx
        let wid = w ^. walletId
        _ <- request @ApiWallet ctx
            (Link.deleteWallet @'Shelley w) Default Empty
        let poolIdAbsent = PoolId $ BS.pack $ replicate 32 1
        r <- joinStakePool @n ctx (ApiT poolIdAbsent) (w, fixturePassphrase)
        expectResponseCode HTTP.status404 r
        expectErrorMessage (errMsg404NoWallet wid) r

    it "STAKE_POOLS_JOIN_01 - Cannot join non-existant stakepool" $ \ctx -> do
        w <- fixtureWallet ctx
        let poolIdAbsent = PoolId $ BS.pack $ replicate 32 1
        r <- joinStakePool @n ctx (ApiT poolIdAbsent) (w, fixturePassphrase)
        expectResponseCode HTTP.status404 r
        expectErrorMessage (errMsg404NoSuchPool (toText poolIdAbsent)) r

    it "STAKE_POOLS_JOIN_01 - Cannot join existant stakepool with wrong password" $ \ctx -> do
        w <- fixtureWallet ctx
        pool:_ <- map (view #id) . snd
            <$> unsafeRequest @[ApiStakePool] ctx (Link.listStakePools arbitraryStake) Empty
        joinStakePool @n ctx pool (w, "Wrong Passphrase") >>= flip verify
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403WrongPass
            ]

    it "STAKE_POOLS_JOIN_02 - Cannot join already joined stake pool" $ \ctx -> do
        w <- fixtureWallet ctx
        pool:_ <- map (view #id) . snd
            <$> unsafeRequest @[ApiStakePool] ctx (Link.listStakePools arbitraryStake) Empty
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
            , expectErrorMessage (errMsg403PoolAlreadyJoined $ toText $ getApiT pool)
            ]

    it "STAKE_POOLS_QUIT_02 - Passphrase must be correct to quit" $ \ctx -> do
        w <- fixtureWallet ctx
        pool:_ <- map (view #id) . snd
            <$> unsafeRequest @[ApiStakePool] ctx (Link.listStakePools arbitraryStake) Empty
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

    it "STAKE_POOL_NEXT_02/STAKE_POOLS_QUIT_01 - Cannot quit when active: not_delegating"
        $ \ctx -> do
        w <- fixtureWallet ctx
        quitStakePool @n ctx (w, fixturePassphrase) >>= flip verify
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403NotDelegating
            ]

    it "STAKE_POOLS_JOIN_01 - Can rejoin another stakepool" $ \ctx -> do
        w <- fixtureWallet ctx

        -- make sure we are at the beginning of new epoch
        (currentEpoch, sp) <- getSlotParams ctx
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

        request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
            [ expectField #delegation
                (`shouldBe` notDelegating
                    [ (Just pool1, mkEpochInfo (currentEpoch + 3) sp)
                    ]
                )
            ]
        eventually "Wallet is delegating to p1" $ do
            request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
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
            request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
                [ expectField #delegation (`shouldBe` delegating pool2 [])
                ]

        --quiting
        quitStakePool @n ctx (w, fixturePassphrase) >>= flip verify
            [ expectResponseCode HTTP.status202
            , expectField (#status . #getApiT) (`shouldBe` Pending)
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            ]

        -- Wait for the certificate to be inserted
        eventually "Certificates are inserted" $ do
            let ep = Link.listTransactions @'Shelley w
            request @[ApiTransaction n] ctx ep Default Empty >>= flip verify
                [ expectListField 2
                    (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectListField 2
                    (#status . #getApiT) (`shouldBe` InLedger)
                ]

        eventually "Wallet is not delegating" $ do
            request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
                [ expectField #delegation (`shouldBe` notDelegating [])
                ]

    xit "STAKE_POOLS_JOIN_04 - Rewards accumulate and stop" $ \ctx -> do
        w <- fixtureWallet ctx
        pool:_ <- map (view #id) . snd
            <$> unsafeRequest @[ApiStakePool] ctx (Link.listStakePools arbitraryStake) Empty
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

        -- Wait for money to flow
        eventually "Wallet gets rewards" $ do
            request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
                [ expectField (#balance . #getApiT . #reward)
                    (.> (Quantity 0))
                ]

        -- Quit a pool
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
                ]

        -- Check that rewards have stopped flowing.
        waitForNextEpoch ctx
        waitForNextEpoch ctx
        reward <- getFromResponse (#balance . #getApiT . #reward) <$>
            request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty

        waitForNextEpoch ctx
        request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
            [ expectField
                    (#balance . #getApiT . #reward)
                    (`shouldBe` reward)
            ]

    describe "STAKE_POOLS_JOIN_01x - Fee boundary values" $ do
        it "STAKE_POOLS_JOIN_01x - \
            \I can join if I have just the right amount" $ \ctx -> do
            let (fee, _) = ctx ^. #_feeEstimator $ DelegDescription 1 0 1
            w <- fixtureWalletWith @n ctx [fee]

            pool:_ <- map (view #id) . snd
                <$> unsafeRequest @[ApiStakePool] ctx (Link.listStakePools arbitraryStake) Empty

            joinStakePool @n ctx pool (w, passwd)>>= flip verify
                [ expectResponseCode HTTP.status202
                , expectField (#status . #getApiT) (`shouldBe` Pending)
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                ]

        it "STAKE_POOLS_JOIN_01x - \
           \I cannot join if I have not enough fee to cover" $ \ctx -> do
            let (fee, _) = ctx ^. #_feeEstimator $ DelegDescription 1 0 1
            w <- fixtureWalletWith @n ctx [fee - 1]
            pool:_ <- map (view #id) . snd
                <$> unsafeRequest @[ApiStakePool] ctx (Link.listStakePools arbitraryStake) Empty
            joinStakePool @n ctx pool (w, passwd) >>= flip verify
                [ expectResponseCode HTTP.status403
                , expectErrorMessage (errMsg403DelegationFee 1)
                ]

    describe "STAKE_POOLS_QUIT_01x - Fee boundary values" $ do
        it "STAKE_POOLS_QUIT_01x - \
            \I can quit if I have enough to cover fee" $ \ctx -> do
            let (feeJoin, _) = ctx ^. #_feeEstimator $ DelegDescription 1 1 1
            let (feeQuit, _) = ctx ^. #_feeEstimator $ DelegDescription 1 0 1
            let initBalance = [feeJoin + feeQuit]
            w <- fixtureWalletWith @n ctx initBalance
            pool:_ <- map (view #id) . snd
                <$> unsafeRequest @[ApiStakePool] ctx (Link.listStakePools arbitraryStake) Empty
            joinStakePool @n ctx pool (w, passwd) >>= flip verify
                [ expectResponseCode HTTP.status202
                , expectField (#status . #getApiT) (`shouldBe` Pending)
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                ]

            eventually "Wallet is delegating to p1" $ do
                request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
                    [ expectField #delegation (`shouldBe` delegating pool [])
                    ]

            quitStakePool @n ctx (w, passwd) >>= flip verify
                [ expectResponseCode HTTP.status202
                ]
            eventually "Wallet is not delegating and has balance = 0" $ do
                request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
                    [ expectField #delegation (`shouldBe` notDelegating [])
                    -- balance is 0 because the rest was used for fees
                    , expectField
                        (#balance . #getApiT . #total) (`shouldBe` Quantity 0)
                    , expectField
                        (#balance . #getApiT . #available) (`shouldBe` Quantity 0)
                    ]

        it "STAKE_POOLS_QUIT_01x - \
            \I cannot quit if I have not enough fee to cover" $ \ctx -> do
            let (feeJoin, _) = ctx ^. #_feeEstimator $ DelegDescription 1 1 1
            let (feeQuit, _) = ctx ^. #_feeEstimator $ DelegDescription 0 0 1
            let initBalance = [feeJoin+1]
            w <- fixtureWalletWith @n ctx initBalance

            pool:_ <- map (view #id) . snd
                <$> unsafeRequest @[ApiStakePool] ctx (Link.listStakePools arbitraryStake) Empty

            joinStakePool @n ctx pool (w, passwd) >>= flip verify
                [ expectResponseCode HTTP.status202
                , expectField (#status . #getApiT) (`shouldBe` Pending)
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                ]

            eventually "Wallet is delegating to p1" $ do
                request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
                    [ expectField #delegation (`shouldBe` delegating pool [])
                    ]
            quitStakePool @n ctx (w, passwd) >>= flip verify
                [ expectResponseCode HTTP.status403
                , expectErrorMessage (errMsg403DelegationFee (feeQuit - 1))
                ]

    it "STAKE_POOLS_ESTIMATE_FEE_01x - edge-case fee in-between coeff" $ \ctx -> do
        let (feeMin, _) = ctx ^. #_feeEstimator $ DelegDescription 1 0 1
        w <- fixtureWalletWith @n ctx [feeMin + 1, feeMin + 1]
        let (fee, _) = ctx ^. #_feeEstimator $ DelegDescription 2 1 1
        delegationFee ctx w >>= flip verify
            [ expectResponseCode HTTP.status200
            , expectField (#estimatedMin . #getQuantity) (.<= fee)
            ]

    it "STAKE_POOLS_ESTIMATE_FEE_02 - \
        \empty wallet cannot estimate fee" $ \ctx -> do
        w <- emptyWallet ctx
        let (fee, _) = ctx ^. #_feeEstimator $ DelegDescription 0 0 1
        delegationFee ctx w >>= flip verify
            [ expectResponseCode HTTP.status403
            , expectErrorMessage $ errMsg403DelegationFee fee
            ]

    let listPools ctx = request @[ApiStakePool] @IO ctx (Link.listStakePools arbitraryStake) Default Empty
    describe "STAKE_POOLS_LIST_01 - List stake pools" $ do
        it "immediately has non-zero saturation & stake" $ \ctx -> do
            r <- listPools ctx
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
                , expectListField 0
                    (#metrics . #saturation) (.> 0)
                , expectListField 1
                    (#metrics . #saturation) (.> 0)
                , expectListField 2
                    (#metrics . #saturation) (.> 0)
                ]

        it "eventually has correct margin and cost" $ \ctx -> do
            eventually "pool worker finds the certificate" $ do
                r <- listPools ctx
                expectResponseCode HTTP.status200 r
                verify r
                    [ expectListField 0
                        (#cost) (`shouldBe` Just (Quantity 0))
                    , expectListField 1
                        (#cost) (`shouldBe` Just (Quantity 0))
                    , expectListField 2
                        (#cost) (`shouldBe` Just (Quantity 0))

                    , expectListField 0
                        #margin (`shouldBe` Just
                            (Quantity $ unsafeMkPercentage 0.1 ))
                    , expectListField 1
                        #margin (`shouldBe` Just
                            (Quantity $ unsafeMkPercentage 0.1 ))
                    , expectListField 2
                        #margin (`shouldBe` Just
                            (Quantity $ unsafeMkPercentage 0.1 ))

                    , expectListField 0
                        (#metrics . #producedBlocks) (.> Quantity 0)
                    , expectListField 1
                        (#metrics . #producedBlocks) (.> Quantity 0)
                    , expectListField 2
                        (#metrics . #producedBlocks) (.> Quantity 0)
                    -- TODO: Test that we have non-zero non-myopic member
                    -- rewards, and sort by it.
                    ]

        it "contains pool metadata" $ \ctx -> do
            eventually "metadata is fetched" $ do
                r <- listPools ctx
                let metadataList =
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
                        ]

                verify r
                    [ expectListSize 3
                    , expectField id $ \pools ->
                        -- To ignore the arbitrary order,
                        -- we sort on the names before comparing
                        sortOn name ( mapMaybe (fmap getApiT . view #metadata) pools)
                        `shouldBe` metadataList
                    ]

    it "STAKE_POOLS_LIST_05 - Fails without query parameter" $ \ctx -> do
        r <- request @[ApiStakePool] @IO ctx
            (Link.listStakePools Nothing) Default Empty
        expectResponseCode HTTP.status400 r

    it "STAKE_POOLS_LIST_06 - NonMyopicMemberRewards are 0 when stake is 0" $ \ctx -> do
        let stake = Just $ Coin 0
        r <- request @[ApiStakePool] @IO ctx (Link.listStakePools stake) Default Empty
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
    arbitraryStake = Just $ ada 10000
      where ada = Coin . (1000*1000*)

    passwd = "Secure Passphrase"
