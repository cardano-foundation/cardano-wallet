{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Integration.Jormungandr.Scenario.API.StakePools
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiFee
    , ApiStakePool
    , ApiT (..)
    , ApiTransaction
    , ApiWallet
    , WalletStyle (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..)
    , PassphraseMaxLength (..)
    , PassphraseMinLength (..)
    )
import Cardano.Wallet.Primitive.Types
    ( Direction (..), PoolId (..), TxStatus (..), WalletDelegation (..) )
import Control.Arrow
    ( second )
import Control.Monad
    ( forM_ )
import Data.Functor.Identity
    ( Identity (..) )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.List
    ( find, sortOn )
import Data.Maybe
    ( isJust, isNothing )
import Data.Ord
    ( Down (..) )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text.Class
    ( fromText, toText )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( SpecWith, describe, it, shouldBe )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , TxDescription (..)
    , amount
    , blocks
    , delegation
    , delegationFee
    , direction
    , emptyRandomWallet
    , emptyWallet
    , eventually
    , eventuallyUsingDelay_
    , eventually_
    , expectErrorMessage
    , expectFieldEqual
    , expectFieldSatisfy
    , expectListItemFieldEqual
    , expectListItemFieldSatisfy
    , expectListSizeEqual
    , expectResponseCode
    , faucetUtxoAmt
    , feeEstimator
    , fixturePassphrase
    , fixtureRandomWallet
    , fixtureWallet
    , fixtureWalletWith
    , getFromResponse
    , joinStakePool
    , json
    , metrics
    , quitStakePool
    , request
    , stake
    , status
    , unsafeRequest
    , verify
    , waitForNextEpoch
    , walletId
    , withMethod
    )
import Test.Integration.Framework.TestData
    ( errMsg403DelegationFee
    , errMsg403PoolAlreadyJoined
    , errMsg403WrongPass
    , errMsg403WrongPool
    , errMsg404NoEndpoint
    , errMsg404NoSuchPool
    , errMsg404NoWallet
    , errMsg405
    , errMsg406
    , errMsg415
    , falseWalletIds
    , getHeaderCases
    )
import Test.Integration.Jormungandr.Fixture
    ( OwnerIdentity (..), registerStakePool )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Network.HTTP.Types.Status as HTTP

spec :: forall t n. (n ~ 'Testnet) => SpecWith (Context t)
spec = do
    it "STAKE_POOLS_LIST_01 - List stake pools" $ \ctx -> do
        eventually_ $ do
            r <- request @[ApiStakePool] ctx Link.listStakePools Default Empty
            expectResponseCode HTTP.status200 r
            -- With the current genesis.yaml we have 3 pools with 1 lovelace,
            -- and an epoch length of 3.
            --
            -- For some reason, the first pool (the node we run), produces
            -- blocks in 100% of the /slots/. This means it will have produced
            -- either 1 or 2 blocks in the current epoch.
            verify (sortByPerformance r)
                [ expectListSizeEqual 3

                , expectListItemFieldSatisfy 0
                    #metadata ((== Just "Genesis Pool") . fmap (view #name))
                , expectListItemFieldSatisfy 1
                    #metadata ((== Just "Genesis Pool") . fmap (view #name))
                , expectListItemFieldSatisfy 2
                    #metadata ((== Just "Genesis Pool") . fmap (view #name))

                , expectListItemFieldSatisfy 0
                    #cost (== (Quantity 0))
                , expectListItemFieldSatisfy 1
                    #cost (== (Quantity 0))
                , expectListItemFieldSatisfy 2
                    #cost (== (Quantity 0))

                , expectListItemFieldSatisfy 0
                    #margin (== (Quantity minBound))
                , expectListItemFieldSatisfy 1
                    #margin (== (Quantity minBound))
                , expectListItemFieldSatisfy 2
                    #margin (== (Quantity minBound))

                , expectListItemFieldEqual 0
                    (metrics . stake) 1
                , expectListItemFieldEqual 1
                    (metrics . stake) 1
                , expectListItemFieldEqual 2
                    (metrics . stake) 1

                , expectListItemFieldSatisfy 0
                    (metrics . blocks) (> 1)
                , expectListItemFieldEqual 1
                    (metrics . blocks) 0
                , expectListItemFieldEqual 2
                    (metrics . blocks) 0

                , expectListItemFieldSatisfy 0
                    #apparentPerformance (> 0)
                , expectListItemFieldSatisfy 1
                    #apparentPerformance (== 0)
                , expectListItemFieldSatisfy 2
                    #apparentPerformance (== 0)

                , expectListItemFieldSatisfy 0
                    #desirability (>= 0)
                , expectListItemFieldSatisfy 1
                    #desirability (>= 0)
                , expectListItemFieldSatisfy 2
                    #desirability (>= 0)

                , expectListItemFieldSatisfy 0
                    #saturation (>= 0)
                , expectListItemFieldSatisfy 1
                    #saturation (>= 0)
                , expectListItemFieldSatisfy 2
                    #saturation (>= 0)
                ]

    it "STAKE_POOLS_LIST_02 - May fail on epoch boundaries" $ \ctx -> do
        -- We should be able to catch the stake-pool data in an un-synced state
        -- when we enter into a new epoch. The results should then be
        -- unavailible.
        --
        -- This might take a few tries (epoch changes), so it is only feasible
        -- to test with very short epochs.
        let ms = 1000
        eventuallyUsingDelay_ (50*ms) $ do
            r <- request @[ApiStakePool] ctx Link.listStakePools Default Empty
            verify r
                [ expectResponseCode HTTP.status503
                , expectErrorMessage
                    "I can't list stake pools yet because I need to scan the \
                    \blockchain for metrics first. I'm at"
                ]

    describe "STAKE_POOLS_LIST_03 - v2/stake-pools - Methods Not Allowed" $ do
        let methods = ["POST", "PUT", "DELETE", "CONNECT", "TRACE", "OPTIONS"]
        forM_ methods $ \method -> it (show method) $ \ctx -> do
            r <- request @ApiStakePool ctx
                    (method, "v2/stake-pools") Default Empty
            expectResponseCode @IO HTTP.status405 r
            expectErrorMessage errMsg405 r

    it "STAKE_POOLS_LIST_04 - Discovers new pools when they are registered" $ \ctx -> do
        let nWithMetadata = length . filter (isJust . view #metadata)
        let nWithoutMetadata = length . filter (isNothing . view #metadata)

        (_, pools) <- eventually $
            unsafeRequest @[ApiStakePool] ctx Link.listStakePools Empty

        (poolIdA, poolAOwner)  <- registerStakePool ctx WithMetadata
        (poolIdB, _poolBOwner) <- registerStakePool ctx WithoutMetadata
        (poolIdC, poolCOwner)  <- registerStakePool ctx WithMetadata

        waitForNextEpoch ctx
        (_, pools') <- eventually $
            unsafeRequest @[ApiStakePool] ctx Link.listStakePools Empty

        nWithoutMetadata pools' `shouldBe` nWithoutMetadata pools + 1
        nWithMetadata pools' `shouldBe` nWithMetadata pools + 2

        let (Just poolA) = find ((== ApiT poolIdA) . view #id) pools'
        fmap (view #owner) (poolA ^. #metadata) `shouldBe` Just poolAOwner

        let (Just poolB) = find ((== ApiT poolIdB) . view #id) pools'
        (poolB ^. #metadata) `shouldBe` Nothing

        let (Just poolC) = find ((== ApiT poolIdC) . view #id) pools'
        fmap (view #owner) (poolC ^. #metadata) `shouldBe` Just poolCOwner

    it "STAKE_POOLS_JOIN_01 - Can join a stakepool" $ \ctx -> do
        w <- fixtureWallet ctx
        (_, p:_) <- eventually $
            unsafeRequest @[ApiStakePool] ctx Link.listStakePools Empty

        -- Join a pool
        joinStakePool ctx (p ^. #id) (w, fixturePassphrase) >>= flip verify
            [ expectResponseCode HTTP.status202
            , expectFieldEqual status Pending
            , expectFieldEqual direction Outgoing
            ]

        -- Wait for the certificate to be inserted
        eventually $ do
            let ep = Link.listTransactions @'Shelley w
            request @[ApiTransaction n] ctx ep Default Empty >>= flip verify
                [ expectListItemFieldEqual 0 direction Outgoing
                , expectListItemFieldEqual 0 status InLedger
                ]

    it "STAKE_POOLS_JOIN_01 - Controlled stake increases when joining" $ \ctx -> do
        w <- fixtureWallet ctx
        (_, Right (p:_)) <- eventually $ sortByPerformance <$>
            request @[ApiStakePool] ctx Link.listStakePools Default Empty

        -- Join a pool
        joinStakePool ctx (p ^. #id) (w, fixturePassphrase) >>= flip verify
            [ expectResponseCode HTTP.status202
            , expectFieldEqual status Pending
            , expectFieldEqual direction Outgoing
            ]

        -- Wait for the certificate to be inserted
        eventually $ do
            let ep = Link.listTransactions @'Shelley w
            request @[ApiTransaction n] ctx ep Default Empty >>= flip verify
                [ expectListItemFieldEqual 0 direction Outgoing
                , expectListItemFieldEqual 0 status InLedger
                ]

        let (fee, _) = ctx ^. feeEstimator $ DelegDescription 1 1 1
        let existingPoolStake = getQuantity $ p ^. #metrics . #controlledStake
        let contributedStake = faucetUtxoAmt - fee
        eventually $ do
            v <- request @[ApiStakePool] ctx Link.listStakePools Default Empty
            verify (sortByPerformance v)
                [ expectListItemFieldSatisfy 0 (metrics . stake)
                    (> (existingPoolStake + contributedStake))
                    -- No exact equality since the delegation from previous
                    -- tests may take effect.
                ]

    it "STAKE_POOLS_JOIN_04 - Rewards accumulate and stop" $ \ctx -> do
        w <- fixtureWallet ctx
        (_, p:_) <- eventually $
            unsafeRequest @[ApiStakePool] ctx Link.listStakePools Empty

        -- Join a pool
        joinStakePool ctx (p ^. #id) (w, fixturePassphrase) >>= flip verify
            [ expectResponseCode HTTP.status202
            , expectFieldEqual status Pending
            , expectFieldEqual direction Outgoing
            ]
        eventually $ do
            let ep = Link.listTransactions @'Shelley w
            request @[ApiTransaction n] ctx ep Default Empty >>= flip verify
                [ expectListItemFieldEqual 0 direction Outgoing
                , expectListItemFieldEqual 0 status InLedger
                ]

        -- Wait for money to flow
        eventually $ do
            request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
                [ expectFieldSatisfy (#balance . #getApiT . #reward) (> (Quantity 0))
                ]

        -- Quit a pool
        quitStakePool ctx (p ^. #id) (w, fixturePassphrase) >>= flip verify
            [ expectResponseCode HTTP.status202
            , expectFieldEqual status Pending
            , expectFieldEqual direction Outgoing
            ]
        eventually $ do
            let ep = Link.listTransactions @'Shelley w
            request @[ApiTransaction n] ctx ep Default Empty >>= flip verify
                [ expectListItemFieldEqual 0 direction Outgoing
                , expectListItemFieldEqual 0 status InLedger
                , expectListItemFieldEqual 1 direction Outgoing
                , expectListItemFieldEqual 1 status InLedger
                ]

        waitForNextEpoch ctx
        waitForNextEpoch ctx
        reward <- getFromResponse (#balance . #getApiT . #reward) <$>
            request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty

        waitForNextEpoch ctx
        request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
            [ expectFieldSatisfy
                    (#balance . #getApiT . #reward)
                    (== reward)
            ]

    it "STAKE_POOLS_JOIN_04 -\
        \Delegate, stop in the next epoch, and still earn rewards" $ \ctx -> do
        w <- fixtureWallet ctx
        (_, p1:_) <- eventually $
            unsafeRequest @[ApiStakePool] ctx Link.listStakePools Empty

        joinStakePool ctx (p1 ^. #id) (w, fixturePassphrase) >>= flip verify
            [ expectResponseCode HTTP.status202
            ]

        waitForNextEpoch ctx

        quitStakePool ctx (p1 ^. #id) (w, fixturePassphrase) >>= flip verify
            [ expectResponseCode HTTP.status202
            ]

        reward <- eventually $ do
            r <- request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty
            verify r
                [ expectFieldSatisfy
                        (#balance . #getApiT . #reward)
                        (> (Quantity 0))
                ]
            pure $ getFromResponse (#balance . #getApiT . #reward) r

        waitForNextEpoch ctx

        eventually $ do
            request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
                [ expectFieldSatisfy
                        (#balance . #getApiT . #reward)
                        (== reward)
                ]

    it "STAKE_POOLS_JOIN_01 - I can join another stake-pool after previously \
        \ joining another one" $ \ctx -> do
        w <- fixtureWallet ctx
        (_, p1:p2:_) <- eventually $
            unsafeRequest @[ApiStakePool] ctx Link.listStakePools Empty

        -- Join pool p1
        joinStakePool ctx (p1 ^. #id) (w, fixturePassphrase) >>= flip verify
            [ expectResponseCode HTTP.status202
            , expectFieldEqual status Pending
            , expectFieldEqual direction Outgoing
            ]
        eventually $ do
            let ep = Link.listTransactions @'Shelley w
            request @[ApiTransaction n] ctx ep Default Empty >>= flip verify
                [ expectListItemFieldEqual 0 direction Outgoing
                , expectListItemFieldEqual 0 status InLedger
                ]
        request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
            [ expectFieldEqual delegation (Delegating (p1 ^. #id))
            ]

        -- Join pool p2
        joinStakePool ctx (p2 ^. #id) (w, fixturePassphrase) >>= flip verify
            [ expectResponseCode HTTP.status202
            , expectFieldEqual status Pending
            , expectFieldEqual direction Outgoing
            ]
        eventually $ do
            let ep = Link.listTransactions @'Shelley w
            (code, txs) <- request @[ApiTransaction n] ctx ep Default Empty
            let onlyPending = \case
                    e@Left{} -> e
                    Right xs -> Right $
                        filter (\t -> t ^. #status == ApiT Pending) xs
            verify (code, (onlyPending txs))
                [ expectListSizeEqual 0
                ]
        request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
            [ expectFieldEqual delegation (Delegating (p2 ^. #id))
            ]

    describe "STAKE_POOLS_JOIN_01x - Fee boundary values" $ do
        it "STAKE_POOLS_JOIN_01x - \
            \I can join if I have just the right amount" $ \(ctx) -> do
            (_, p:_) <- eventually $
                unsafeRequest @[ApiStakePool] ctx Link.listStakePools Empty
            let (fee, _) = ctx ^. feeEstimator $ DelegDescription 1 0 1
            w <- fixtureWalletWith ctx [fee]
            joinStakePool ctx (p ^. #id) (w, "Secure Passphrase")>>= flip verify
                [ expectResponseCode HTTP.status202
                , expectFieldEqual status Pending
                , expectFieldEqual direction Outgoing
                ]

        it "STAKE_POOLS_JOIN_01x - \
            \I cannot join if I have not enough fee to cover" $ \ctx -> do
            (_, p:_) <- eventually $
                unsafeRequest @[ApiStakePool] ctx Link.listStakePools Empty
            let (fee, _) = ctx ^. feeEstimator $ DelegDescription 1 0 1
            w <- fixtureWalletWith ctx [fee - 1]
            r <- joinStakePool ctx (p ^. #id) (w, "Secure Passphrase")
            expectResponseCode HTTP.status403 r
            expectErrorMessage (errMsg403DelegationFee 1) r

        it "STAKE_POOLS_JOIN_01x - I cannot join stake-pool with 0 balance" $ \ctx -> do
            (_, p:_) <- eventually $
                unsafeRequest @[ApiStakePool] ctx Link.listStakePools Empty
            w <- emptyWallet ctx
            let (fee, _) = ctx ^. feeEstimator $ DelegDescription 0 0 1
            r <- joinStakePool ctx (p ^. #id) (w, "Secure Passphrase")
            expectResponseCode HTTP.status403 r
            expectErrorMessage (errMsg403DelegationFee fee) r

    describe "STAKE_POOLS_QUIT_01x - Fee boundary values" $ do
        it "STAKE_POOLS_QUIT_01x - \
            \I can quit if I have enough to cover fee" $ \(ctx) -> do
            let (feeJoin, _) = ctx ^. feeEstimator $ DelegDescription 1 1 1
            let (feeQuit, _) = ctx ^. feeEstimator $ DelegDescription 1 0 1
            let initBalance = [feeJoin + feeQuit]
            (w, p) <- joinStakePoolWithWalletBalance ctx initBalance
            rq <- quitStakePool ctx (p ^. #id) (w, "Secure Passphrase")
            expectResponseCode HTTP.status202 rq
            eventually $ do
                request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
                    [ expectFieldEqual delegation (NotDelegating)
                    -- balance is 0 because the rest was used for fees
                    , expectFieldEqual (#balance . #getApiT . #total) (Quantity 0)
                    , expectFieldEqual (#balance . #getApiT . #available) (Quantity 0)
                    ]

        it "STAKE_POOLS_QUIT_01x - \
            \I cannot quit if I have not enough fee to cover" $ \ctx -> do
            let (feeJoin, _) = ctx ^. feeEstimator $ DelegDescription 1 1 1
            let (feeQuit, _) = ctx ^. feeEstimator $ DelegDescription 0 0 1
            let initBalance = [feeJoin+1]
            (w, p) <- joinStakePoolWithWalletBalance ctx initBalance
            rq <- quitStakePool ctx (p ^. #id) (w, "Secure Passphrase")
            verify rq
                [ expectResponseCode HTTP.status403
                , expectErrorMessage (errMsg403DelegationFee (feeQuit - 1))
                ]

    it "STAKE_POOLS_JOIN_01 - I cannot rejoin the same stake-pool" $ \ctx -> do
        let (fee, _) = ctx ^. feeEstimator $ DelegDescription 1 1 1
        (w, p) <- joinStakePoolWithWalletBalance ctx [10*fee]

        -- Join again
        r <- joinStakePool ctx (p ^. #id) (w, fixturePassphrase)
        expectResponseCode HTTP.status403 r
        let poolId = toText $ getApiT $ p ^. #id
        expectErrorMessage (errMsg403PoolAlreadyJoined poolId) r

    it "STAKE_POOLS_JOIN_01 - \
        \I definitely can quit and join another" $ \ctx -> do
        (_, p1:p2:_) <- eventually $
            unsafeRequest @[ApiStakePool] ctx Link.listStakePools Empty
        w <- fixtureWallet ctx

        r <- joinStakePool ctx (p1 ^. #id) (w, fixturePassphrase)
        expectResponseCode HTTP.status202 r
        eventually $ do
            request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
                [ expectFieldEqual delegation (Delegating (p1 ^. #id))
                ]

        r1q <- quitStakePool ctx (p1 ^. #id) (w, fixturePassphrase)
        expectResponseCode HTTP.status202 r1q
        eventually $ do
            request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
                [ expectFieldEqual delegation (NotDelegating)
                ]

        r2 <- joinStakePool ctx (p2 ^. #id) (w, fixturePassphrase)
        expectResponseCode HTTP.status202 r2
        eventually $ do
            request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
                [ expectFieldEqual delegation (Delegating (p2 ^. #id))
                ]

    it "STAKE_POOLS_JOIN_01 - Cannot join non-existant stakepool" $ \ctx -> do
        let poolIdAbsent = PoolId $ BS.pack $ replicate 32 0
        w <- emptyWallet ctx
        r <- joinStakePool ctx (ApiT poolIdAbsent) (w, "Secure Passphrase")
        expectResponseCode HTTP.status404 r
        expectErrorMessage (errMsg404NoSuchPool (toText poolIdAbsent)) r

    it "STAKE_POOLS_JOIN_01 - \
        \ If a wallet joins a stake pool, others are not affected" $ \ctx -> do
        (wA, wB) <- (,) <$> fixtureWallet ctx <*> fixtureWallet ctx
        (_, p:_) <- eventually $
            unsafeRequest @[ApiStakePool] ctx Link.listStakePools Empty

        -- Join a pool
        joinStakePool ctx (p ^. #id) (wA, fixturePassphrase) >>= flip verify
            [ expectResponseCode HTTP.status202
            , expectFieldEqual status Pending
            , expectFieldEqual direction Outgoing
            ]

        -- Wait for the certificate to be inserted
        eventually $ do
            let ep = Link.listTransactions @'Shelley wA
            request @[ApiTransaction n] ctx ep Default Empty >>= flip verify
                [ expectListItemFieldEqual 0 direction Outgoing
                , expectListItemFieldEqual 0 status InLedger
                ]

        -- Verify the wallet is now delegating
        request @ApiWallet ctx (Link.getWallet @'Shelley wA) Default Empty >>= flip verify
            [ expectFieldEqual delegation (Delegating (p ^. #id))
            ]

        -- Verify the other is NOT delegating
        request @ApiWallet ctx (Link.getWallet @'Shelley wB) Default Empty >>= flip verify
            [ expectFieldEqual delegation NotDelegating
            ]

    it "STAKE_POOLS_JOIN_02 - Passphrase must be correct to join" $ \ctx -> do
        (_, p:_) <- eventually $
            unsafeRequest @[ApiStakePool] ctx Link.listStakePools Empty
        w <- fixtureWallet ctx
        r <- joinStakePool ctx (p ^. #id) (w, "Incorrect Passphrase")
        expectResponseCode HTTP.status403 r
        expectErrorMessage errMsg403WrongPass r

    describe "STAKE_POOLS_JOIN/QUIT_02 -\
        \ Passphrase must have appropriate length" $ do

        let pMax = passphraseMaxLength (Proxy @"encryption")
        let pMin = passphraseMinLength (Proxy @"encryption")
        let tooShort =
                "passphrase is too short: expected at least 10 characters"
        let tooLong =
                "passphrase is too long: expected at most 255 characters"
        let tests =
                [ (tooLong, replicate (pMax + 1) '1')
                , (tooShort, replicate (pMin - 1) '1')
                ]
        let verifyIt ctx doStakePool pass expec = do
                (_, p:_) <- eventually $ do
                    unsafeRequest @[ApiStakePool] ctx Link.listStakePools Empty
                w <- emptyWallet ctx
                r <- doStakePool ctx (p ^. #id) (w, T.pack pass)
                expectResponseCode HTTP.status400 r
                expectErrorMessage expec r

        forM_ tests $ \(expec, passphrase) -> do
            it ("Join: " ++ expec) $ \ctx -> do
                verifyIt ctx joinStakePool passphrase expec

            it ("Quit: " ++ expec) $ \ctx -> do
                verifyIt ctx quitStakePool passphrase expec

    describe "STAKE_POOLS_JOIN/QUIT_02 - Passphrase must be text" $ do
        let verifyIt ctx sPoolEndp = do
                (_, p:_) <- eventually $
                    unsafeRequest @[ApiStakePool] ctx Link.listStakePools Empty
                w <- emptyWallet ctx
                let payload = Json [json| {
                        "passphrase": 123
                        } |]
                r <- request @(ApiTransaction n) ctx (sPoolEndp p w)
                        Default payload
                expectResponseCode HTTP.status400 r
                expectErrorMessage "expected Text, encountered Number" r
        it "Join" $ \ctx -> do
            verifyIt ctx Link.joinStakePool
        it "Quit" $ \ctx -> do
            verifyIt ctx Link.quitStakePool

    it "STAKE_POOLS_JOIN_03 - Byron wallet cannot join stake pool" $ \ctx -> do
        (_, p:_) <- eventually $
            unsafeRequest @[ApiStakePool] ctx Link.listStakePools Empty
        w <- emptyRandomWallet ctx
        r <- joinStakePool ctx (p ^. #id) (w, "Secure Passprase")
        expectResponseCode HTTP.status404 r

    -- NOTE
    -- This is only true because:
    --
    -- 1/ We are in Jörmungandr scenario were fees can be known exactly
    -- 2/ Fixture wallets are made of homogeneous UTxOs (all equal to the same
    -- value) and therefore, the random selection has no influence.
    it "STAKE_POOLS_ESTIMATE_FEE_01 - fee matches eventual cost" $ \ctx -> do
        (_, p:_) <- eventually $
            unsafeRequest @[ApiStakePool] ctx Link.listStakePools Empty
        w <- fixtureWallet ctx
        r <- delegationFee ctx w
        verify r
            [ expectResponseCode HTTP.status200
            ]
        let fee = getFromResponse amount r
        joinStakePool ctx (p ^. #id) (w, fixturePassphrase) >>= flip verify
            [ expectFieldEqual amount fee
            ]

    it "STAKE_POOLS_ESTIMATE_FEE_01x - edge-case fee in-between coeff" $ \ctx -> do
        let (feeMin, _) = ctx ^. feeEstimator $ DelegDescription 1 0 1
        w <- fixtureWalletWith ctx [feeMin + 1, feeMin + 1]
        r <- delegationFee ctx w
        let (fee, _) = ctx ^. feeEstimator $ DelegDescription 2 1 1
        verify r
            [ expectResponseCode HTTP.status200
            , expectFieldEqual amount fee
            ]

    it "STAKE_POOLS_ESTIMATE_FEE_02 - \
        \empty wallet cannot estimate fee" $ \ctx -> do
        w <- emptyWallet ctx
        let (fee, _) = ctx ^. feeEstimator $ DelegDescription 0 0 1
        delegationFee ctx w >>= flip verify
            [ expectResponseCode HTTP.status403
            , expectErrorMessage $ errMsg403DelegationFee fee
            ]

    it "STAKE_POOLS_ESTIMATE_FEE_03 - can't use byron wallets" $ \ctx -> do
        w <- fixtureRandomWallet ctx
        let ep = Link.getDelegationFee w
        r <- request @(ApiTransaction 'Mainnet) ctx ep Default Empty
        verify r
            [ expectResponseCode HTTP.status404 -- should fail
            , expectErrorMessage $ errMsg404NoWallet (w ^. walletId)
            ]

    describe "STAKE_POOLS_ESTIMATE_FEE_04 - false wallet ids" $ do
        forM_ falseWalletIds $ \(wDesc, walId) -> do
            let path = "wallets/" <> walId
            it ("wallet:" ++ wDesc) $ \ctx -> do
                let endpoint = "v2/" <> T.pack path <> "/delegations/fees"
                rg <- request @ApiFee ctx ("GET", endpoint) Default Empty
                expectResponseCode @IO HTTP.status404 rg
                if wDesc == "40 chars hex"
                then expectErrorMessage (errMsg404NoWallet $ T.pack walId) rg
                else expectErrorMessage errMsg404NoEndpoint rg

    describe "STAKE_POOLS_ESTIMATE_FEE_05 -\
        \ v2/wallets/{wid}/delegations/fees - Methods Not Allowed" $ do
        let methods = ["POST", "PUT", "DELETE", "CONNECT", "TRACE", "OPTIONS"]
        forM_ methods $ \method -> it (show method) $ \ctx -> do
            w <- emptyWallet ctx
            let ep = "v2/wallets/"<> w ^. walletId <> "/delegations/fees"
            r <- request @ApiFee ctx (method, ep) Default Empty
            expectResponseCode @IO HTTP.status405 r
            expectErrorMessage errMsg405 r

    describe "STAKE_POOLS_ESTIMATE_FEE_06 - HTTP headers" $ do
        forM_ (getHeaderCases HTTP.status200)
            $ \(title, headers, exps) -> it title $ \ctx -> do
                w <- fixtureWallet ctx
                let ep = Link.getDelegationFee w
                r <- request @ApiFee ctx ep headers Empty
                verify r exps

    describe "STAKE_POOLS_JOIN/QUIT_05 - Bad request" $ do
        let verifyIt ctx sPoolEndp = do
                w <- emptyWallet ctx
                let payload = NonJson  "{ passphrase: Secure Passphrase }"
                r <- request @(ApiTransaction n) ctx
                    (sPoolEndp (Identity arbitraryPoolId) w) Default payload
                expectResponseCode HTTP.status400 r

        it "Join" $ \ctx -> do
            verifyIt ctx Link.joinStakePool
        it "Quit" $ \ctx -> do
            verifyIt ctx Link.quitStakePool

    describe "STAKE_POOLS_JOIN/QUIT_05 -  Methods Not Allowed" $ do
        let methods = ["POST", "CONNECT", "TRACE", "OPTIONS"]
        forM_ methods $ \method -> it ("Join: " ++ show method) $ \ctx -> do
            w <- emptyWallet ctx
            let payload = Json [json| {
                    "passphrase": "Secure Passphrase"
                    } |]
            let link = withMethod method $
                    Link.joinStakePool (Identity arbitraryPoolId) w
            r <- request @(ApiTransaction n) ctx link Default payload
            expectResponseCode HTTP.status405 r
            expectErrorMessage errMsg405 r

    describe "STAKE_POOLS_JOIN/QUIT_05 - HTTP headers" $ do
        let verifyIt ctx sPoolEndp headers expec = do
                w <- emptyWallet ctx
                let payload = Json [json| {
                        "passphrase": "Secure Passphrase"
                        } |]
                r <- request @(ApiTransaction n) ctx
                        (sPoolEndp (Identity arbitraryPoolId) w) headers payload
                verify r expec

        let payloadHeaderCases =
                    [ ( "No HTTP headers -> 415", None
                       , [ expectResponseCode @IO HTTP.status415
                         , expectErrorMessage errMsg415 ]
                       )
                     , ( "Accept: text/plain -> 406"
                       , Headers
                             [ ("Content-Type", "application/json")
                             , ("Accept", "text/plain") ]
                       , [ expectResponseCode @IO HTTP.status406
                         , expectErrorMessage errMsg406 ]
                       )
                     , ( "No Content-Type -> 415"
                       , Headers [ ("Accept", "application/json") ]
                       , [ expectResponseCode @IO HTTP.status415
                         , expectErrorMessage errMsg415 ]
                       )
                     , ( "Content-Type: text/plain -> 415"
                       , Headers [ ("Content-Type", "text/plain") ]
                       , [ expectResponseCode @IO HTTP.status415
                         , expectErrorMessage errMsg415 ]
                       )
                     ]
        forM_ payloadHeaderCases $ \(title, headers, expectations) -> do
            it ("Join: " ++ title) $ \ctx ->
                verifyIt ctx Link.joinStakePool headers expectations
        forM_ payloadHeaderCases $ \(title, headers, expectations) -> do
            it ("Quit: " ++ title) $ \ctx ->
                verifyIt ctx Link.quitStakePool headers expectations

    it "STAKE_POOLS_QUIT_01 - Can quit stake pool" $ \ctx -> do
        (w, p) <- joinStakePoolWithFixtureWallet ctx

        r <- quitStakePool ctx (p ^. #id) (w, fixturePassphrase)
        expectResponseCode HTTP.status202 r

        -- Wait for the certificate to be inserted
        eventually $ do
            let ep = Link.listTransactions @'Shelley w
            request @[ApiTransaction n] ctx ep Default Empty >>= flip verify
                [ expectListItemFieldEqual 0 direction Outgoing
                , expectListItemFieldEqual 0 status InLedger
                , expectListItemFieldEqual 1 direction Outgoing
                , expectListItemFieldEqual 1 status InLedger
                ]

        request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
            [ expectFieldEqual delegation NotDelegating
            ]

    it "STAKE_POOLS_QUIT_01 - Quiting before even joining" $ \ctx -> do
        (_, p:_) <- eventually $
            unsafeRequest @[ApiStakePool] ctx Link.listStakePools Empty
        w <- emptyWallet ctx

        r <- quitStakePool ctx (p ^. #id) (w, "Secure Passprase")
        expectResponseCode HTTP.status403 r
        expectErrorMessage (errMsg403WrongPool $ toText $ getApiT $ p ^. #id) r

    it "STAKE_POOLS_QUIT_02 - Passphrase must be correct to quit" $ \ctx -> do
        (w, p) <- joinStakePoolWithFixtureWallet ctx

        r <- quitStakePool ctx (p ^. #id) (w, "Incorrect Passphrase")
        expectResponseCode HTTP.status403 r
        expectErrorMessage errMsg403WrongPass r

    it "STAKE_POOLS_QUIT_02 - Cannot quit existant stake pool \
       \I have not joined" $ \ctx -> do
        (_, p1:p2:_) <- eventually $
            unsafeRequest @[ApiStakePool] ctx Link.listStakePools Empty
        w <- fixtureWallet ctx
        r <- joinStakePool ctx (p1 ^. #id) (w, fixturePassphrase)
        expectResponseCode HTTP.status202 r
        eventually $ do
            request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
                [ expectFieldEqual delegation (Delegating (p1 ^. #id))
                ]

        let pId = p2 ^. #id
        let wrongPoolId = toText $ getApiT pId
        rq <- quitStakePool ctx pId (w, fixturePassphrase)
        expectResponseCode HTTP.status403 rq
        expectErrorMessage (errMsg403WrongPool wrongPoolId) rq

-- | An arbitrary but valid pool id, to avoid having to list pools for testing
-- bad requests and headers.
arbitraryPoolId :: ApiT PoolId
arbitraryPoolId = either (error . show) ApiT $ fromText
    "a659052d84ddb6a04189bee523d59c0a3385c921f43db5dc5de17a4f3f11dc4c"

joinStakePoolWithWalletBalance
    :: (Context t)
    -> [Natural]
    -> IO (ApiWallet, ApiStakePool)
joinStakePoolWithWalletBalance ctx balance = do
    w <- fixtureWalletWith ctx balance
    (_, p:_) <- eventually $
        unsafeRequest @[ApiStakePool] ctx Link.listStakePools Empty
    r <- joinStakePool ctx (p ^. #id) (w, "Secure Passphrase")
    expectResponseCode HTTP.status202 r
    -- Verify the wallet is now delegating
    eventually $ do
        request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
            [ expectFieldEqual delegation (Delegating (p ^. #id))
            ]
    return (w, p)

joinStakePoolWithFixtureWallet
    :: (Context t)
    -> IO (ApiWallet, ApiStakePool)
joinStakePoolWithFixtureWallet ctx = do
    w <- fixtureWallet ctx
    (_, p:_) <- eventually $
        unsafeRequest @[ApiStakePool] ctx Link.listStakePools Empty
    r <- joinStakePool ctx (p ^. #id) (w, fixturePassphrase)
    expectResponseCode HTTP.status202 r
    -- Verify the wallet is now delegating
    eventually $ do
        request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
            [ expectFieldEqual delegation (Delegating (p ^. #id))
            ]
    return (w, p)

sortByPerformance
    :: Functor f
    => (a, f [ApiStakePool])
    -> (a, f [ApiStakePool])
sortByPerformance =
    second (fmap (sortOn (Down . view #apparentPerformance)))
