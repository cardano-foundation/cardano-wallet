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
    ( ApiNetworkInformation
    , ApiStakePool
    , ApiT (..)
    , ApiTransaction
    , ApiWallet
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.Types
    ( Direction (..), PoolId (..), TxStatus (..), WalletDelegation (..) )
import Control.Monad
    ( forM_, unless )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text.Class
    ( toText )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( SpecWith, describe, it )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , apparentPerformance
    , balanceAvailable
    , balanceReward
    , balanceTotal
    , blocks
    , delegation
    , direction
    , emptyByronWallet
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
    , fixturePassphrase
    , fixtureWallet
    , fixtureWalletWith
    , getFromResponse
    , getWalletEp
    , joinStakePool
    , joinStakePoolEp
    , json
    , listStakePoolsEp
    , listTxEp
    , metrics
    , networkInfoEp
    , quitStakePool
    , quitStakePoolEp
    , request
    , stake
    , stakePoolEp
    , status
    , unsafeRequest
    , verify
    )
import Test.Integration.Framework.TestData
    ( errMsg403DelegationFee
    , errMsg403PoolAlreadyJoined
    , errMsg403WrongPass
    , errMsg403WrongPool
    , errMsg404NoSuchPool
    , errMsg405
    , errMsg406
    , errMsg415
    , passphraseMaxLength
    , passphraseMinLength
    , stakeDelegationFee
    )

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Network.HTTP.Types.Status as HTTP

spec :: forall t n. (n ~ 'Testnet) => SpecWith (Context t)
spec = do
    it "STAKE_POOLS_LIST_01 - List stake pools" $ \ctx -> do
        eventually_ $ do
            r <- request @[ApiStakePool] ctx listStakePoolsEp Default Empty
            expectResponseCode HTTP.status200 r
            -- With the current genesis.yaml we have 3 pools with 1 lovelace,
            -- and an epoch length of 3.
            --
            -- For some reason, the first pool (the node we run), produces
            -- blocks in 100% of the /slots/. This means it will have produced
            -- either 1 or 2 blocks in the current epoch.
            verify r
                [ expectListSizeEqual 3

                , expectListItemFieldEqual 0
                    (metrics . stake) 1
                , expectListItemFieldSatisfy 0
                    (metrics . blocks) (> 1)
                , expectListItemFieldSatisfy 0
                    apparentPerformance (> 0)

                , expectListItemFieldEqual 1
                    (metrics . stake) 1
                , expectListItemFieldEqual 1
                    (metrics . blocks) 0
                , expectListItemFieldEqual 2
                    apparentPerformance 0

                , expectListItemFieldEqual 2
                    (metrics . stake) 1
                , expectListItemFieldEqual 2
                    (metrics . blocks) 0
                , expectListItemFieldEqual 2
                    apparentPerformance 0
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
            r <- request @[ApiStakePool] ctx listStakePoolsEp Default Empty
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

    it "STAKE_POOLS_JOIN_01 - Can join a stakepool" $ \ctx -> do
        w <- fixtureWallet ctx
        (_, p:_) <- eventually $
            unsafeRequest @[ApiStakePool] ctx listStakePoolsEp Empty

        -- Join a pool
        joinStakePool ctx (p ^. #id) (w, fixturePassphrase) >>= flip verify
            [ expectResponseCode HTTP.status200
            , expectFieldEqual status Pending
            , expectFieldEqual direction Outgoing
            ]

        -- Wait for the certificate to be inserted
        eventually $ do
            let ep = listTxEp w mempty
            request @[ApiTransaction n] ctx ep Default Empty >>= flip verify
                [ expectListItemFieldEqual 0 direction Outgoing
                , expectListItemFieldEqual 0 status InLedger
                ]

    it "STAKE_POOLS_JOIN_01 - Controlled stake increases when joining" $ \ctx -> do
        w <- fixtureWallet ctx
        (_, p:_) <- eventually $
            unsafeRequest @[ApiStakePool] ctx listStakePoolsEp Empty

        -- Join a pool
        joinStakePool ctx (p ^. #id) (w, fixturePassphrase) >>= flip verify
            [ expectResponseCode HTTP.status200
            , expectFieldEqual status Pending
            , expectFieldEqual direction Outgoing
            ]

        -- Wait for the certificate to be inserted
        eventually $ do
            let ep = listTxEp w mempty
            request @[ApiTransaction n] ctx ep Default Empty >>= flip verify
                [ expectListItemFieldEqual 0 direction Outgoing
                , expectListItemFieldEqual 0 status InLedger
                ]

        let existingPoolStake = getQuantity $ p ^. #metrics . #controlledStake
        let contributedStake = faucetUtxoAmt - stakeDelegationFee
        eventually $ do
            print contributedStake
            print existingPoolStake
            request @[ApiStakePool] ctx listStakePoolsEp Default Empty >>= flip verify
                [ expectListItemFieldSatisfy 0 (metrics . stake)
                    (> (existingPoolStake + contributedStake))
                    -- No exact equality since the delegation from previous
                    -- tests may take effect.
                ]

    it "STAKE_POOLS_JOIN_04 - Rewards accumulate and stop" $ \ctx -> do
        w <- fixtureWallet ctx
        (_, p:_) <- eventually $
            unsafeRequest @[ApiStakePool] ctx listStakePoolsEp Empty

        -- Join a pool
        joinStakePool ctx (p ^. #id) (w, fixturePassphrase) >>= flip verify
            [ expectResponseCode HTTP.status200
            , expectFieldEqual status Pending
            , expectFieldEqual direction Outgoing
            ]
        eventually $ do
            let ep = listTxEp w mempty
            request @[ApiTransaction n] ctx ep Default Empty >>= flip verify
                [ expectListItemFieldEqual 0 direction Outgoing
                , expectListItemFieldEqual 0 status InLedger
                ]

        -- Wait for money to flow
        eventually $ do
            request @ApiWallet ctx (getWalletEp w) Default Empty >>= flip verify
                [ expectFieldSatisfy balanceReward (> 0)
                ]

        -- Quit a pool
        quitStakePool ctx (p ^. #id) (w, fixturePassphrase) >>= flip verify
            [ expectResponseCode HTTP.status202
            , expectFieldEqual status Pending
            , expectFieldEqual direction Outgoing
            ]
        eventually $ do
            let ep = listTxEp w mempty
            request @[ApiTransaction n] ctx ep Default Empty >>= flip verify
                [ expectListItemFieldEqual 0 direction Outgoing
                , expectListItemFieldEqual 0 status InLedger
                , expectListItemFieldEqual 1 direction Outgoing
                , expectListItemFieldEqual 1 status InLedger
                ]

        reward <- getFromResponse balanceReward <$>
            request @ApiWallet ctx (getWalletEp w) Default Empty

        -- Wait an epoch and make sure the reward doesn't increase
        epoch <- getFromResponse (#networkTip . #epochNumber) <$>
            request @ApiNetworkInformation ctx networkInfoEp Default Empty
        eventually $ do
            epoch' <- getFromResponse (#networkTip . #epochNumber) <$>
                request @ApiNetworkInformation ctx networkInfoEp Default Empty
            unless (getApiT epoch' > getApiT epoch) $ fail "not yet"
        request @ApiWallet ctx (getWalletEp w) Default Empty >>= flip verify
            [ expectFieldSatisfy balanceReward (== reward)
            ]

    it "STAKE_POOLS_JOIN_01 - I can join another stake-pool after previously \
        \ joining another one" $ \ctx -> do
        w <- fixtureWallet ctx
        (_, p1:p2:_) <- eventually $
            unsafeRequest @[ApiStakePool] ctx listStakePoolsEp Empty

        -- Join pool p1
        joinStakePool ctx (p1 ^. #id) (w, fixturePassphrase) >>= flip verify
            [ expectResponseCode HTTP.status200
            , expectFieldEqual status Pending
            , expectFieldEqual direction Outgoing
            ]
        eventually $ do
            let ep = listTxEp w mempty
            request @[ApiTransaction n] ctx ep Default Empty >>= flip verify
                [ expectListItemFieldEqual 0 direction Outgoing
                , expectListItemFieldEqual 0 status InLedger
                ]
        request @ApiWallet ctx (getWalletEp w) Default Empty >>= flip verify
            [ expectFieldEqual delegation (Delegating (p1 ^. #id))
            ]

        -- Join pool p2
        joinStakePool ctx (p2 ^. #id) (w, fixturePassphrase) >>= flip verify
            [ expectResponseCode HTTP.status200
            , expectFieldEqual status Pending
            , expectFieldEqual direction Outgoing
            ]
        eventually $ do
            let ep = listTxEp w mempty
            (code, txs) <- request @[ApiTransaction n] ctx ep Default Empty
            let onlyPending = \case
                    e@Left{} -> e
                    Right xs -> Right $
                        filter (\t -> t ^. #status == ApiT Pending) xs
            verify (code, (onlyPending txs))
                [ expectListSizeEqual 0
                ]
        request @ApiWallet ctx (getWalletEp w) Default Empty >>= flip verify
            [ expectFieldEqual delegation (Delegating (p2 ^. #id))
            ]

    describe "STAKE_POOLS_JOIN_01x - Fee boundary values" $ do
        it "STAKE_POOLS_JOIN_01x - \
            \I can join if I have just the right amount" $ \(ctx) -> do
            (_, p:_) <- eventually $
                unsafeRequest @[ApiStakePool] ctx listStakePoolsEp Empty
            w <- fixtureWalletWith ctx [stakeDelegationFee]
            joinStakePool ctx (p ^. #id) (w, "Secure Passphrase")>>= flip verify
                [ expectResponseCode HTTP.status200
                , expectFieldEqual status Pending
                , expectFieldEqual direction Outgoing
                ]

        it "STAKE_POOLS_JOIN_01x - \
            \I cannot join if I have not enough fee to cover" $ \ctx -> do
            (_, p:_) <- eventually $
                unsafeRequest @[ApiStakePool] ctx listStakePoolsEp Empty
            w <- fixtureWalletWith ctx [stakeDelegationFee - 1]
            r <- joinStakePool ctx (p ^. #id) (w, "Secure Passphrase")
            print r
            expectResponseCode HTTP.status403 r
            expectErrorMessage (errMsg403DelegationFee 1) r

        it "STAKE_POOLS_JOIN_01x - I cannot join stake-pool with 0 balance" $ \ctx -> do
            (_, p:_) <- eventually $
                unsafeRequest @[ApiStakePool] ctx listStakePoolsEp Empty
            w <- emptyWallet ctx
            r <- joinStakePool ctx (p ^. #id) (w, "Secure Passphrase")
            expectResponseCode HTTP.status403 r
            expectErrorMessage (errMsg403DelegationFee stakeDelegationFee) r

    describe "STAKE_POOLS_QUIT_01x - Fee boundary values" $ do
        it "STAKE_POOLS_QUIT_01x - \
            \I can quit if I have enough to cover fee" $ \(ctx) -> do
            let initBalance = [2*stakeDelegationFee + 1]
            (w, p) <- joinStakePoolWithWalletBalance ctx initBalance
            rq <- quitStakePool ctx (p ^. #id) (w, "Secure Passphrase")
            expectResponseCode HTTP.status202 rq
            eventually $ do
                request @ApiWallet ctx (getWalletEp w) Default Empty >>= flip verify
                    [ expectFieldEqual delegation (NotDelegating)
                    -- balance is 1 because the rest was used for fees
                    , expectFieldEqual balanceTotal 1
                    , expectFieldEqual balanceAvailable 1
                    ]

        it "STAKE_POOLS_QUIT_01x - \
            \I cannot quit if I have not enough fee to cover" $ \ctx -> do
            let initBalance = [stakeDelegationFee]
            (w, p) <- joinStakePoolWithWalletBalance ctx initBalance
            rq <- quitStakePool ctx (p ^. #id) (w, "Secure Passphrase")
            verify rq
                [ expectResponseCode HTTP.status403
                , expectErrorMessage (errMsg403DelegationFee stakeDelegationFee)
                ]

    it "STAKE_POOLS_JOIN_01 - I cannot rejoin the same stake-pool" $ \ctx -> do
        (w, p) <- joinStakePoolWithWalletBalance ctx [1000]

        -- Join again
        r <- joinStakePool ctx (p ^. #id) (w, fixturePassphrase)
        expectResponseCode HTTP.status403 r
        let poolId = toText $ getApiT $ p ^. #id
        expectErrorMessage (errMsg403PoolAlreadyJoined poolId) r

    it "STAKE_POOLS_JOIN_01 - \
        \I definitely can quit and join another" $ \ctx -> do
        (_, p1:p2:_) <- eventually $
            unsafeRequest @[ApiStakePool] ctx listStakePoolsEp Empty
        w <- fixtureWallet ctx

        r <- joinStakePool ctx (p1 ^. #id) (w, fixturePassphrase)
        expectResponseCode HTTP.status200 r
        eventually $ do
            request @ApiWallet ctx (getWalletEp w) Default Empty >>= flip verify
                [ expectFieldEqual delegation (Delegating (p1 ^. #id))
                ]

        r1q <- quitStakePool ctx (p1 ^. #id) (w, fixturePassphrase)
        expectResponseCode HTTP.status202 r1q
        eventually $ do
            request @ApiWallet ctx (getWalletEp w) Default Empty >>= flip verify
                [ expectFieldEqual delegation (NotDelegating)
                ]

        r2 <- joinStakePool ctx (p2 ^. #id) (w, fixturePassphrase)
        expectResponseCode HTTP.status200 r2
        eventually $ do
            request @ApiWallet ctx (getWalletEp w) Default Empty >>= flip verify
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
            unsafeRequest @[ApiStakePool] ctx listStakePoolsEp Empty

        -- Join a pool
        joinStakePool ctx (p ^. #id) (wA, fixturePassphrase) >>= flip verify
            [ expectResponseCode HTTP.status200
            , expectFieldEqual status Pending
            , expectFieldEqual direction Outgoing
            ]

        -- Wait for the certificate to be inserted
        eventually $ do
            let ep = listTxEp wA mempty
            request @[ApiTransaction n] ctx ep Default Empty >>= flip verify
                [ expectListItemFieldEqual 0 direction Outgoing
                , expectListItemFieldEqual 0 status InLedger
                ]

        -- Verify the wallet is now delegating
        request @ApiWallet ctx (getWalletEp wA) Default Empty >>= flip verify
            [ expectFieldEqual delegation (Delegating (p ^. #id))
            ]

        -- Verify the other is NOT delegating
        request @ApiWallet ctx (getWalletEp wB) Default Empty >>= flip verify
            [ expectFieldEqual delegation NotDelegating
            ]

    it "STAKE_POOLS_JOIN_02 - Passphrase must be correct to join" $ \ctx -> do
        (_, p:_) <- eventually $
            unsafeRequest @[ApiStakePool] ctx listStakePoolsEp Empty
        w <- fixtureWallet ctx
        r <- joinStakePool ctx (p ^. #id) (w, "Incorrect Passphrase")
        expectResponseCode HTTP.status403 r
        expectErrorMessage errMsg403WrongPass r

    describe "STAKE_POOLS_JOIN/QUIT_02 -\
        \ Passphrase must have appropriate length" $ do

        let pMax = passphraseMaxLength
        let pMin = passphraseMinLength
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
                    unsafeRequest @[ApiStakePool] ctx listStakePoolsEp Empty
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
                    unsafeRequest @[ApiStakePool] ctx listStakePoolsEp Empty
                w <- emptyWallet ctx
                let payload = Json [json| {
                        "passphrase": 123
                        } |]
                r <- request @(ApiTransaction n) ctx (sPoolEndp (p ^. #id) w)
                        Default payload
                expectResponseCode HTTP.status400 r
                expectErrorMessage "expected Text, encountered Number" r
        it "Join" $ \ctx -> do
            verifyIt ctx joinStakePoolEp
        it "Quit" $ \ctx -> do
            verifyIt ctx quitStakePoolEp

    it "STAKE_POOLS_JOIN_03 - Byron wallet cannot join stake pool" $ \ctx -> do
        (_, p:_) <- eventually $
            unsafeRequest @[ApiStakePool] ctx listStakePoolsEp Empty
        w <- emptyByronWallet ctx
        r <- joinStakePool ctx (p ^. #id) (w, "Secure Passprase")
        expectResponseCode HTTP.status404 r

    describe "STAKE_POOLS_JOIN/QUIT_05 - Bad request" $ do
        let verifyIt ctx sPoolEndp = do
                (_, p:_) <- eventually $
                    unsafeRequest @[ApiStakePool] ctx listStakePoolsEp Empty
                w <- emptyWallet ctx
                let payload = NonJson  "{ passphrase: Secure Passphrase }"
                r <- request @(ApiTransaction n) ctx (sPoolEndp (p ^. #id) w)
                        Default payload
                expectResponseCode HTTP.status400 r

        it "Join" $ \ctx -> do
            verifyIt ctx joinStakePoolEp
        it "Quit" $ \ctx -> do
            verifyIt ctx quitStakePoolEp

    describe "STAKE_POOLS_JOIN/QUIT_05 -  Methods Not Allowed" $ do
        let methods = ["POST", "CONNECT", "TRACE", "OPTIONS"]
        forM_ methods $ \method -> it ("Join: " ++ show method) $ \ctx -> do
            (_, p:_) <- eventually $
                unsafeRequest @[ApiStakePool] ctx listStakePoolsEp Empty
            w <- emptyWallet ctx
            let payload = Json [json| {
                    "passphrase": "Secure Passphrase"
                    } |]
            r <- request @(ApiTransaction n) ctx (stakePoolEp method (p ^. #id) w)
                    Default payload
            expectResponseCode HTTP.status405 r
            expectErrorMessage errMsg405 r

    describe "STAKE_POOLS_JOIN/QUIT_05 - HTTP headers" $ do
        let verifyIt ctx sPoolEndp headers expec = do
                (_, p:_) <- eventually $ do
                    unsafeRequest @[ApiStakePool] ctx listStakePoolsEp Empty
                w <- emptyWallet ctx
                let payload = Json [json| {
                        "passphrase": "Secure Passphrase"
                        } |]
                r <- request @(ApiTransaction n) ctx (sPoolEndp (p ^. #id) w)
                        headers payload
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
        let payloadHeaderCasesJoin = payloadHeaderCases ++
                [ ( "No Accept -> 202"
                  , Headers [ ("Content-Type", "application/json") ]
                  , [ expectResponseCode @IO HTTP.status403 ]
                  )
                ]
        let payloadHeaderCasesQuit = payloadHeaderCases ++
                [ ( "No Accept -> 202"
                  , Headers [ ("Content-Type", "application/json") ]
                  , [ expectResponseCode @IO HTTP.status403 ]
                  )
                ]
        forM_ payloadHeaderCasesJoin $ \(title, headers, expectations) -> do
            it ("Join: " ++ title) $ \ctx ->
                verifyIt ctx joinStakePoolEp headers expectations
        forM_ payloadHeaderCasesQuit $ \(title, headers, expectations) -> do
            it ("Quit: " ++ title) $ \ctx ->
                verifyIt ctx quitStakePoolEp headers expectations

    it "STAKE_POOLS_QUIT_01 - Can quit stake pool" $ \ctx -> do
        (w, p) <- joinStakePoolWithFixtureWallet ctx

        r <- quitStakePool ctx (p ^. #id) (w, fixturePassphrase)
        expectResponseCode HTTP.status202 r

        -- Wait for the certificate to be inserted
        eventually $ do
            let ep = listTxEp w mempty
            request @[ApiTransaction n] ctx ep Default Empty >>= flip verify
                [ expectListItemFieldEqual 0 direction Outgoing
                , expectListItemFieldEqual 0 status InLedger
                , expectListItemFieldEqual 1 direction Outgoing
                , expectListItemFieldEqual 1 status InLedger
                ]

        request @ApiWallet ctx (getWalletEp w) Default Empty >>= flip verify
            [ expectFieldEqual delegation NotDelegating
            ]

    it "STAKE_POOLS_QUIT_01 - Quiting before even joining" $ \ctx -> do
        (_, p:_) <- eventually $
            unsafeRequest @[ApiStakePool] ctx listStakePoolsEp Empty
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
            unsafeRequest @[ApiStakePool] ctx listStakePoolsEp Empty
        w <- fixtureWallet ctx
        r <- joinStakePool ctx (p1 ^. #id) (w, fixturePassphrase)
        expectResponseCode HTTP.status200 r
        eventually $ do
            request @ApiWallet ctx (getWalletEp w) Default Empty >>= flip verify
                [ expectFieldEqual delegation (Delegating (p1 ^. #id))
                ]

        let pId = p2 ^. #id
        let wrongPoolId = toText $ getApiT pId
        rq <- quitStakePool ctx pId (w, fixturePassphrase)
        expectResponseCode HTTP.status403 rq
        expectErrorMessage (errMsg403WrongPool wrongPoolId) rq

joinStakePoolWithWalletBalance
    :: (Context t)
    -> [Natural]
    -> IO (ApiWallet, ApiStakePool)
joinStakePoolWithWalletBalance ctx balance = do
    w <- fixtureWalletWith ctx balance
    (_, p:_) <- eventually $
        unsafeRequest @[ApiStakePool] ctx listStakePoolsEp Empty
    r <- joinStakePool ctx (p ^. #id) (w, "Secure Passphrase")
    expectResponseCode HTTP.status200 r
    -- Verify the wallet is now delegating
    eventually $ do
        request @ApiWallet ctx (getWalletEp w) Default Empty >>= flip verify
            [ expectFieldEqual delegation (Delegating (p ^. #id))
            ]
    return (w, p)

joinStakePoolWithFixtureWallet
    :: (Context t)
    -> IO (ApiWallet, ApiStakePool)
joinStakePoolWithFixtureWallet ctx = do
    w <- fixtureWallet ctx
    (_, p:_) <- eventually $
        unsafeRequest @[ApiStakePool] ctx listStakePoolsEp Empty
    r <- joinStakePool ctx (p ^. #id) (w, fixturePassphrase)
    expectResponseCode HTTP.status200 r
    -- Verify the wallet is now delegating
    eventually $ do
        request @ApiWallet ctx (getWalletEp w) Default Empty >>= flip verify
            [ expectFieldEqual delegation (Delegating (p ^. #id))
            ]
    return (w, p)
