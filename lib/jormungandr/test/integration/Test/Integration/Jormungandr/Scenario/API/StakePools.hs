{-# LANGUAGE DataKinds #-}
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
    ( ApiStakePool, ApiT (..), ApiTransaction )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..), fromHex )
import Cardano.Wallet.Primitive.Types
    ( PoolId (..) )
import Control.Monad
    ( forM_ )
import Data.Quantity
    ( Quantity (..) )
import Test.Hspec
    ( SpecWith, describe, it )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , apparentPerformance
    , blocks
    , emptyByronWallet
    , emptyWallet
    , eventually
    , eventuallyUsingDelay_
    , eventually_
    , expectErrorMessage
    , expectListItemFieldEqual
    , expectListItemFieldSatisfy
    , expectListSizeEqual
    , expectResponseCode
    , joinStakePool
    , joinStakePoolEp
    , json
    , listStakePoolsEp
    , metrics
    , quitStakePool
    , quitStakePoolEp
    , request
    , stake
    , stakePoolEp
    , unsafeRequest
    , verify
    )
import Test.Integration.Framework.TestData
    ( errMsg404NoSuchPool
    , errMsg405
    , errMsg406
    , errMsg415
    , passphraseMaxLength
    , passphraseMinLength
    )

import qualified Cardano.Wallet.Api.Types as Types
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
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
        (_, p:_) <- eventually $
            unsafeRequest @[ApiStakePool] ctx listStakePoolsEp Empty
        w <- emptyWallet ctx
        r <- joinStakePool ctx p (w, "Secure Passphrase")
        expectResponseCode HTTP.status403 r

    it "STAKE_POOLS_JOIN_01 - I cannot join another until I quit\
        \ or maybe I will just re-join another?" $ \ctx -> do
        (_, p1:p2:_) <- eventually $
            unsafeRequest @[ApiStakePool] ctx listStakePoolsEp Empty
        w <- emptyWallet ctx
        r1 <- joinStakePool ctx p1 (w, "Secure Passprase")
        expectResponseCode HTTP.status403 r1

        r2 <- joinStakePool ctx p2 (w, "Secure Passprase")
        expectResponseCode HTTP.status403 r2

    it "STAKE_POOLS_JOIN_01 - \
        \I definitely can quit and join another" $ \ctx -> do
        (_, p1:p2:_) <- eventually $
            unsafeRequest @[ApiStakePool] ctx listStakePoolsEp Empty
        w <- emptyWallet ctx
        r1 <- joinStakePool ctx p1 (w, "Secure Passprase")
        expectResponseCode HTTP.status403 r1

        r1q <- quitStakePool ctx p1 (w, "Secure Passprase")
        expectResponseCode HTTP.status501 r1q

        r2 <- joinStakePool ctx p2 (w, "Secure Passprase")
        expectResponseCode HTTP.status403 r2

    it "STAKE_POOLS_JOIN_01 - Cannot join non-existant stakepool" $ \ctx -> do
        let pId =
                "0000000000000000000000000000000000000000000000000000000000000000"
        let (Right addr) = fromHex pId
        let poolIdAbsent = ApiT $ PoolId addr
        let poolMetrics = Types.ApiStakePoolMetrics (Quantity 0) (Quantity 0)
        let pool = Types.ApiStakePool poolIdAbsent poolMetrics 0
        w <- emptyWallet ctx
        r <- joinStakePool ctx pool (w, "Secure Passphrase")
        expectResponseCode HTTP.status404 r
        expectErrorMessage (errMsg404NoSuchPool (T.decodeUtf8 pId)) r

    it "STAKE_POOLS_JOIN_02 - Passphrase must be correct to join" $ \ctx -> do
        (_, p:_) <- eventually $
            unsafeRequest @[ApiStakePool] ctx listStakePoolsEp Empty
        w <- emptyWallet ctx
        r <- joinStakePool ctx p (w, "Incorrect Passphrase")
        expectResponseCode HTTP.status403 r

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
                r <- doStakePool ctx p (w, T.pack pass)
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
                r <- request @(ApiTransaction n) ctx (sPoolEndp p w)
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
        r <- joinStakePool ctx p (w, "Secure Passprase")
        expectResponseCode HTTP.status404 r

    describe "STAKE_POOLS_JOIN/QUIT_05 - Bad request" $ do
        let verifyIt ctx sPoolEndp = do
                (_, p:_) <- eventually $
                    unsafeRequest @[ApiStakePool] ctx listStakePoolsEp Empty
                w <- emptyWallet ctx
                let payload = NonJson  "{ passphrase: Secure Passphrase }"
                r <- request @(ApiTransaction n) ctx (sPoolEndp p w)
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
            r <- request @(ApiTransaction n) ctx (stakePoolEp method p w)
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
                r <- request @(ApiTransaction n) ctx (sPoolEndp p w)
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
                  , [ expectResponseCode @IO HTTP.status501 ]
                  )
                ]
        forM_ payloadHeaderCasesJoin $ \(title, headers, expectations) -> do
            it ("Join: " ++ title) $ \ctx ->
                verifyIt ctx joinStakePoolEp headers expectations
        forM_ payloadHeaderCasesQuit $ \(title, headers, expectations) -> do
            it ("Quit: " ++ title) $ \ctx ->
                verifyIt ctx quitStakePoolEp headers expectations

    it "STAKE_POOLS_QUIT_01 - Can quit stake pool" $ \ctx -> do
        (_, p:_) <- eventually $
            unsafeRequest @[ApiStakePool] ctx listStakePoolsEp Empty
        w <- emptyWallet ctx
        r <- joinStakePool ctx p (w, "Secure Passprase")
        expectResponseCode HTTP.status403 r

        rq <- quitStakePool ctx p (w, "Secure Passprase")
        expectResponseCode HTTP.status501 rq

    it "STAKE_POOLS_QUIT_01 - Quiting before even joining" $ \ctx -> do
        (_, p:_) <- eventually $
            unsafeRequest @[ApiStakePool] ctx listStakePoolsEp Empty
        w <- emptyWallet ctx

        rq <- quitStakePool ctx p (w, "Secure Passprase")
        expectResponseCode HTTP.status501 rq

    it "STAKE_POOLS_QUIT_02 - Passphrase must be correct to quit" $ \ctx -> do
        (_, p:_) <- eventually $
            unsafeRequest @[ApiStakePool] ctx listStakePoolsEp Empty
        w <- emptyWallet ctx
        joinStakePool ctx p (w, "Secure Passprase")
            >>= (expectResponseCode HTTP.status403)

        r <- quitStakePool ctx p (w, "Incorrect Passphrase")
        expectResponseCode HTTP.status501 r
