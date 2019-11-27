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
    ( Direction (..), PoolId (..), TxStatus (..) )
import Control.Monad
    ( forM_ )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Test.Hspec
    ( SpecWith, describe, it, xit )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , apparentPerformance
    , blocks
    , direction
    , emptyByronWallet
    , emptyWallet
    , eventually
    , eventuallyUsingDelay_
    , eventually_
    , expectErrorMessage
    , expectFieldEqual
    , expectListItemFieldEqual
    , expectListItemFieldSatisfy
    , expectListSizeEqual
    , expectResponseCode
    , fixturePassphrase
    , fixtureWallet
    , joinStakePool
    , joinStakePoolEp
    , json
    , listStakePoolsEp
    , listTxEp
    , metrics
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
    ( errMsg403PoolAlreadyJoined
    , errMsg404NoSuchPool
    , errMsg405
    , errMsg406
    , errMsg415
    , passphraseMaxLength
    , passphraseMinLength
    )

import qualified Data.ByteString.Char8 as B8
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
        w <- fixtureWallet ctx

        -- Join a pool
        (_, p:_) <- eventually $
            unsafeRequest @[ApiStakePool] ctx listStakePoolsEp Empty
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
            -- TODO: verify the wallet delegation status after #899

    it "STAKE_POOLS_JOIN_01 - I can join another stake-pool after previously \
        \ joining another one" $ \ctx -> do
        (_, p1:p2:_) <- eventually $
            unsafeRequest @[ApiStakePool] ctx listStakePoolsEp Empty
        w <- fixtureWallet ctx

        joinStakePool ctx (p1 ^. #id) (w, fixturePassphrase) >>= flip verify
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

        joinStakePool ctx (p2 ^. #id) (w, fixturePassphrase) >>= flip verify
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

    xit "STAKE_POOLS_JOIN_100 - I cannot rejoin the same stake-pool" $ \ctx -> do
        -- TODO: enable after #899
        (_, p:_) <- eventually $
            unsafeRequest @[ApiStakePool] ctx listStakePoolsEp Empty
        let pId = p ^. #id
        w <- fixtureWallet ctx

        joinStakePool ctx pId (w, fixturePassphrase) >>= flip verify
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

        r <- joinStakePool ctx pId (w, fixturePassphrase)
        expectResponseCode HTTP.status403 r
        let (ApiT (PoolId bs)) = pId
        expectErrorMessage (errMsg403PoolAlreadyJoined (T.decodeUtf8 bs)) r

    it "STAKE_POOLS_JOIN_01 - \
        \I definitely can quit and join another" $ \ctx -> do
        (_, p1:p2:_) <- eventually $
            unsafeRequest @[ApiStakePool] ctx listStakePoolsEp Empty

        w <- fixtureWallet ctx

        joinStakePool ctx (p1 ^. #id) (w, fixturePassphrase) >>= flip verify
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

        r1q <- quitStakePool ctx (p1 ^. #id) (w, "Secure Passprase")
        expectResponseCode HTTP.status501 r1q

        joinStakePool ctx (p2 ^. #id) (w, fixturePassphrase) >>= flip verify
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

    it "STAKE_POOLS_JOIN_01 - Cannot join non-existant stakepool" $ \ctx -> do
        let pId = B8.replicate 64 '0'
        let (Right addr) = fromHex pId
        let poolIdAbsent = ApiT $ PoolId addr
        w <- emptyWallet ctx
        r <- joinStakePool ctx poolIdAbsent (w, "Secure Passphrase")
        expectResponseCode HTTP.status404 r
        expectErrorMessage (errMsg404NoSuchPool (T.decodeUtf8 pId)) r

    it "STAKE_POOLS_JOIN_02 - Passphrase must be correct to join" $ \ctx -> do
        (_, p:_) <- eventually $
            unsafeRequest @[ApiStakePool] ctx listStakePoolsEp Empty
        w <- emptyWallet ctx
        r <- joinStakePool ctx (p ^. #id) (w, "Incorrect Passphrase")
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
        r <- joinStakePool ctx (p ^. #id) (w, "Secure Passprase")
        expectResponseCode HTTP.status403 r

        rq <- quitStakePool ctx (p ^. #id) (w, "Secure Passprase")
        expectResponseCode HTTP.status501 rq

    it "STAKE_POOLS_QUIT_01 - Quiting before even joining" $ \ctx -> do
        (_, p:_) <- eventually $
            unsafeRequest @[ApiStakePool] ctx listStakePoolsEp Empty
        w <- emptyWallet ctx

        rq <- quitStakePool ctx (p ^. #id) (w, "Secure Passprase")
        expectResponseCode HTTP.status501 rq

    it "STAKE_POOLS_QUIT_02 - Passphrase must be correct to quit" $ \ctx -> do
        (_, p:_) <- eventually $
            unsafeRequest @[ApiStakePool] ctx listStakePoolsEp Empty
        w <- emptyWallet ctx
        joinStakePool ctx (p ^. #id) (w, "Secure Passprase")
            >>= (expectResponseCode HTTP.status403)

        r <- quitStakePool ctx (p ^. #id) (w, "Incorrect Passphrase")
        expectResponseCode HTTP.status501 r
