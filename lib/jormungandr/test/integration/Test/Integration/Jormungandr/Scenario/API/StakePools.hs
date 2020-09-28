{-# LANGUAGE AllowAmbiguousTypes #-}
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

import Cardano.CLI
    ( Port (..) )
import Cardano.Pool.Jormungandr.Metadata
    ( ApiStakePool )
import Cardano.Wallet.Api.Types
    ( ApiT (..)
    , ApiTransaction
    , ApiWallet
    , DecodeAddress
    , DecodeStakeAddress
    , EncodeAddress
    , WalletStyle (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( PassphraseMaxLength (..) )
import Cardano.Wallet.Primitive.Types
    ( Direction (..), FeePolicy (..), PoolId (..), TxStatus (..) )
import Cardano.Wallet.Transaction
    ( DelegationAction (..) )
import Control.Monad.IO.Class
    ( liftIO )
import Data.Functor.Identity
    ( Identity (..) )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.List
    ( find )
import Data.Maybe
    ( isJust, isNothing )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text.Class
    ( fromText, toText )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( SpecWith, describe, it, pendingWith, shouldBe )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , ResourceT
    , TxDescription (..)
    , delegating
    , delegationFee
    , emptyRandomWallet
    , emptyWallet
    , eventually
    , eventuallyUsingDelay
    , expectErrorMessage
    , expectField
    , expectListField
    , expectListSize
    , expectResponseCode
    , faucetUtxoAmt
    , fixturePassphrase
    , fixtureRandomWallet
    , fixtureWallet
    , fixtureWalletWith
    , fixtureWalletWithMnemonics
    , getFromResponse
    , getSlotParams
    , joinStakePool
    , json
    , mkEpochInfo
    , notDelegating
    , pubKeyFromMnemonics
    , quitStakePool
    , request
    , restoreWalletFromPubKey
    , runResourceT
    , runResourceT
    , unsafeRequest
    , verify
    , waitAllTxsInLedger
    , waitForNextEpoch
    , walletId
    , (.>)
    , (.>=)
    )
import Test.Integration.Framework.TestData
    ( errMsg403DelegationFee
    , errMsg403NoRootKey
    , errMsg403NonNullReward
    , errMsg403NotDelegating
    , errMsg403PoolAlreadyJoined
    , errMsg403WrongPass
    , errMsg404NoSuchPool
    , errMsg404NoWallet
    )
import Test.Integration.Jormungandr.Fixture
    ( OwnerIdentity (..), registerStakePool )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Network.HTTP.Types.Status as HTTP

spec :: forall n t.
    ( DecodeAddress n
    , DecodeStakeAddress n
    , EncodeAddress n
    ) => SpecWith (Port "node", FeePolicy, Context t)
spec = do
    describe "HW_WALLETS_02,03 - Delegation with restored HW Wallets" $ do
        it "HW_WALLETS_03 - Cannot join SP" $ \(_,_,ctx) -> runResourceT @IO $ do
            (w, mnemonics) <- fixtureWalletWithMnemonics ctx
            let pubKey = pubKeyFromMnemonics mnemonics
            r <- request @ApiWallet ctx (Link.deleteWallet @'Shelley w) Default Empty
            expectResponseCode HTTP.status204 r

            wk <- restoreWalletFromPubKey @ApiWallet @'Shelley ctx pubKey "Wallet from pubkey"
            -- cannot join stake pool
            (_, p:_) <- liftIO $ eventually "Stake pools are listed" $
                unsafeRequest @[ApiStakePool] ctx Link.listJormungandrStakePools Empty
            rJoin <- joinStakePool @n ctx (p ^. #id) (wk, fixturePassphrase)
            expectResponseCode HTTP.status403 rJoin
            expectErrorMessage (errMsg403NoRootKey $ wk ^. walletId) rJoin

        it "HW_WALLETS_02 - Restoration from account public key preserves delegation\
            \ but I cannot quit" $ \(_,_,ctx) -> runResourceT @IO $ do
            -- create wallet and get acc pub key from mnemonics
            (w, mnemonics) <- fixtureWalletWithMnemonics ctx
            let accPub = pubKeyFromMnemonics mnemonics

            --make sure you are at the beginning of the epoch
            waitForNextEpoch ctx
            (currentEpoch, sp) <- getSlotParams ctx

            -- join stake pool
            (_, p:_) <- eventually "Stake pools are listed" $
                unsafeRequest @[ApiStakePool] ctx Link.listJormungandrStakePools Empty
            r <- joinStakePool @n ctx (p ^. #id) (w, fixturePassphrase)
            expectResponseCode HTTP.status202 r
            waitAllTxsInLedger @n ctx w
            let expectedDelegation =
                    [ expectField #delegation
                        (`shouldBe` notDelegating
                            [ (Just (p ^. #id), mkEpochInfo (currentEpoch + 2) sp)
                            ]
                        )
                    ]
            request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
                expectedDelegation

            -- delete wallet
            rDel <-
                request @ApiWallet ctx (Link.deleteWallet @'Shelley w) Default Empty
            expectResponseCode HTTP.status204 rDel

            -- restore from pub key and make sure delegation preserved
            wRestored <- restoreWalletFromPubKey @ApiWallet @'Shelley ctx accPub "Wallet from pubkey"
            request @ApiWallet ctx (Link.getWallet @'Shelley wRestored) Default Empty >>= flip verify
                expectedDelegation

            -- cannot quit stake pool
            rQuit <- quitStakePool @n ctx (wRestored, fixturePassphrase)
            expectResponseCode HTTP.status403 rQuit
            expectErrorMessage (errMsg403NoRootKey $ wRestored ^. walletId) rQuit

    it "STAKE_POOLS_LIST_01 - List stake pools" $ \(_,_,ctx) -> runResourceT @IO $ do
        liftIO $ eventually "Listing stake pools shows expected information" $ do
            r <- request @[ApiStakePool] ctx Link.listJormungandrStakePools Default Empty
            expectResponseCode HTTP.status200 r
            verify r
                [ expectListSize 3

                , expectListField 0
                    #metadata ((`shouldBe` Just "Genesis Pool") . fmap (view #name . getApiT))
                , expectListField 1
                    #metadata ((`shouldBe` Just "Genesis Pool") . fmap (view #name . getApiT))
                , expectListField 2
                    #metadata ((`shouldBe` Just "Genesis Pool") . fmap (view #name . getApiT))

                , expectListField 0
                    #cost (`shouldBe` (Quantity 0))
                , expectListField 1
                    #cost (`shouldBe` (Quantity 0))
                , expectListField 2
                    #cost (`shouldBe` (Quantity 0))

                , expectListField 0
                    #margin (`shouldBe` (Quantity minBound))
                , expectListField 1
                    #margin (`shouldBe` (Quantity minBound))
                , expectListField 2
                    #margin (`shouldBe` (Quantity minBound))

                , expectListField 0
                    (#metrics . #producedBlocks) (.>= Quantity 0)
                , expectListField 1
                    (#metrics . #producedBlocks) (.>= Quantity 0)
                , expectListField 2
                    (#metrics . #producedBlocks) (.>= Quantity 0)

                , expectListField 0
                    #apparentPerformance (.>= 0)
                , expectListField 1
                    #apparentPerformance (.>= 0)
                , expectListField 2
                    #apparentPerformance (.>= 0)

                , expectListField 0
                    #desirability (.>= 0)
                , expectListField 1
                    #desirability (.>= 0)
                , expectListField 2
                    #desirability (.>= 0)

                , expectListField 0
                    #saturation (.>= 0)
                , expectListField 1
                    #saturation (.>= 0)
                , expectListField 2
                    #saturation (.>= 0)
                ]

    it "STAKE_POOLS_LIST_02 - May fail on epoch boundaries" $ \(_,_,ctx) -> runResourceT @IO $ do
        -- We should be able to catch the stake-pool data in an un-synced state
        -- when we enter into a new epoch. The results should then be
        -- unavailible.
        --
        -- This might take a few tries (epoch changes), so it is only feasible
        -- to test with very short epochs.
        let ms = 1000
        liftIO $ eventuallyUsingDelay (50*ms) 100
            "Shows error when listing stake pools on epoch boundaries"
            $ do
            r <- request @[ApiStakePool] ctx Link.listJormungandrStakePools Default Empty
            verify r
                [ expectResponseCode HTTP.status503
                , expectErrorMessage
                    "I can't list stake pools yet because I need to scan the \
                    \blockchain for metrics first. I'm at"
                ]

    it "STAKE_POOLS_LIST_04 - Discovers new pools when they are registered"
      $ \(nPort,feePolicy,ctx) -> do
        let nWithMetadata = length . filter (isJust . view #metadata)
        let nWithoutMetadata = length . filter (isNothing . view #metadata)

        (_, pools) <- eventually "Stake pools are listed" $
            unsafeRequest @[ApiStakePool] ctx Link.listJormungandrStakePools Empty

        (poolIdA, poolAOwner)  <- registerStakePool nPort feePolicy WithMetadata
        (poolIdB, _poolBOwner) <- registerStakePool nPort feePolicy WithoutMetadata
        (poolIdC, poolCOwner)  <- registerStakePool nPort feePolicy WithMetadata

        waitForNextEpoch ctx
        (_, pools') <- eventually "Stake pools are listed again" $
            unsafeRequest @[ApiStakePool] ctx Link.listJormungandrStakePools Empty

        nWithoutMetadata pools' `shouldBe` nWithoutMetadata pools + 1
        nWithMetadata pools' `shouldBe` nWithMetadata pools + 2

        let (Just poolA) = find ((== ApiT poolIdA) . view #id) pools'
        fmap (view #owner . getApiT) (poolA ^. #metadata) `shouldBe` Just poolAOwner

        let (Just poolB) = find ((== ApiT poolIdB) . view #id) pools'
        (poolB ^. #metadata) `shouldBe` Nothing

        let (Just poolC) = find ((== ApiT poolIdC) . view #id) pools'
        fmap (view #owner . getApiT) (poolC ^. #metadata) `shouldBe` Just poolCOwner

    it "STAKE_POOLS_JOIN_01 - Can join a stakepool" $ \(_,_,ctx) -> runResourceT @IO $ do
        w <- fixtureWallet ctx
        (_, p:_) <- eventually "Stake pools are listed" $
            unsafeRequest @[ApiStakePool] ctx Link.listJormungandrStakePools Empty

        -- Join a pool
        joinStakePool @n ctx (p ^. #id) (w, fixturePassphrase) >>= flip verify
            [ expectResponseCode HTTP.status202
            , expectField (#status . #getApiT) (`shouldBe` Pending)
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            ]

        -- Wait for the certificate to be inserted
        eventually "Certificates are insterted" $ do
            let ep = Link.listTransactions @'Shelley w
            request @[ApiTransaction n] ctx ep Default Empty >>= flip verify
                [ expectListField 0
                    (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectListField 0
                    (#status . #getApiT) (`shouldBe` InLedger)
                ]

    it "STAKE_POOLS_JOIN_01 - Controlled stake increases when joining" $ \(_,_,ctx) -> runResourceT @IO $ do
        w <- fixtureWallet ctx
        (_, Right (p:_)) <- eventually "Stake pools are listed" $
            request @[ApiStakePool] ctx Link.listJormungandrStakePools Default Empty

        -- Join a pool
        joinStakePool @n ctx (p ^. #id) (w, fixturePassphrase) >>= flip verify
            [ expectResponseCode HTTP.status202
            , expectField (#status . #getApiT) (`shouldBe` Pending)
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            ]

        -- Wait for the certificate to be inserted
        eventually "Certificates are insterted" $ do
            let ep = Link.listTransactions @'Shelley w
            request @[ApiTransaction n] ctx ep Default Empty >>= flip verify
                [ expectListField 0
                    (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectListField 0
                    (#status . #getApiT) (`shouldBe` InLedger)
                ]

        let (fee, _) = ctx ^. #_feeEstimator $ DelegDescription (Join dummyPool)
        let existingPoolStake = getQuantity $ p ^. #metrics . #controlledStake
        let contributedStake = faucetUtxoAmt - fee
        eventually "Controlled stake increases for the stake pool" $ do
            v <- request @[ApiStakePool] ctx Link.listJormungandrStakePools Default Empty
            verify v
                [ expectListField 0 (#metrics . #controlledStake)
                    (.> Quantity (existingPoolStake + contributedStake))
                    -- No exact equality since the delegation from previous
                    -- tests may take effect.
                ]

    it "STAKE_POOLS_JOIN_04 - Rewards accumulate and stop" $ \(_,_,ctx) -> runResourceT @IO $ do

        liftIO $ pendingWith "https://github.com/input-output-hk/cardano-wallet/issues/2140"

        w <- fixtureWallet ctx
        (_, p:_) <- eventually "Stake pools are listed" $
            unsafeRequest @[ApiStakePool] ctx Link.listJormungandrStakePools Empty

        -- Join a pool
        joinStakePool @n ctx (p ^. #id) (w, fixturePassphrase) >>= flip verify
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

        -- Wait for money to flow
        eventually "Wallet gets rewards" $ do
            request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
                [ expectField (#balance . #getApiT . #reward)
                    (.> (Quantity 0))
                ]

        -- Can't quit a pool
        quitStakePool @n ctx (w, fixturePassphrase) >>= flip verify
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403NonNullReward
            ]

    it "STAKE_POOLS_JOIN_04 -\
        \Delegate, stop in the next epoch, and still earn rewards" $ \(_,_,ctx) -> runResourceT @IO $ do

        liftIO $ pendingWith "https://github.com/input-output-hk/cardano-wallet/issues/2140"

        w <- fixtureWallet ctx
        (_, p1:_) <- eventually "Stake pools are listed" $
            unsafeRequest @[ApiStakePool] ctx Link.listJormungandrStakePools Empty

        joinStakePool @n ctx (p1 ^. #id) (w, fixturePassphrase) >>= flip verify
            [ expectResponseCode HTTP.status202
            ]

        waitForNextEpoch ctx

        quitStakePool @n ctx (w, fixturePassphrase) >>= flip verify
            [ expectResponseCode HTTP.status202
            ]

        reward <- eventually "wallet gets rewards" $ do
            r <- request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty
            verify r
                [ expectField
                        (#balance . #getApiT . #reward)
                        (.> Quantity 0)
                ]
            pure $ getFromResponse (#balance . #getApiT . #reward) r

        waitForNextEpoch ctx

        eventually "rewards are the same in the next epoch" $ do
            request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
                [ expectField
                        (#balance . #getApiT . #reward)
                        (`shouldBe` reward)
                ]

    describe "STAKE_POOLS_JOIN_01x - Fee boundary values" $ do
        it "STAKE_POOLS_JOIN_01x - \
            \I can join if I have just the right amount" $ \(_,_,ctx) -> runResourceT @IO $ do
            (_, p:_) <- eventually "Stake pools are listed" $
                unsafeRequest @[ApiStakePool] ctx Link.listJormungandrStakePools Empty
            let (fee, _) = ctx ^. #_feeEstimator $ DelegDescription (Join dummyPool)
            w <- fixtureWalletWith @n ctx [fee+3]
            joinStakePool @n ctx (p ^. #id) (w, fixturePassphrase)>>= flip verify
                [ expectResponseCode HTTP.status202
                , expectField (#status . #getApiT) (`shouldBe` Pending)
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                ]

        it "STAKE_POOLS_JOIN_01x - \
            \I cannot join if I have not enough fee to cover" $ \(_,_,ctx) -> runResourceT @IO $ do
            (_, p:_) <- eventually "Stake pools are listed" $
                unsafeRequest @[ApiStakePool] ctx Link.listJormungandrStakePools Empty
            let (fee, _) = ctx ^. #_feeEstimator $ DelegDescription (Join dummyPool)
            w <- fixtureWalletWith @n ctx [fee - 1]
            r <- joinStakePool @n ctx (p ^. #id) (w, fixturePassphrase)
            expectResponseCode HTTP.status403 r
            expectErrorMessage (errMsg403DelegationFee 1) r

        it "STAKE_POOLS_JOIN_01x - I cannot join stake-pool with 0 balance" $ \(_,_,ctx) -> runResourceT @IO $ do
            (_, p:_) <- eventually "Stake pools are listed" $
                unsafeRequest @[ApiStakePool] ctx Link.listJormungandrStakePools Empty
            w <- emptyWallet ctx
            let (fee, _) = ctx ^. #_feeEstimator $ DelegDescription (Join dummyPool)
            r <- joinStakePool @n ctx (p ^. #id) (w, fixturePassphrase)
            expectResponseCode HTTP.status403 r
            expectErrorMessage (errMsg403DelegationFee fee) r

    describe "STAKE_POOLS_QUIT_01x - Fee boundary values" $ do
        it "STAKE_POOLS_QUIT_01x - \
            \I can quit if I have enough to cover fee" $ \(_,_,ctx) -> runResourceT @IO $ do
            let (_, feeJoin) = ctx ^. #_feeEstimator $ DelegDescription (Join dummyPool)
            let (feeQuit, _) = ctx ^. #_feeEstimator $ DelegDescription Quit
            let initBalance = [feeJoin + feeQuit + 3]
            (w, _) <- joinStakePoolWithWalletBalance @n ctx initBalance
            rq <- quitStakePool @n ctx (w, fixturePassphrase)
            expectResponseCode HTTP.status202 rq
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
            \I cannot quit if I have not enough fee to cover" $ \(_,_,ctx) -> runResourceT @IO $ do
            let (_, feeJoin) = ctx ^. #_feeEstimator $ DelegDescription (Join dummyPool)
            let (feeQuit, _) = ctx ^. #_feeEstimator $ DelegDescription Quit
            let initBalance = [feeJoin+1]
            (w, _) <- joinStakePoolWithWalletBalance @n ctx initBalance
            rq <- quitStakePool @n ctx (w, fixturePassphrase)
            verify rq
                [ expectResponseCode HTTP.status403
                , expectErrorMessage (errMsg403DelegationFee (feeQuit - 1))
                ]

    it "STAKE_POOLS_JOIN_01 - I cannot rejoin the same stake-pool" $ \(_,_,ctx) -> runResourceT @IO $ do
        let (_, feeJoin) = ctx ^. #_feeEstimator $ DelegDescription (Join dummyPool)
        (w, p) <- joinStakePoolWithWalletBalance @n ctx [10*feeJoin]

        -- Join again
        r <- joinStakePool @n ctx (p ^. #id) (w, fixturePassphrase)
        expectResponseCode HTTP.status403 r
        let poolId = toText $ getApiT $ p ^. #id
        expectErrorMessage (errMsg403PoolAlreadyJoined poolId) r

    it "STAKE_POOLS_JOIN_01 - Cannot join non-existent stakepool" $ \(_,_,ctx) -> runResourceT @IO $ do
        let poolIdAbsent = PoolId $ BS.pack $ replicate 32 0
        w <- emptyWallet ctx
        r <- joinStakePool @n ctx (ApiT poolIdAbsent) (w, fixturePassphrase)
        expectResponseCode HTTP.status404 r
        expectErrorMessage (errMsg404NoSuchPool (toText poolIdAbsent)) r

    it "STAKE_POOLS_JOIN_01 - \
        \ If a wallet joins a stake pool, others are not affected" $ \(_,_,ctx) -> runResourceT @IO $ do
        (wA, wB) <- (,) <$> fixtureWallet ctx <*> fixtureWallet ctx
        (_, p:_) <- eventually "Stake pools are listed" $
            unsafeRequest @[ApiStakePool] ctx Link.listJormungandrStakePools Empty

        -- Join a pool
        joinStakePool @n ctx (p ^. #id) (wA, fixturePassphrase) >>= flip verify
            [ expectResponseCode HTTP.status202
            , expectField (#status . #getApiT) (`shouldBe` Pending)
            , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
            ]

        -- Wait for the certificate to be inserted
        eventually "Last outgoing tx is in ledger" $ do
            let ep = Link.listTransactions @'Shelley wA
            request @[ApiTransaction n] ctx ep Default Empty >>= flip verify
                [ expectListField 0
                    (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectListField 0
                    (#status . #getApiT) (`shouldBe` InLedger)
                ]

        -- Verify the wallet is now delegating
        (currentEpochNo, sp) <- getSlotParams ctx
        request @ApiWallet ctx (Link.getWallet @'Shelley wA) Default Empty >>= flip verify
            [ expectField #delegation
                (`shouldBe` notDelegating
                    [ (Just (p ^. #id), mkEpochInfo (currentEpochNo + 2) sp)
                    ]
                )
            ]

        -- Verify the other is NOT delegating
        request @ApiWallet ctx (Link.getWallet @'Shelley wB) Default Empty >>= flip verify
            [ expectField #delegation (`shouldBe` notDelegating [])
            ]

    describe "STAKE_POOLS_JOIN_02 - Passphrase must be correct to join" $ do
        let verifyIt ctx wallet pass expectations = runResourceT @IO $ do
                (_, p:_) <- eventually "Stake pools are listed" $ do
                    unsafeRequest @[ApiStakePool] ctx Link.listJormungandrStakePools Empty
                w <- wallet ctx
                r <- joinStakePool @n ctx (p ^. #id) (w, pass)
                verify r expectations

        it "Wallet with funds - incorrect pass" $ \(_,_,ctx) -> do
            verifyIt ctx fixtureWallet
                    "Incorrect Passphrase"
                    [ expectResponseCode HTTP.status403
                    , expectErrorMessage errMsg403WrongPass
                    ]

        it "Wallet with funds - empty pass" $ \(_,_,ctx) -> do
            verifyIt ctx fixtureWallet
                    ""
                    [ expectResponseCode HTTP.status403
                    , expectErrorMessage errMsg403WrongPass
                    ]

        it "Empty wallet - incorrect pass" $ \(_,_,ctx) -> do
            verifyIt ctx emptyWallet
                    "Incorrect Passphrase"
                    [ expectResponseCode HTTP.status403
                    , expectErrorMessage
                        "I'm unable to select enough coins to pay for\
                        \ a delegation certificate."
                    ]

        it "Empty wallet - empty pass" $ \(_,_,ctx) -> do
            verifyIt ctx emptyWallet
                    ""
                    [ expectResponseCode HTTP.status403
                    , expectErrorMessage
                        "I'm unable to select enough coins to pay for\
                        \ a delegation certificate."
                    ]

    describe "STAKE_POOLS_JOIN/QUIT_02 -\
        \ Passphrase must have appropriate length" $ do

        let pMax = passphraseMaxLength (Proxy @"lenient")
        let tooLongMsg =
                "passphrase is too long: expected at most 255 characters"
        let passTooLong = replicate (pMax + 1) '1'

        let verifyIt ctx doStakePool pass expec = runResourceT @IO $ do
                (_, p:_) <- eventually "Stake pools are listed" $ do
                    unsafeRequest @[ApiStakePool] ctx Link.listJormungandrStakePools Empty
                w <- emptyWallet ctx
                r <- doStakePool ctx (p ^. #id) (w, T.pack pass)
                expectResponseCode HTTP.status400 r
                expectErrorMessage expec r

        it ("Join: " ++ tooLongMsg) $ \(_,_,ctx) -> do
            verifyIt ctx (joinStakePool @n) passTooLong tooLongMsg

        it ("Quit: " ++ tooLongMsg) $ \(_,_,ctx) -> do
            verifyIt ctx (\_ _ -> quitStakePool @n ctx) passTooLong tooLongMsg

    describe "STAKE_POOLS_JOIN/QUIT_02 - Passphrase must be text" $ do
        let verifyIt ctx sPoolEndp = runResourceT @IO $ do
                (_, p:_) <- eventually "Stake pools are listed" $
                    unsafeRequest @[ApiStakePool] ctx Link.listJormungandrStakePools Empty
                w <- emptyWallet ctx
                let payload = Json [json| { "passphrase": 123 } |]
                r <- request @(ApiTransaction n) ctx (sPoolEndp p w)
                        Default payload
                expectResponseCode HTTP.status400 r
                expectErrorMessage "parsing Text failed" r
        it "Join" $ \(_,_,ctx) -> do
            verifyIt ctx Link.joinStakePool
        it "Quit" $ \(_,_,ctx) -> do
            verifyIt ctx (const Link.quitStakePool)

    it "STAKE_POOLS_JOIN_03 - Byron wallet cannot join stake pool" $ \(_,_,ctx) -> runResourceT @IO $ do
        (_, p:_) <- eventually "Stake pools are listed" $
            unsafeRequest @[ApiStakePool] ctx Link.listJormungandrStakePools Empty
        w <- emptyRandomWallet ctx
        r <- joinStakePool @n ctx (p ^. #id) (w, "Secure Passprase")
        expectResponseCode HTTP.status404 r

    -- NOTE
    -- This is only true because:
    --
    -- 1/ We are in Jörmungandr scenario were fees can be known exactly
    -- 2/ Fixture wallets are made of homogeneous UTxOs (all equal to the same
    -- value) and therefore, the random selection has no influence.
    it "STAKE_POOLS_ESTIMATE_FEE_01 - fee matches eventual cost" $ \(_,_,ctx) -> runResourceT @IO $ do
        (_, p:_) <- eventually "Stake pools are listed" $
            unsafeRequest @[ApiStakePool] ctx Link.listJormungandrStakePools Empty
        w <- fixtureWallet ctx
        r <- delegationFee ctx w
        verify r
            [ expectResponseCode HTTP.status200
            ]
        let fee = getFromResponse #estimatedMin r
        joinStakePool @n ctx (p ^. #id) (w, fixturePassphrase) >>= flip verify
            [ expectField #amount (`shouldBe` fee)
            ]

    it "STAKE_POOLS_ESTIMATE_FEE_01x - edge-case fee in-between coeff" $ \(_,_,ctx) -> runResourceT @IO $ do
        liftIO $ pendingWith
            "This is currently testing two different things. On one hand \
            \the fee estimator from the integration tests, and on the other \
            \hand, the fee estimation from the API. These are not quite aligned \
            \and are actually returning different results, which makes this kind \
            \of tests hard to write."
        let (feeMin, _) = ctx ^. #_feeEstimator $ DelegDescription (Join dummyPool)
        w <- fixtureWalletWith @n ctx [feeMin + 1, feeMin + 1]
        r <- delegationFee ctx w
        let (fee, _) = ctx ^. #_feeEstimator $ DelegDescription (Join dummyPool)
        verify r
            [ expectResponseCode HTTP.status200
            , expectField #estimatedMin (`shouldBe` Quantity fee)
            ]

    it "STAKE_POOLS_ESTIMATE_FEE_02 - \
        \empty wallet cannot estimate fee" $ \(_,_,ctx) -> runResourceT @IO $ do
        w <- emptyWallet ctx
        let (fee, _) = ctx ^. #_feeEstimator $ DelegDescription (Join dummyPool)
        delegationFee ctx w >>= flip verify
            [ expectResponseCode HTTP.status403
            , expectErrorMessage $ errMsg403DelegationFee fee
            ]

    it "STAKE_POOLS_ESTIMATE_FEE_03 - can't use byron wallets" $ \(_,_,ctx) -> runResourceT @IO $ do
        w <- fixtureRandomWallet ctx
        let ep = Link.getDelegationFee w
        r <- request @(ApiTransaction n) ctx ep Default Empty
        verify r
            [ expectResponseCode HTTP.status404 -- should fail
            , expectErrorMessage $ errMsg404NoWallet (w ^. walletId)
            ]

    describe "STAKE_POOLS_JOIN/QUIT_05 - Bad request" $ do
        let verifyIt ctx sPoolEndp = runResourceT @IO $ do
                w <- emptyWallet ctx
                let payload = NonJson  "{ passphrase: Secure Passphrase }"
                r <- request @(ApiTransaction n) ctx
                    (sPoolEndp (Identity arbitraryPoolId) w) Default payload
                expectResponseCode HTTP.status400 r

        it "Join" $ \(_,_,ctx) -> do
            verifyIt ctx Link.joinStakePool
        it "Quit" $ \(_,_,ctx) -> do
            verifyIt ctx (const Link.quitStakePool)

    it "STAKE_POOLS_QUIT_01 - Quiting before even joining" $ \(_,_,ctx) -> runResourceT @IO $ do
        w <- emptyWallet ctx

        r <- quitStakePool @n ctx (w, "Secure Passprase")
        expectResponseCode HTTP.status403 r
        expectErrorMessage errMsg403NotDelegating r

    it "STAKE_POOLS_QUIT_02 - Passphrase must be correct to quit" $ \(_,_,ctx) -> runResourceT @IO $ do
        (w, _) <- joinStakePoolWithFixtureWallet @n ctx

        r <- quitStakePool @n ctx (w, "Incorrect Passphrase")
        expectResponseCode HTTP.status403 r
        expectErrorMessage errMsg403WrongPass r

    it "STAKE_POOL_NEXT_01 - Can join/re-join another but cannot quit stake pool" $ \(_,_,ctx) -> runResourceT @IO $ do

        liftIO $ pendingWith "https://github.com/input-output-hk/cardano-wallet/issues/2140"

        (_, p1:p2:_) <- eventually "Stake pools are listed" $
            unsafeRequest @[ApiStakePool] ctx Link.listJormungandrStakePools Empty
        w <- fixtureWallet ctx

        request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
            [ expectField #delegation (`shouldBe` notDelegating [])
            ]

        -- make sure we are at the beginning of new epoch
        (currentEpoch, sp) <- getSlotParams ctx
        waitForNextEpoch ctx

        -- joining first pool
        r1 <- joinStakePool @n ctx (p1 ^. #id) (w, fixturePassphrase)
        expectResponseCode HTTP.status202 r1
        waitAllTxsInLedger @n ctx w
        request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
            [ expectField #delegation
                (`shouldBe` notDelegating
                    [ (Just (p1 ^. #id), mkEpochInfo (currentEpoch + 3) sp)
                    ]
                )
            ]
        eventually "Wallet is delegating to p1" $ do
            request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
                [ expectField #delegation (`shouldBe` delegating (p1 ^. #id) [])
                ]

        -- rejoining second pool
        r2 <- joinStakePool @n ctx (p2 ^. #id) (w, fixturePassphrase)
        expectResponseCode HTTP.status202 r2
        waitAllTxsInLedger @n ctx w
        request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
            [ expectField #delegation
                (`shouldBe` delegating (p1 ^. #id)
                    [ (Just (p2 ^. #id), mkEpochInfo (currentEpoch + 5) sp)
                    ]
                )
            ]
        eventually "Wallet is delegating to p2" $ do
            request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
                [ expectField #delegation (`shouldBe` delegating (p2 ^. #id) [])
                ]

        -- Can't quit a pool because rewards were earned.
        -- (Jörmungandr also does not allow spending rewards at this moment.
        --  Transactions with withdrawRewards=true are rejected by the node
        --  and infinitely 'pending' in the wallet)
        quitStakePool @n ctx (w, fixturePassphrase) >>= flip verify
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403NonNullReward
            ]


    it "STAKE_POOL_NEXT_02/STAKE_POOLS_QUIT_01 - Cannot quit when active: not_delegating"
        $ \(_,_,ctx) -> runResourceT @IO $ do
        w <- emptyWallet ctx
        r <- quitStakePool @n ctx (w, "Secure Passprase")
        expectResponseCode HTTP.status403 r
        expectErrorMessage errMsg403NotDelegating r

    it "STAKE_POOL_NEXT_02 - Override join with join in the same epoch =>\
        \ delegating to the last one in the end" $ \(_,_,ctx) -> runResourceT @IO $ do
        (_, p1:p2:_) <- eventually "Stake pools are listed" $
            unsafeRequest @[ApiStakePool] ctx Link.listJormungandrStakePools Empty
        w <- fixtureWallet ctx

        request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty
            >>= flip verify
                [ expectField #delegation (`shouldBe` notDelegating []) ]

        -- make sure we are at the beginning of new epoch
        (currentEpoch, sp) <- getSlotParams ctx
        waitForNextEpoch ctx

        let joinPool pool = do
                r1 <- joinStakePool @n ctx (pool ^. #id) (w, fixturePassphrase)
                expectResponseCode HTTP.status202 r1
                waitAllTxsInLedger @n ctx w
                request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
                    [ expectField #delegation
                        (`shouldBe` notDelegating
                            [ (Just (pool ^. #id), mkEpochInfo (currentEpoch + 3) sp)
                            ]
                        )
                    ]

        -- join p1 and p2 in the same epoch and make sure delegation next is
        -- always to the last one joined
        joinPool p1
        joinPool p2

    it "STAKE_POOL_NEXT_03 - Join 2 in two subsequent epochs => delegating to 1st in epoch X + 2\
        \ and 2nd in epoch X + 3"
        $ \(_,_,ctx) -> runResourceT @IO $ do
        (_, p1:p2:_) <- liftIO $ eventually "Stake pools are listed" $
            unsafeRequest @[ApiStakePool] ctx Link.listJormungandrStakePools Empty
        w <- fixtureWallet ctx

        request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
            [ expectField #delegation (`shouldBe` notDelegating []) ]

        -- make sure we are at the beginning of new epoch
        (currentEpoch, sp) <- getSlotParams ctx
        waitForNextEpoch ctx

        r1 <- joinStakePool @n ctx (p1 ^. #id) (w, fixturePassphrase)
        expectResponseCode HTTP.status202 r1
        waitAllTxsInLedger @n ctx w
        request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
            [ expectField #delegation
                (`shouldBe` notDelegating
                    [ (Just (p1 ^. #id), mkEpochInfo (currentEpoch + 3) sp)
                    ]
                )
            ]

        -- coming to the next epoch and should be not delegating with two nexts
        -- after another joining
        waitForNextEpoch ctx

        r2 <- joinStakePool @n ctx (p2 ^. #id) (w, fixturePassphrase)
        expectResponseCode HTTP.status202 r2
        waitAllTxsInLedger @n ctx w
        request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
            [ expectField #delegation
                (`shouldBe` notDelegating
                    [ (Just (p1 ^. #id), mkEpochInfo (currentEpoch + 3) sp)
                    , (Just (p2 ^. #id), mkEpochInfo (currentEpoch + 4) sp)
                    ]
                )
            ]

        -- coming to the next epoch:
        --  active: delegating to p1, next: delegating to p2
        eventually "Delegating to p1 and about to delegate to p2" $ do
            request @ApiWallet ctx (Link.getWallet @'Shelley w) Default Empty >>= flip verify
                [ expectField (#tip . #slotId . #epochNumber . #getApiT)
                    (`shouldBe` currentEpoch + 3)
                , expectField #delegation
                    (`shouldBe` delegating (p1 ^. #id)
                        [ (Just (p2 ^. #id), mkEpochInfo (currentEpoch + 4) sp) ]
                    )
                ]

-- | An arbitrary but valid pool id, to avoid having to list pools for testing
-- bad requests and headers.
arbitraryPoolId :: ApiT PoolId
arbitraryPoolId = either (error . show) ApiT $ fromText
    "a659052d84ddb6a04189bee523d59c0a3385c921f43db5dc5de17a4f3f11dc4c"

dummyPool :: PoolId
dummyPool = PoolId mempty

joinStakePoolWithWalletBalance
    :: forall n t.
        ( DecodeAddress n
        , DecodeStakeAddress n
        , EncodeAddress n
        )
    => (Context t)
    -> [Natural]
    -> ResourceT IO (ApiWallet, ApiStakePool)
joinStakePoolWithWalletBalance ctx balance = do
    w <- fixtureWalletWith @n ctx balance
    (_, p:_) <- eventually "Stake pools are listed in joinStakePoolWithWalletBalance" $
        unsafeRequest @[ApiStakePool] ctx Link.listJormungandrStakePools Empty
    r <- joinStakePool @n ctx (p ^. #id) (w, fixturePassphrase)
    expectResponseCode HTTP.status202 r
    -- Verify the certificate was discovered
    eventually "Tx in ledger in joinStakePoolWithWalletBalance" $ do
        let ep = Link.listTransactions @'Shelley w
        request @[ApiTransaction n] ctx ep Default Empty >>= flip verify
            [ expectListField 0 (#direction . #getApiT) (`shouldBe` Outgoing)
            , expectListField 0 (#status . #getApiT) (`shouldBe` InLedger)
            ]
    return (w, p)

joinStakePoolWithFixtureWallet
    :: forall n t.
        ( DecodeAddress n
        , DecodeStakeAddress n
        )
    => (Context t)
    -> ResourceT IO (ApiWallet, ApiStakePool)
joinStakePoolWithFixtureWallet ctx = do
    w <- fixtureWallet ctx
    (_, p:_) <- eventually "Stake pools are listed in joinStakePoolWithFixtureWallet" $
        unsafeRequest @[ApiStakePool] ctx Link.listJormungandrStakePools Empty
    r <- joinStakePool @n ctx (p ^. #id) (w, fixturePassphrase)
    expectResponseCode HTTP.status202 r
    -- Verify the certificate was discovered
    liftIO $ eventually "Tx in ledger in joinStakePoolWithFixtureWallet" $ do
        let ep = Link.listTransactions @'Shelley w
        request @[ApiTransaction n] ctx ep Default Empty >>= flip verify
            [ expectListField 0 (#direction . #getApiT) (`shouldBe` Outgoing)
            , expectListField 0 (#status . #getApiT) (`shouldBe` InLedger)
            ]
    return (w, p)
