{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{- HLINT ignore "Use head" -}
{- HLINT ignore "Use :" -}

module Test.Integration.Scenario.Preprod where

import Prelude

import qualified Cardano.Wallet.Api.Link as Link
import qualified Network.HTTP.Types.Status as HTTP

import Cardano.Wallet.Api.Types
    ( ApiT (..)
    , ApiTransaction
    , ApiTxId (..)
    , ApiTxInput (source)
    , ApiWallet
    , WalletStyle (..)
    )
import Cardano.Wallet.Api.Types.Amount
    ( ApiAmount (..)
    )
import Cardano.Wallet.Pools
    ( StakePool
    )
import Cardano.Wallet.Primitive.NetworkId
    ( HasSNetworkId (..)
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..)
    )
import Cardano.Wallet.Primitive.Types.Tx.TxMeta
    ( Direction (..)
    , TxStatus (..)
    )
import Control.Monad.IO.Unlift
    ( MonadUnliftIO (..)
    )
import Data.Generics.Internal.VL.Lens
    ( view
    , (^.)
    )
import Data.List
    ( sortOn
    )
import Data.Maybe
    ( isJust
    )
import Data.Ord
    ( Down (..)
    )
import Data.Text
    ( Text
    )
import Numeric.Natural
    ( Natural
    )
import Test.Hspec
    ( SpecWith
    , describe
    , expectationFailure
    , it
    , pendingWith
    , shouldBe
    , shouldSatisfy
    )
import Test.Integration.Framework.DSL
    ( Context
    , Headers (..)
    , Payload (..)
    , between
    , counterexample
    , eventuallyUsingDelay
    , expectField
    , expectResponseCode
    , expectSuccess
    , fixturePassphrase
    , fixturePreprodWallets
    , getFromResponse
    , getResponse
    , json
    , listAddresses
    , request
    , verify
    , (.<)
    )

spec :: forall n . HasSNetworkId n => SpecWith Context
spec = do
    describe "transactions" $ do
        it "simple transaction (TRANS_CREATE_01x)" $ \ctx -> do
            -- Note on 'richestWalletFirst':
            --
            -- By always sending money from the richer wallet to the poorer wallet,
            -- the setup should last longer without needing manual intervention.
            [wa, wb] <- richestWalletFirst <$> fixturePreprodWallets ctx
            let amt = 1_000_000
            let expectedMinFee = 150_000
            let expectedMaxFee = 200_000
            let ApiAmount initialBalanceA = wa ^. #balance . #available
            let ApiAmount initialBalanceB = wb ^. #balance . #available

            payload <- mkTxPayload @IO ctx wb amt fixturePassphrase

            rTx <-
                request @(ApiTransaction n)
                    ctx
                    (Link.createTransactionOld @'Shelley wa)
                    Default
                    payload
            ra <-
                request @ApiWallet
                    ctx
                    (Link.getWallet @'Shelley wa)
                    Default
                    Empty

            verify
                rTx
                [ expectSuccess
                , expectResponseCode HTTP.status202
                , expectField (#fee . #toNatural)
                    $ between (expectedMinFee, expectedMaxFee)
                , expectField (#amount . #toNatural)
                    $ between (expectedMinFee + amt, expectedMaxFee + amt)
                , expectField #inputs $ \inputs' -> do
                    inputs' `shouldSatisfy` all (isJust . source)
                , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                , expectField (#status . #getApiT) (`shouldBe` Pending)
                , expectField #metadata (`shouldBe` Nothing)
                ]

            verify
                ra
                [ expectSuccess
                , expectField (#balance . #total)
                    $ between
                        ( ApiAmount (initialBalanceA - expectedMaxFee - amt)
                        , ApiAmount (initialBalanceA - expectedMinFee - amt)
                        )
                , expectField
                    (#balance . #available . #toNatural)
                    (.< initialBalanceA)
                ]

            let txid = getFromResponse #id rTx
            let linkSrc = Link.getTransaction @'Shelley wa (ApiTxId txid)
            let ApiAmount fee = getFromResponse #fee rTx
            eventually "transaction is no longer pending on source wallet" $ do
                rSrc <- request @(ApiTransaction n) ctx linkSrc Default Empty
                verify
                    rSrc
                    [ expectResponseCode HTTP.status200
                    , expectField (#amount . #toNatural)
                        $ between (expectedMinFee + amt, expectedMaxFee + amt)
                    , expectField #inputs $ \inputs' -> do
                        inputs' `shouldSatisfy` all (isJust . source)
                    , expectField (#direction . #getApiT) (`shouldBe` Outgoing)
                    , expectField (#status . #getApiT) (`shouldBe` InLedger)
                    , expectField (#metadata) (`shouldBe` Nothing)
                    , expectField (#fee . #toNatural)
                        $ between (expectedMinFee, expectedMaxFee)
                    ]

            let linkDest = Link.getTransaction @'Shelley wb (ApiTxId txid)
            eventually "transaction is discovered by destination wallet" $ do
                rDst <- request @(ApiTransaction n) ctx linkDest Default Empty
                verify
                    rDst
                    [ expectField (#amount . #toNatural) (`shouldBe` amt)
                    , expectField (#direction . #getApiT) (`shouldBe` Incoming)
                    ]

            eventually "wa and wb balances are as expected" $ do
                rb <-
                    request @ApiWallet
                        ctx
                        (Link.getWallet @'Shelley wb)
                        Default
                        Empty
                expectField
                    (#balance . #available)
                    (`shouldBe` ApiAmount (initialBalanceB + amt))
                    rb

                ra2 <-
                    request @ApiWallet
                        ctx
                        (Link.getWallet @'Shelley wa)
                        Default
                        Empty
                expectField
                    (#balance . #available)
                    (`shouldBe` ApiAmount (initialBalanceA - fee - amt))
                    ra2

    describe "stake pools" $ do
        describe "list" $ do
            let listPools :: Context -> Maybe Coin -> IO [StakePool]
                listPools ctx stake = do
                    r <- request @[ApiT StakePool] @IO ctx
                        (Link.listStakePools stake) Default Empty
                    expectResponseCode HTTP.status200 r
                    return $ map getApiT $ getResponse r
            let expectOr True _ = pure ()
                expectOr False msg = expectationFailure msg
            let arbitraryStake = Just $ Coin 1_000_000_000
            it "some have non-zero rewards, stake, producedBlocks, saturation" $ \ctx -> do
                pools  <- listPools ctx arbitraryStake
                let rewards
                        = view (#metrics . #nonMyopicMemberRewards . #getQuantity)
                let relativeStake
                        = view (#metrics . #relativeStake . #getQuantity . #toRational)
                let producedBlocks
                        = view (#metrics . #producedBlocks . #getQuantity)
                let saturation
                        = view (#metrics . #saturation)
                counterexample (show pools) $ do
                    any (\p -> rewards p > 0) pools
                        `expectOr` "some pools should have non-zero rewards"
                    any (\p -> relativeStake p > 0) pools
                        `expectOr` "some pools should have non-zero stake"
                    any (\p -> producedBlocks p > 0) pools
                        `expectOr` "some pools should have non-zero producedBlocks"
                    any (\p -> saturation p > 0) pools
                        `expectOr` "some pools should have non-zero saturation"

            it "some have metadata" $ \ctx -> do
                pendingWith "metadata fetching not configured; could be enabled"
                pools  <- listPools ctx arbitraryStake
                any (isJust . view #metadata) pools
                    `expectOr` "some pools should have metadata"
  where
    richestWalletFirst :: [ApiWallet] -> [ApiWallet]
    richestWalletFirst = sortOn (Down . view (#balance . #total . #toNatural))

    mkTxPayload
        :: MonadUnliftIO m
        => Context
        -> ApiWallet
        -> Natural
        -> Text
        -> m Payload
    mkTxPayload ctx wDest amt passphrase = do
        addrs <- listAddresses @n ctx wDest
        let destination = (addrs !! 1) ^. #id
        return
            $ Json
                [json|{
                "payments": [{
                    "address": #{destination},
                    "amount": {
                        "quantity": #{amt},
                        "unit": "lovelace"
                    }
                }],
                "passphrase": #{passphrase}
            }|]

    -- For preprod a slightly longer timeout of 5 min is useful
    eventually = eventuallyUsingDelay
            (10 * s)
            300 -- already in s
      where
        s = 1_000_000
