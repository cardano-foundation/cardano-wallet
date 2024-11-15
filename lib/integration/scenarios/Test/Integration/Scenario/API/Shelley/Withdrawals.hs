{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Integration.Scenario.API.Shelley.Withdrawals (spec) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiConstructTransaction (..)
    , ApiDecodedTransaction
    , ApiSerialisedTransaction (..)
    , ApiWallet
    , ResourceContext (..)
    , WalletStyle (..)
    )
import Cardano.Wallet.Api.Types.Amount
    ( ApiAmount (ApiAmount)
    )
import Cardano.Wallet.Primitive.NetworkId
    ( HasSNetworkId
    )
import Control.Monad.Trans.Resource
    ( runResourceT
    )
import Data.Aeson
    ( toJSON
    )
import Data.Generics.Internal.VL.Lens
    ( view
    , (^.)
    )
import Test.Hspec
    ( SpecWith
    , describe
    )
import Test.Hspec.Expectations.Lifted
    ( shouldBe
    , shouldSatisfy
    )
import Test.Hspec.Extra
    ( it
    )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , eventually
    , expectField
    , expectResponseCode
    , expectSuccess
    , fixtureWallet
    , getFromResponse
    , json
    , request
    , rewardWallet
    , signTx
    , submitTxWithWid
    , verify
    )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Network.HTTP.Types.Status as HTTP

spec :: forall n. HasSNetworkId n => SpecWith Context
spec = describe "WITHDRAWALS" $ do
    it "TRANS_NEW_CREATE_03a - Withdrawal from self, 0 rewards" $ \ctx -> runResourceT $ do
        wa <- fixtureWallet ctx
        let initialBalance = wa ^. #balance . #available . #toNatural
        let withdrawal = Json [json|{ "withdrawal": "self" }|]

        rTx <-
            request @(ApiConstructTransaction n)
                ctx
                (Link.createUnsignedTransaction @'Shelley wa)
                Default
                withdrawal
        verify
            rTx
            [ expectResponseCode HTTP.status202
            , expectField (#coinSelection . #metadata) (`shouldBe` Nothing)
            , expectField (#coinSelection . #withdrawals) (`shouldSatisfy` null)
            ]
        let expectedFee = getFromResponse (#fee . #toNatural) rTx

        let ApiSerialisedTransaction apiTx _ = getFromResponse #transaction rTx

        signedTx <- signTx ctx wa apiTx [expectResponseCode HTTP.status202]

        let decodePayload = Json (toJSON signedTx)
        rDecodedTx <-
            request @(ApiDecodedTransaction n)
                ctx
                (Link.decodeTransaction @'Shelley wa)
                Default
                decodePayload
        verify
            rDecodedTx
            [ expectResponseCode HTTP.status202
            , expectField #withdrawals (`shouldBe` [])
            ]

        -- Submit tx
        submittedTx <- submitTxWithWid ctx wa signedTx
        verify
            submittedTx
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]

        -- Make sure wallet balance is decreased by fee, since rewards = 0
        eventually "Wallet balance is decreased by fee" $ do
            rWa <-
                request @ApiWallet
                    ctx
                    (Link.getWallet @'Shelley wa)
                    Default
                    Empty
            verify
                rWa
                [ expectSuccess
                , expectField
                    (#balance . #available . #toNatural)
                    (`shouldBe` initialBalance - fromIntegral expectedFee)
                , expectField
                    (#balance . #reward . #toNatural)
                    (`shouldBe` 0)
                ]

    it "TRANS_NEW_CREATE_03a - Withdrawal from self" $ \ctx -> runResourceT $ do
        (wa, _) <- rewardWallet ctx

        let withdrawal = Json [json|{ "withdrawal": "self" }|]
        let rewardInitialBalance = wa ^. (#balance . #available . #toNatural)

        rTx <-
            request @(ApiConstructTransaction n)
                ctx
                (Link.createUnsignedTransaction @'Shelley wa)
                Default
                withdrawal
        verify
            rTx
            [ expectResponseCode HTTP.status202
            , expectField (#coinSelection . #metadata) (`shouldBe` Nothing)
            , expectField (#fee . #toNatural) (`shouldSatisfy` (> 0))
            , expectField
                (#coinSelection . #withdrawals)
                (`shouldSatisfy` (not . null))
            ]

        let expectedFee = getFromResponse (#fee . #toNatural) rTx
        let ApiSerialisedTransaction apiTx _ = getFromResponse #transaction rTx

        signedTx <- signTx ctx wa apiTx [expectResponseCode HTTP.status202]

        let decodePayload = Json (toJSON signedTx)
        let withdrawalWith ownership wdrls = case wdrls of
                [wdrl] ->
                    wdrl ^. #amount > ApiAmount 0
                        && wdrl ^. #context == ownership
                _ -> False

        rDecodedTx <-
            request @(ApiDecodedTransaction n)
                ctx
                (Link.decodeTransaction @'Shelley wa)
                Default
                decodePayload
        verify
            rDecodedTx
            [ expectResponseCode HTTP.status202
            , expectField #withdrawals (`shouldSatisfy` (withdrawalWith Our))
            ]

        -- Submit tx
        submittedTx <- submitTxWithWid ctx wa signedTx
        verify
            submittedTx
            [ expectSuccess
            , expectResponseCode HTTP.status202
            ]

        -- Make sure wallet balance is increased by withdrawalAmt - fee
        let withdrawalAmt =
                view (#amount . #toNatural)
                    $ head
                    $ getFromResponse (#withdrawals) rDecodedTx
        eventually "Wallet balance is increased by withdrawalAmt - fee" $ do
            rWa <-
                request @ApiWallet
                    ctx
                    (Link.getWallet @'Shelley wa)
                    Default
                    Empty
            verify
                rWa
                [ expectSuccess
                , expectField
                    (#balance . #available . #toNatural)
                    ( `shouldBe`
                        rewardInitialBalance + (withdrawalAmt - fromIntegral expectedFee)
                    )
                ]

        eventually "Reward balance is 0" $ do
            rWa <-
                request @ApiWallet
                    ctx
                    (Link.getWallet @'Shelley wa)
                    Default
                    Empty
            verify
                rWa
                [ expectSuccess
                , expectField
                    (#balance . #reward . #toNatural)
                    (`shouldBe` 0)
                ]

        wb <- fixtureWallet ctx
        rDecodedTx' <-
            request @(ApiDecodedTransaction n)
                ctx
                (Link.decodeTransaction @'Shelley wb)
                Default
                decodePayload
        verify
            rDecodedTx'
            [ expectResponseCode HTTP.status202
            , expectField #withdrawals (`shouldSatisfy` (withdrawalWith External))
            ]
