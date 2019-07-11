{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.Jormungandr.Scenario.API.Transactions
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiFee, ApiTransaction )
import Cardano.Wallet.Primitive.Types
    ( DecodeAddress (..), EncodeAddress (..) )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Test.Hspec
    ( SpecWith, it )
import Test.Integration.Framework.DSL
    ( Context
    , Headers (..)
    , Payload (..)
    , emptyWallet
    , expectErrorMessage
    , expectResponseCode
    , fixtureNInputs
    , fixtureMaxTxSize
    , fixtureWallet
    , json
    , listAddresses
    , postTxEp
    , postTxFeeEp
    , request
    , verify
    )
import Test.Integration.Framework.TestData
    ( errMsg403TxTooBig )

import qualified Network.HTTP.Types.Status as HTTP

spec :: forall t. (EncodeAddress t, DecodeAddress t) => SpecWith (Context t)
spec = do
    it "TRANS_CREATE_09 - 0 amount transaction is accepted on single output tx" $ \ctx -> do
        (wSrc, payload) <- fixtureZeroAmtSingle ctx
        r <- request @(ApiTransaction t) ctx (postTxEp wSrc) Default payload
        expectResponseCode HTTP.status202 r

    it "TRANS_CREATE_09 - 0 amount transaction is accepted on multi-output tx" $ \ctx -> do
        (wSrc, payload) <- fixtureZeroAmtMulti ctx
        r <- request @(ApiTransaction t) ctx (postTxEp wSrc) Default payload
        expectResponseCode HTTP.status202 r

    it "TRANS_ESTIMATE_09 - 0 tx fee estimation is accepted on single output tx" $ \ctx -> do
        (wSrc, payload) <- fixtureZeroAmtSingle ctx
        r <- request @ApiFee ctx (postTxFeeEp wSrc) Default payload
        expectResponseCode HTTP.status202 r

    it "TRANS_ESTIMATE_09 - 0 amount tx fee estimation is accepted on multi-output tx" $ \ctx -> do
        (wSrc, payload) <- fixtureZeroAmtMulti ctx
        r <- request @ApiFee ctx (postTxFeeEp wSrc) Default payload
        expectResponseCode HTTP.status202 r

    it "TRANS_CREATE_10, TRANS_ESTIMATE_10 - 256 input/output tx/fee" $ \ctx -> do
        (wSrc, _, payload) <- fixtureNInputs ctx (256, 1_000_000)
        fee <- request @ApiFee ctx (postTxFeeEp wSrc) Default payload
        tx <- request @(ApiTransaction t) ctx (postTxEp wSrc) Default payload
        verify fee
            [ expectResponseCode HTTP.status403
            , expectErrorMessage (errMsg403TxTooBig 10)
            ]
        verify tx
            [ expectResponseCode HTTP.status403
            , expectErrorMessage (errMsg403TxTooBig 10)
            ]

    it "TRANS_CREATE_10, TRANS_ESTIMATE_10 - Cannot post tx when max tx size reached" $ \ctx -> do
        (wSrc, _, payload) <- fixtureMaxTxSize ctx (46, 1_000_000) 45_000_000
        tx <- request @(ApiTransaction t) ctx (postTxEp wSrc) Default payload
        fee <- request @ApiFee ctx (postTxFeeEp wSrc) Default payload
        verify tx
            [ expectResponseCode HTTP.status403
            , expectErrorMessage (errMsg403TxTooBig 10)
            ]
        verify fee
            [ expectResponseCode HTTP.status403
            , expectErrorMessage (errMsg403TxTooBig 10)
            ]

  where
    fixtureZeroAmtSingle ctx = do
        wSrc <- fixtureWallet ctx
        wDest <- emptyWallet ctx
        addr:_ <- listAddresses ctx wDest

        let destination = addr ^. #id
        let payload = Json [json|{
                "payments": [{
                    "address": #{destination},
                    "amount": {
                        "quantity": 0,
                        "unit": "lovelace"
                    }
                }],
                "passphrase": "cardano-wallet"
            }|]
        return (wSrc, payload)

    fixtureZeroAmtMulti ctx = do
        wSrc <- fixtureWallet ctx
        wDest <- emptyWallet ctx
        addrs <- listAddresses ctx wDest

        let destination1 = (addrs !! 1) ^. #id
        let destination2 = (addrs !! 2) ^. #id
        let payload = Json [json|{
                "payments": [{
                    "address": #{destination1},
                    "amount": {
                        "quantity": 0,
                        "unit": "lovelace"
                    }
                },
                {
                    "address": #{destination2},
                    "amount": {
                        "quantity": 23,
                        "unit": "lovelace"
                    }
                }],
                "passphrase": "cardano-wallet"
            }|]
        return (wSrc, payload)
