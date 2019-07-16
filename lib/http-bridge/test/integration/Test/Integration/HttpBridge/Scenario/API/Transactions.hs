{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.HttpBridge.Scenario.API.Transactions
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiFee, ApiTransaction )
import Cardano.Wallet.Primitive.Types
    ( DecodeAddress (..), EncodeAddress (..) )
import Control.Monad
    ( forM_ )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( SpecWith, describe, it )
import Test.Integration.Framework.DSL
    ( Context
    , Headers (..)
    , Payload (..)
    , emptyWallet
    , expectErrorMessage
    , expectResponseCode
    , fixtureWallet
    , fixtureWalletWith
    , json
    , listAddresses
    , postTxEp
    , postTxFeeEp
    , request
    , verify
    )
import Test.Integration.Framework.TestData
    ( errMsg403TxTooBig, errMsg403ZeroAmtOutput )

import qualified Network.HTTP.Types.Status as HTTP

spec :: forall t. (EncodeAddress t, DecodeAddress t) => SpecWith (Context t)
spec = do
    it "TRANS_CREATE_09 - 0 amount transaction is forbidden on single output tx" $ \ctx -> do
        (wSrc, payload) <- fixtureZeroAmtSingle ctx
        r <- request @(ApiTransaction t) ctx (postTxEp wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403ZeroAmtOutput
            ]

    it "TRANS_CREATE_09 - 0 amount transaction is forbidden on multi-output tx" $ \ctx -> do
        (wSrc, payload) <- fixtureZeroAmtMulti ctx
        r <- request @(ApiTransaction t) ctx (postTxEp wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403ZeroAmtOutput
            ]

    it "TRANS_ESTIMATE_09 - \
        \a fee cannot be estimated for a tx with an output of amount 0 (single)" $ \ctx -> do
        (wSrc, payload) <- fixtureZeroAmtSingle ctx
        r <- request @ApiFee ctx (postTxFeeEp wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403ZeroAmtOutput
            ]

    it "TRANS_ESTIMATE_09 - \
        \a fee cannot be estimated for a tx with an output of amount 0 (multi)" $ \ctx -> do
        (wSrc, payload) <- fixtureZeroAmtMulti ctx
        r <- request @ApiFee ctx (postTxFeeEp wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403ZeroAmtOutput
            ]

    describe "TRANS_CREATE_10, TRANS_ESTIMATE_10 - \
        \Cannot post tx/fee when max tx size reached" $ do
        let matrix =
                [ ( "single output"
                  , (1, 46_000_001 :: Natural)
                  , 46
                  )
                , ( "multi output"
                  , (47, 10)
                  , 32
                  )
                ]
        forM_ matrix $ \(title, (nInputs, amt), errInputs) -> it title $ \ctx -> do
            wSrc <- fixtureWalletWith ctx (replicate 47 1_000_000)
            wDest <- emptyWallet ctx
            address <- (view #id . head) <$> listAddresses ctx wDest
            let for = flip map
            let payments = for (replicate nInputs address) $ \addr -> [json|{
                    "address": #{addr},
                    "amount": {
                        "quantity": #{amt},
                        "unit": "lovelace"
                    }
                }|]
            let payload = Json [json|{
                    "payments": #{payments},
                    "passphrase": "Secure Passphrase"
                }|]
            fee <- request @ApiFee ctx (postTxFeeEp wSrc) Default payload
            tx <- request @(ApiTransaction t) ctx (postTxEp wSrc) Default payload
            verify fee
             [ expectResponseCode HTTP.status403
             , expectErrorMessage (errMsg403TxTooBig errInputs)
             ]
            verify tx
             [ expectResponseCode HTTP.status403
             , expectErrorMessage (errMsg403TxTooBig errInputs)
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
