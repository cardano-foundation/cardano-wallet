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
    ( ApiFee, ApiTransaction (..) )
import Cardano.Wallet.Primitive.Types
    ( DecodeAddress (..), EncodeAddress (..) )
import Control.Monad
    ( forM_ )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( SpecWith, describe, it, shouldBe, shouldSatisfy )
import Test.Integration.Framework.DSL
    ( Context
    , Headers (..)
    , Payload (..)
    , emptyWallet
    , expectErrorMessage
    , expectResponseCode
    , fixtureWallet
    , fixtureWalletWith
    , for
    , json
    , listAddresses
    , listAllTransactions
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

    it "TRANS_ESTIMATE_09 - \
        \a fee can be estimated for a tx with an output of amount 0 (single)" $ \ctx -> do
        (wSrc, payload) <- fixtureZeroAmtSingle ctx
        r <- request @ApiFee ctx (postTxFeeEp wSrc) Default payload
        expectResponseCode HTTP.status202 r

    it "TRANS_ESTIMATE_09 - \
        \a fee can be estimated for a tx with an output of amount 0 (multi)" $ \ctx -> do
        (wSrc, payload) <- fixtureZeroAmtMulti ctx
        r <- request @ApiFee ctx (postTxFeeEp wSrc) Default payload
        expectResponseCode HTTP.status202 r

    it "TRANS_LIST_?? - List transactions of a fixture wallet" $ \ctx -> do
        txs <- fixtureWallet ctx >>= listAllTransactions ctx
        length txs `shouldBe` 10
        txs `shouldSatisfy` all (null . inputs)

    describe "TRANS_CREATE_10, TRANS_ESTIMATE_10 - \
        \Cannot post tx/fee when max tx size reached" $ do
        let matrix =
                [ ( "single output"
                  , (1, 76_000_001 :: Natural)
                  , 76
                  )
                , ( "multi output"
                  , (77, 1)
                  , 47
                  )
                ]
        forM_ matrix $ \(title, (nInputs, amt), errInputs) -> it title $ \ctx -> do
            wSrc <- fixtureWalletWith ctx (replicate 77 1_000_000)
            wDest <- emptyWallet ctx
            address <- (view #id . head) <$> listAddresses ctx wDest
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
