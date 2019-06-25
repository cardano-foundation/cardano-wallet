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
    ( ApiTransaction )
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
    , fixtureWallet
    , json
    , listAddresses
    , postTxEp
    , request
    , verify
    )
import Test.Integration.Framework.TestData
    ( errMsg403InvalidTransaction )

import qualified Network.HTTP.Types.Status as HTTP

spec :: forall t. (EncodeAddress t, DecodeAddress t) => SpecWith (Context t)
spec = do
    it "TRANS_CREATE_09 - 0 amount transaction is forbidden on single output tx" $ \ctx -> do
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
        r <- request @(ApiTransaction t) ctx (postTxEp wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403InvalidTransaction
            ]

    it "TRANS_CREATE_09 - 0 amount transaction is forbidden on multi-output tx" $ \ctx -> do
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

        r <- request @(ApiTransaction t) ctx (postTxEp wSrc) Default payload
        verify r
            [ expectResponseCode HTTP.status403
            , expectErrorMessage errMsg403InvalidTransaction
            ]
